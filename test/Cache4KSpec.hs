{-# LANGUAGE PackageImports #-}

-- | The 4 KiB cache ("Memory.Cache4K"): a few concrete runs, its server proof
-- ("Proof.Cache.Server4K") on random states, and the composition end to end.
module Cache4KSpec
  ( cache4KTests,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import CompositionSpec (coreDiff, ctTrace, genByteProg, genCTProg, genData, genSecretFreeProg, genWord, sameModuloCtrl, simHops, withData)
import Core
import Data.Functor.Identity (Identity (..))
import Core.Instruction
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types
import Proof.Cache.Server4K
import Proof.Functional.Obligation (isaAt)
import Proof.Leakage.Model
import Proof.Machine
import Core.RegFile (RegFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

cache4KTests :: TestTree
cache4KTests =
  testGroup
    "Core behind a 4 KiB cache"
    [ runTests,
      testProperty "read, from the lines of the first and last byte, answers as memory does" $
        withMaxSuccess 2000 $ checkCoverage $
          forAllShow genMem (const "<memory>") $ \mem -> forAll genAddr $ \a -> forAll (elements [Byte, Half, Types.Word]) $ \size ->
            let c = cacheFill (lastByteOf a size) mem (cacheFill a mem emptyCache4K)
             in cover 10 (straddles a size) "straddling" (cacheRead a size c == readResp a size mem),
      serverTests,
      testProperty "composition: the cached core's bus trace follows from the leakage alone" $
        withMaxSuccess 300 $ checkCoverage $
          forAll genCTProg $ \prog ->
            let sys = initSys prog
                r = compositeReport sys
             in cover 30 (stallCycles sys > 0) "the core waits for the cache" (counterexample r (P.null r)),
      testProperty "... also with reads and writes of every width and alignment" $
        withMaxSuccess 300 $ checkCoverage $
          forAll genByteProg $ \prog -> forAll genData $ \ds ->
            let sys = withData ds (initSys prog)
                r = compositeReport sys
             in cover 30 (stallCycles sys > 0) "the core waits for the cache" (counterexample r (P.null r)),
      testProperty "the cached run is the ideal run with stall cycles inserted" $
        withMaxSuccess 300 $
          forAll genCTProg $ \prog ->
            let r = stutterReport 300 (initSys prog)
             in counterexample r (P.null r),
      testProperty "... also with reads and writes of every width and alignment" $
        withMaxSuccess 300 $
          forAll genByteProg $ \prog ->
            forAll genData $ \ds ->
              let r = stutterReport 300 (withData ds (initSys prog))
               in counterexample r (P.null r),
      testProperty "a secret-free program leaks nothing about its data, behind the cache" $
        withMaxSuccess 300 $
          forAll genSecretFreeProg $ \prog ->
            forAll genData $ \da ->
              forAll genData $ \db ->
                let s1 = withData da (initSys prog)
                    s2 = withData db (initSys prog)
                 in ctTrace 100 (isaAt initPc s1) == ctTrace 100 (isaAt initPc s2)
                      P.&& cachedObs 300 s1 == cachedObs 300 s2,
      testProperty "a load through a loaded pointer leaks it, even to a timing attacker" $
        -- The control: @lw x5, 0(x0); lw x1, 0(x5); lw x2, 0(x0); ebreak@. The
        -- second load's address is data. With the pointer at 0 the second load
        -- hits the line the first one filled; with the pointer at 16, in the next
        -- set, it misses. So even an attacker who sees only in which cycles a
        -- request is made tells the two apart.
        let prog =
              map encode $
                unsafeFromList $
                  [ IType (Load Types.Word Signed) 5 0 0,
                    IType (Load Types.Word Signed) 1 5 0,
                    IType (Load Types.Word Signed) 2 0 0
                  ]
                    P.++ P.replicate 47 Core.Instruction.break
            timing p = P.map (/= NoAccess) (cachedObs 100 (withData [(0, p)] (initSys prog)))
         in once (timing 0 /= timing 16)
    ]

-- Concrete runs -------------------------------------------------------------------

memU :: MemFn
memU = MemFn (\a -> slice d7 d0 (pack (a * 13 + 7)))

rdReq :: Address -> Size -> Request
rdReq a sz = Just (MemAccess False a sz Nothing)

wrReq :: Address -> Size -> Word -> Request
wrReq a sz v = Just (MemAccess False a sz (Just (Identity v)))

-- | Serve requests one hop each, from the empty cache in front of 'memU':
-- the stall count and the response word of each.
hops :: [Request] -> [(Cycles, Word)]
hops = go (CacheSrv emptyCache4K Nothing memU)
  where
    go _ [] = []
    go y (q : qs) = let (y', k, x) = hop y q in (k, runIdentity (inputMem x)) : go y' qs

runTests :: TestTree
runTests =
  testGroup
    "concrete runs"
    [ testCase "a read miss stalls, then the rest of its line hits" $
        hops [rdReq 0x100 Types.Word, rdReq 0x10C Types.Word, rdReq 0x106 Half]
          @?= [ (penalty, memReadWord 0x100 memU),
                (0, memReadWord 0x10C memU),
                (0, loadExtend Half Unsigned (memReadWord 0x106 memU))
              ],
      testCase "lines 4 KiB apart evict each other" $
        P.map fst (hops [rdReq 0x100 Types.Word, rdReq 0x1100 Types.Word, rdReq 0x100 Types.Word])
          @?= [penalty, penalty, penalty],
      testCase "a write hit is merged into the cached word" $
        hops [rdReq 0x100 Types.Word, wrReq 0x101 Byte 0xAB, rdReq 0x100 Types.Word]
          @?= [ (penalty, memReadWord 0x100 memU),
                (0, 0),
                (0, memReadWord 0x100 (memWrite 0x101 Byte 0xAB memU))
              ],
      testCase "a write that leaves its word drops its lines" $
        P.map fst (hops [rdReq 0x100 Types.Word, rdReq 0x110 Types.Word, wrReq 0x10E Types.Word 1, rdReq 0x100 Types.Word, rdReq 0x110 Types.Word])
          @?= [penalty, penalty, 0, penalty, penalty]
    ]

-- The server proof on random states -------------------------------------------------

-- | A memory whose bytes are a function of their address, different in every
-- test.
genMem :: Gen MemFn
genMem = do
  k1 <- genU32
  k2 <- genU32
  pure (MemFn (\a -> slice d7 d0 (pack ((a * (k1 .|. 1) + k2) `xor` (a `shiftR` 9)))))
  where
    genU32 = fromIntegral <$> choose (0 :: Integer, 0xFFFFFFFF) :: Gen (Unsigned 32)

-- | Tags of the first three 4 KiB blocks of memory, sometimes others.
genTag :: Gen Tag
genTag = frequency [(10, elements [0, 1, 2]), (1, fromIntegral <$> choose (0 :: Int, 0xFFFFF))]

-- | The cache in front of memory, with no miss pending: lines mostly valid,
-- holding memory's words, and rarely a wrong word, which takes the server out of
-- its invariant at that word.
genSrv :: Gen (CacheSrv MemFn)
genSrv = do
  mem <- genMem
  es <- vectorOf 256 (frequency [(2, pure 0), (8, ((1 :: BitVector 1) ++#) <$> genTag)])
  ws <- P.mapM (genData1 mem es) [0 .. 1023]
  pure (CacheSrv (Cache4K (TagArr (unsafeFromList es)) (DataArr (unsafeFromList ws))) Nothing mem)
  where
    genData1 mem es i =
      let s = fromIntegral (i `P.div` 4) :: SetIdx
          w = fromIntegral (i `P.mod` 4) :: WordIdx
          e = es P.!! (i `P.div` 4)
       in if entryValid e
            then frequency [(60, pure (memReadWord (wordAddr (entryTag e) s w 0) mem)), (1, genWord)]
            else genWord

-- | Addresses in the first three 4 KiB blocks, sometimes anywhere.
genAddr :: Gen Address
genAddr =
  frequency
    [ (10, fromIntegral <$> choose (0 :: Int, 3 * 4096 - 1)),
      (1, fromIntegral <$> choose (0 :: Integer, 0xFFFFFFFF))
    ]

-- | Requests: none, fetches, and reads and writes of every width and alignment,
-- and also requests the core never makes.
genReq :: Gen Request
genReq =
  frequency
    [ (1, pure Nothing),
      (2, (\a -> Just (MemAccess True a Types.Word Nothing)) <$> genAddr),
      (1, (\a sz -> Just (MemAccess True a sz Nothing)) <$> genAddr <*> genSize),
      (1, (\a sz v -> Just (MemAccess True a sz (Just (Identity v)))) <$> genAddr <*> genSize <*> genWord),
      (8, (\a sz -> Just (MemAccess False a sz Nothing)) <$> genAddr <*> genSize),
      (4, (\a sz v -> Just (MemAccess False a sz (Just (Identity v)))) <$> genAddr <*> genSize <*> genWord)
    ]
  where
    genSize = elements [Byte, Half, Types.Word]

-- | Witnesses, half of the time at the request's own set, word and bytes.
genWitness :: Request -> Gen (Address, SetIdx, WordIdx)
genWitness q = do
  let a = maybe 0 memAddress q
  wa <- frequency [(1, (\o -> a + fromIntegral o) <$> choose (0 :: Int, 4)), (1, genAddr)]
  ws <- frequency [(1, pure (setOf a)), (1, fromIntegral <$> choose (0 :: Int, 255))]
  ww <- frequency [(1, pure (wordOf a)), (1, fromIntegral <$> choose (0 :: Int, 3))]
  pure (wa, ws, ww)

-- | The address of the last byte of an access.
lastByteOf :: Address -> Size -> Address
lastByteOf a sz =
  a + case sz of
    Byte -> 0
    Half -> 1
    Types.Word -> 3

-- | Does an access cross a word boundary?
straddles :: Address -> Size -> Bool
straddles a sz = a `shiftR` 2 /= lastByteOf a sz `shiftR` 2

-- | What kind of request, against what cache.
reqKind :: CacheSrv MemFn -> Request -> String
reqKind y q = case q of
  Nothing -> "none"
  Just (MemAccess _ a sz (Just _))
    | straddles a sz -> "straddling write"
    | hit a c -> "write hit"
    | otherwise -> "write miss"
  Just (MemAccess True _ _ Nothing) -> "fetch"
  Just (MemAccess False a sz Nothing)
    | straddles a sz -> "straddling read"
    | hit a c -> "hit"
    | otherwise -> "miss"
  where
    c = srvCache y

coverKinds :: CacheSrv MemFn -> Request -> Property -> Property
coverKinds y q =
  P.foldr
    (\k f -> cover 3 (reqKind y q == k) k . f)
    id
    ["none", "fetch", "write hit", "write miss", "straddling write", "hit", "miss", "straddling read"]

serverTests :: TestTree
serverTests =
  testGroup
    "server proof"
    [ testProperty "the empty cache starts in the invariant" $
        forAllShow genMem (const "<memory>") $ \mem ->
          forAll (fromIntegral <$> choose (0 :: Int, 255)) $ \s ->
            forAll (fromIntegral <$> choose (0 :: Int, 3)) $ \w -> srvInit s w mem,
      testProperty "refinement: the hop of a request ends with ideal memory's response" $
        withMaxSuccess 4000 $ checkCoverage $
          forAllShow genSrv (const "<server>") $ \y -> forAll genReq $ \q -> forAll (genWitness q) $ \(wa, ws, ww) ->
            coverKinds y q $
              cover 80 (srvInvAt ws ww y) "in the invariant at the witness" $
                cover 10 (ws == maybe 0 (setOf . memAddress) q P.&& ww == maybe 0 (wordOf . memAddress) q) "the witness is the accessed word" $
                  srvRefine wa ws ww y q,
      testProperty "leakage: the stalls and the next tags follow from the tags and the observed request" $
        withMaxSuccess 4000 $ checkCoverage $
          forAllShow genSrv (const "<server>") $ \y -> forAll genReq $ \q -> forAll (genWitness q) $ \(_, ws, _) ->
            coverKinds y q (property (srvLeak ws y q)),
      testProperty "the cache answers late only to a data read" $
        withMaxSuccess 2000 $
          forAllShow genSrv (const "<server>") $ \y -> forAll genReq $ \q -> stallsOnReads y q
    ]

-- Composition, end to end -----------------------------------------------------------

-- | The requests the core makes behind the 4 KiB cache, cycle by cycle, until it
-- halts.
cachedObs :: Int -> Sys -> [Obs]
cachedObs n sys = go n (initCacheSys sys emptyCache4K)
  where
    go :: Int -> CacheSys RegFile MemBytes Cache4K -> [Obs]
    go 0 _ = []
    go j cs
      | P.not (running (cacheSysCore cs)) = []
      | otherwise = let (cs', o) = stepCachedOut cs in obsOf o : go (j - 1) cs'

-- | The same trace from the leakage alone: the core's simulator produces each
-- hop's requests as behind ideal memory, and the cache's simulator adds the
-- cycles in which the core waits.
compositeTrace :: Int -> Sys -> [Obs]
compositeTrace n sys0 = go (P.take n (simHops sys0)) tagEmpty
  where
    go [] _ = []
    go (os : rest) t =
      let (t', bus) = serveAll t os
       in bus P.++ go rest t'
    serveAll t [] = (t, [])
    serveAll t (o : os) =
      let (t1, xs) = simBus t o
          (t2, ys) = serveAll t1 os
       in (t2, xs P.++ ys)

compositeReport :: Sys -> String
compositeReport sys0
  | cached == P.take (P.length cached) composite = ""
  | otherwise = "first difference at cycle " P.++ show i P.++ ": " P.++ show (P.take 6 (P.drop i cached)) P.++ " vs " P.++ show (P.take 6 (P.drop i composite))
  where
    cached = cachedObs 600 sys0
    composite = compositeTrace 600 sys0
    i = P.length (P.takeWhile id (P.zipWith (==) cached composite))

-- | The cycles in which the core behind the cache waits, until it halts.
stallCycles :: Sys -> Int
stallCycles sys0 = go (600 :: Int) (initCacheSys sys0 emptyCache4K)
  where
    go 0 _ = 0
    go j cs
      | P.not (running (cacheSysCore cs)) = 0
      | otherwise =
          (if inputMemReady (sysInput (cacheSysCore cs)) then 0 else 1)
            + go (j - 1) (fst (stepCachedOut cs))

-- | Is the cache coherent with memory at every set the harness's memory can
-- fill? Memory is 'MEM_SIZE' words, below address 512, so only sets 0 to 31.
coherentLow :: MemBytes -> Cache4K -> Bool
coherentLow mem c = P.and [coherentAt mem s w c | s <- [0 .. 31], w <- [0 .. 3]]

-- | Walk the cached system and the ideal one together for @n@ cycles of the
-- cached one, skipping the cached run's stall cycles -- those whose bus input is
-- not ready -- in the ideal one. Returns @\"\"@ if every stall cycle makes no
-- request and leaves the core alone, the cache stays coherent with memory, and
-- every other cycle makes the ideal run's request from the ideal run's state, up
-- to 'sameModuloCtrl'.
stutterReport :: Int -> Sys -> String
stutterReport n sys0 = go n (0 :: Int) (initCacheSys sys0 emptyCache4K) sys0
  where
    go :: Int -> Int -> CacheSys RegFile MemBytes Cache4K -> Sys -> String
    go 0 _ _ _ = ""
    go j c cs ideal
      | P.not (running core) = ""
      | P.not (coherentLow (sysMem core) (cacheSysCache cs)) = failAt "the cache is not coherent with memory" ""
      | P.not ready =
          if obsOf oC /= NoAccess
            then failAt "a stall cycle made a request: " (show (obsOf oC))
            else
              -- The bus input is the memory's answer, which may arrive now.
              if P.not (sameModuloCtrl (withInput (cacheSysCore cs')) core)
                then failAt "a stall cycle changed the core: " (P.unwords (coreDiff core (cacheSysCore cs')))
                else go (j - 1) (c + 1) cs' ideal
      | P.not (sameModuloCtrl core ideal) = failAt "the cached core left the ideal run: " (P.unwords (coreDiff ideal core))
      | obsOf oC /= obsOf oI = failAt "different requests: " (show (obsOf oC, obsOf oI))
      | otherwise = go (j - 1) (c + 1) cs' ideal'
      where
        core = cacheSysCore cs
        ready = inputMemReady (sysInput core)
        withInput (Sys st _ m) = Sys st (sysInput core) m
        (cs', oC) = stepCachedOut cs
        (ideal', oI) = stepSysOut ideal
        failAt what detail = "\ncycle " P.++ show c P.++ ": " P.++ what P.++ detail P.++ "\n"
