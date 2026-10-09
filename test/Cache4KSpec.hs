{-# LANGUAGE PackageImports #-}

-- | The 4 KiB cache ("Memory.Cache4K"): a few concrete runs, its server proof
-- ("Proof.Cache.Server4K") on random states, and the composition end to end.
module Cache4KSpec
  ( cache4KTests,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import CompositionSpec (genByteProg, genData, genWord, withData)
import Core
import Data.Functor.Identity (Identity (..))
import Data.Maybe (catMaybes)
import Instruction (Sign (..), loadExtend)
import LeakageSpec (genCTProg)
import Memory.Cache (inBlock)
import Memory.Cache4K
import Memory.Types
import Proof.Cache.Server4K
import Proof.Leakage.Model
import Proof.Leakage.Simulator (archOfLeak, censor, leakSimHop)
import Proof.Machine
import RegFile (RegFile)
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
      serverTests,
      testProperty "composition: the cached core's bus trace follows from the leakage alone" $
        withMaxSuccess 300 $ checkCoverage $
          forAll genCTProg $ \prog ->
            let sys = initSys prog
                r = compositeReportK sys
             in cover 30 (stallCyclesK sys > 0) "the core waits for the cache" (counterexample r (P.null r)),
      testProperty "... also with reads and writes of every width and alignment" $
        withMaxSuccess 300 $ checkCoverage $
          forAll genByteProg $ \prog -> forAll genData $ \ds ->
            let sys = withData ds (initSys prog)
                r = compositeReportK sys
             in cover 30 (stallCyclesK sys > 0) "the core waits for the cache" (counterexample r (P.null r))
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
    go y (q : qs) = let (y', k, x) = hopK y q in (k, runIdentity (inputMem x)) : go y' qs

runTests :: TestTree
runTests =
  testGroup
    "concrete runs"
    [ testCase "a read miss stalls, then the rest of its line hits" $
        hops [rdReq 0x100 Types.Word, rdReq 0x10C Types.Word, rdReq 0x106 Half]
          @?= [ (penaltyK, memReadWord 0x100 memU),
                (0, memReadWord 0x10C memU),
                (0, loadExtend Half Unsigned (memReadWord 0x106 memU))
              ],
      testCase "lines 4 KiB apart evict each other" $
        P.map fst (hops [rdReq 0x100 Types.Word, rdReq 0x1100 Types.Word, rdReq 0x100 Types.Word])
          @?= [penaltyK, penaltyK, penaltyK],
      testCase "a write hit is merged into the cached word" $
        hops [rdReq 0x100 Types.Word, wrReq 0x101 Byte 0xAB, rdReq 0x100 Types.Word]
          @?= [ (penaltyK, memReadWord 0x100 memU),
                (0, 0),
                (0, memReadWord 0x100 (memWriteWord Byte 0x101 0xAB memU))
              ],
      testCase "a write that leaves its word drops its lines" $
        P.map fst (hops [rdReq 0x100 Types.Word, rdReq 0x110 Types.Word, wrReq 0x10E Types.Word 1, rdReq 0x100 Types.Word, rdReq 0x110 Types.Word])
          @?= [penaltyK, penaltyK, 0, penaltyK, penaltyK]
    ]

-- The server proof on random states -------------------------------------------------

-- | A memory whose bytes are a function of their address, different in every
-- test.
genMemK :: Gen MemFn
genMemK = do
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
genSrvK :: Gen (SrvK MemFn)
genSrvK = do
  mem <- genMemK
  es <- vectorOf 256 (frequency [(2, pure 0), (8, ((1 :: BitVector 1) ++#) <$> genTag)])
  ws <- P.mapM (genData1 mem es) [0 .. 1023]
  pure (CacheSrv (Cache4K (TagArr (unsafeFromList es)) (DataArr (unsafeFromList ws))) Nothing mem)
  where
    genData1 mem es i =
      let s = fromIntegral (i `P.div` 4) :: SetIdx
          w = fromIntegral (i `P.mod` 4) :: WordIdx
          e = es P.!! (i `P.div` 4)
       in if entryValid e
            then frequency [(60, pure (memReadWord (wordAddr (entryTag e) s w) mem)), (1, genWord)]
            else genWord

-- | Addresses in the first three 4 KiB blocks, sometimes anywhere.
genAddrK :: Gen Address
genAddrK =
  frequency
    [ (10, fromIntegral <$> choose (0 :: Int, 3 * 4096 - 1)),
      (1, fromIntegral <$> choose (0 :: Integer, 0xFFFFFFFF))
    ]

-- | Requests: none, fetches, and reads and writes of every width and alignment,
-- and also requests the core never makes.
genReqK :: Gen Request
genReqK =
  frequency
    [ (1, pure Nothing),
      (2, (\a -> Just (MemAccess True a Types.Word Nothing)) <$> genAddrK),
      (1, (\a sz -> Just (MemAccess True a sz Nothing)) <$> genAddrK <*> genSize),
      (1, (\a sz v -> Just (MemAccess True a sz (Just (Identity v)))) <$> genAddrK <*> genSize <*> genWord),
      (8, (\a sz -> Just (MemAccess False a sz Nothing)) <$> genAddrK <*> genSize),
      (4, (\a sz v -> Just (MemAccess False a sz (Just (Identity v)))) <$> genAddrK <*> genSize <*> genWord)
    ]
  where
    genSize = elements [Byte, Half, Types.Word]

-- | Witnesses, half of the time at the request's own set, word and bytes.
genWitness :: Request -> Gen (Address, SetIdx, WordIdx)
genWitness q = do
  let a = maybe 0 memAddress q
  wa <- frequency [(1, (\o -> a + fromIntegral o) <$> choose (0 :: Int, 4)), (1, genAddrK)]
  ws <- frequency [(1, pure (setOf a)), (1, fromIntegral <$> choose (0 :: Int, 255))]
  ww <- frequency [(1, pure (wordOf a)), (1, fromIntegral <$> choose (0 :: Int, 3))]
  pure (wa, ws, ww)

-- | What kind of request, against what cache.
reqKindK :: SrvK MemFn -> Request -> String
reqKindK y q = case q of
  Nothing -> "none"
  Just (MemAccess _ a sz (Just _))
    | P.not (inBlock a sz) -> "straddling write"
    | hitK a c -> "write hit"
    | otherwise -> "write miss"
  Just (MemAccess True _ _ Nothing) -> "fetch"
  Just (MemAccess False a sz Nothing)
    | P.not (inBlock a sz) -> "straddling read"
    | hitK a c -> "hit"
    | otherwise -> "miss"
  where
    c = srvCache y

coverKindsK :: SrvK MemFn -> Request -> Property -> Property
coverKindsK y q =
  P.foldr
    (\k f -> cover 3 (reqKindK y q == k) k . f)
    id
    ["none", "fetch", "write hit", "write miss", "straddling write", "hit", "miss", "straddling read"]

serverTests :: TestTree
serverTests =
  testGroup
    "server proof"
    [ testProperty "the empty cache starts in the invariant" $
        forAllShow genMemK (const "<memory>") $ \mem ->
          forAll (fromIntegral <$> choose (0 :: Int, 255)) $ \s ->
            forAll (fromIntegral <$> choose (0 :: Int, 3)) $ \w -> srvInitK s w mem,
      testProperty "refinement: the hop of a request ends with ideal memory's response" $
        withMaxSuccess 4000 $ checkCoverage $
          forAllShow genSrvK (const "<server>") $ \y -> forAll genReqK $ \q -> forAll (genWitness q) $ \(wa, ws, ww) ->
            coverKindsK y q $
              cover 80 (srvInvAtK ws ww y) "in the invariant at the witness" $
                cover 10 (ws == maybe 0 (setOf . memAddress) q P.&& ww == maybe 0 (wordOf . memAddress) q) "the witness is the accessed word" $
                  srvRefineK wa ws ww y q,
      testProperty "leakage: the stalls and the next tags follow from the tags and the observed request" $
        withMaxSuccess 4000 $ checkCoverage $
          forAllShow genSrvK (const "<server>") $ \y -> forAll genReqK $ \q -> forAll (genWitness q) $ \(_, ws, _) ->
            coverKindsK y q (property (srvLeakK ws y q)),
      testProperty "the cache answers late only to a data read" $
        withMaxSuccess 2000 $
          forAllShow genSrvK (const "<server>") $ \y -> forAll genReqK $ \q -> stallsOnReadsK y q
    ]

-- Composition, end to end -----------------------------------------------------------

-- | The requests the core makes behind the 4 KiB cache, cycle by cycle, until it
-- halts.
cachedObsK :: Int -> Sys -> [Obs]
cachedObsK n sys = go n (initCacheSys sys emptyCache4K)
  where
    go :: Int -> CacheSys RegFile MemBytes Cache4K -> [Obs]
    go 0 _ = []
    go j cs
      | P.not (running (cacheSysCore cs)) = []
      | otherwise = let (cs', o) = stepCachedKOut cs in obsOf o : go (j - 1) cs'

-- | The same trace from the leakage alone: the core's simulator produces each
-- hop's requests as behind ideal memory, and the cache's simulator adds the
-- cycles in which the core waits.
compositeTraceK :: Int -> Sys -> [Obs]
compositeTraceK n sys0 = go n (archOfLeak sys0, censor sys0) tagEmpty
  where
    go 0 _ _ = []
    go k as t =
      let (as', HopObs o1 o2 o3 o4) = leakSimHop as
          (t', os) = serveAll t (catMaybes [o1, o2, o3, o4])
       in os P.++ go (k - 1) as' t'
    serveAll t [] = (t, [])
    serveAll t (o : os) =
      let (t1, xs) = stBusK t o
          (t2, ys) = serveAll t1 os
       in (t2, xs P.++ ys)

compositeReportK :: Sys -> String
compositeReportK sys0
  | cached == P.take (P.length cached) composite = ""
  | otherwise = "first difference at cycle " P.++ show i P.++ ": " P.++ show (P.take 6 (P.drop i cached)) P.++ " vs " P.++ show (P.take 6 (P.drop i composite))
  where
    cached = cachedObsK 600 sys0
    composite = compositeTraceK 600 sys0
    i = P.length (P.takeWhile id (P.zipWith (==) cached composite))

-- | The cycles in which the core behind the cache waits, until it halts.
stallCyclesK :: Sys -> Int
stallCyclesK sys0 = go (600 :: Int) (initCacheSys sys0 emptyCache4K)
  where
    go 0 _ = 0
    go j cs
      | P.not (running (cacheSysCore cs)) = 0
      | otherwise =
          (if inputMemReady (sysInput (cacheSysCore cs)) then 0 else 1)
            + go (j - 1) (fst (stepCachedKOut cs))
