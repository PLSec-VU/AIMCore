{-# LANGUAGE PackageImports #-}

-- | The core behind a cache, checked against the core behind ideal memory.
--
-- The constant-time leakage proof ("Proof.Leakage.Induction") is about the core
-- attached to memory that answers every request in the next cycle. Behind a
-- cache ('Proof.Machine.stepCachedOut') a load that misses is answered only
-- after the miss penalty, and in the meantime the core stalls. The tests here
-- check the two facts that let the ideal-memory result carry over:
--
--   * a stall cycle is a stutter step ('Proof.Cache.Obligation.stallObligation'),
--     the cache stays coherent with memory, and every other cycle is a cycle of
--     the ideal run -- so the cached run is the ideal run with stall cycles
--     inserted, and makes the same requests;
--   * end to end, a program whose control flow and addresses do not depend on
--     the data it loads makes the same requests at the same times behind a
--     cache, whatever the data.
module CompositionSpec
  ( compositionTests,
    genByteProg,
    genData,
    genWord,
    withData,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Core
import Data.Functor.Identity (Identity (..))
import ISA (IsaState, StepG (..), isaStep)
import Instruction
import Data.Maybe (catMaybes)
import Data.Monoid (getFirst)
import LeakageSpec (genCTProg)
import Memory.Cache
import Memory.Types
import Proof.Cache.Obligation (stallObligation, waitObligation)
import Proof.Cache.Server
import Proof.Leakage.Model
import Proof.Leakage.Simulator (archOfLeak, censor, leakSimHop)
import Proof.Machine
import ProofSpec (genArbSys1)
import RegFile (RegFile, RegFileOps)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | The cache the tests use: the four-line cache the server proof is about
-- ("Proof.Cache.Server"), so lines collide often.
type TestCache = Cache4

-- | Two systems equal in everything but the control lines, which
-- 'Core.withCtrlReset' overwrites before any stage reads them.
sameModuloCtrl :: Sys -> Sys -> Bool
sameModuloCtrl (Sys a ia ma) (Sys b ib mb) =
  a {stateCtrl = initCtrl} == b {stateCtrl = initCtrl}
    P.&& inputIsInstr ia == inputIsInstr ib
    P.&& runIdentity (inputMem ia) == runIdentity (inputMem ib)
    P.&& inputMemReady ia == inputMemReady ib
    P.&& ma == mb

-- | The fields, other than the control lines, on which two systems differ, as
-- @field(first | second)@.
coreDiff :: Sys -> Sys -> [String]
coreDiff (Sys a ia ma) (Sys b ib mb) =
  P.concat
    [ f "fePc" stateFePc,
      f "dePc" stateDePc,
      f "exPc" stateExPc,
      f "ex" stateExInstr,
      f "me" stateMeInstr,
      f "meAddr" stateMeAddr,
      f "meRes" (runIdentity . stateMeRes),
      f "wb" stateWbInstr,
      f "wbRes" (runIdentity . stateWbRes),
      f "halt" stateHalt,
      f "haltNextPc" stateHaltNextPc,
      f "loadInFlight" stateLoadInFlight,
      g "isInstr" inputIsInstr,
      g "bus" (runIdentity . inputMem),
      g "ready" inputMemReady,
      ["registers" | stateRegFile a /= stateRegFile b],
      ["memory" | ma /= mb]
    ]
  where
    f :: (Eq v, Show v) => String -> (State Identity -> v) -> [String]
    f name sel
      | sel a == sel b = []
      | otherwise = [name P.++ "(" P.++ show (sel a) P.++ " | " P.++ show (sel b) P.++ ")"]
    g :: (Eq v, Show v) => String -> (Input Identity -> v) -> [String]
    g name sel
      | sel ia == sel ib = []
      | otherwise = [name P.++ "(" P.++ show (sel ia) P.++ " | " P.++ show (sel ib) P.++ ")"]

-- | Walk the cached system and the ideal one together for @n@ cycles of the
-- cached one, skipping the cached run's stall cycles -- those whose bus input is
-- not ready -- in the ideal one. Returns @\"\"@ if every stall cycle makes no
-- request and leaves the core alone, the cache stays coherent with memory, and
-- every other cycle makes the ideal run's request from the ideal run's state, up
-- to 'sameModuloCtrl'.
stutterReport :: Int -> Int -> Vec PROG_SIZE Word -> String
stutterReport penalty n prog = stutterReport' penalty n (initSys prog)

-- | 'stutterReport' from a given initial system.
stutterReport' :: Int -> Int -> Sys -> String
stutterReport' penalty n sys0 = go n (0 :: Int) (initCacheSys sys0 emptyCache4) sys0
  where

    go :: Int -> Int -> CacheSys RegFile MemBytes TestCache -> Sys -> String
    go 0 _ _ _ = ""
    go j c cs ideal
      | P.not (running core) = ""
      | P.not (coherent4 (sysMem core) (cacheSysCache cs)) = failAt "the cache is not coherent with memory" ""
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
        (cs', oC) = stepCachedOut (fromIntegral penalty) cs
        (ideal', oI) = stepSysOut ideal
        failAt what detail = "\ncycle " P.++ show c P.++ ": " P.++ what P.++ detail P.++ "\n"

-- | The ISA-level constant-time leakage trace of the first @n@ instructions.
ctTrace :: Int -> IsaState -> [L]
ctTrace 0 _ = []
ctTrace n a = leakOf a : case isaStep a of
  Next a' -> ctTrace (n - 1) a'
  IsaHalted -> []

-- | The requests the core makes behind a cache, cycle by cycle, until it halts.
cachedObs :: Int -> Int -> Sys -> [Obs]
cachedObs penalty n sys = go n (initCacheSys sys emptyCache4)
  where
    go :: Int -> CacheSys RegFile MemBytes TestCache -> [Obs]
    go 0 _ = []
    go j cs
      | P.not (running (cacheSysCore cs)) = []
      | otherwise = let (cs', o) = stepCachedOut (fromIntegral penalty) cs in obsOf o : go (j - 1) cs'

-- | Load a data image (word-aligned addresses below the program) into a system.
withData :: [(Address, Word)] -> Sys -> Sys
withData ws (Sys s i m) = Sys s i (P.foldr (\(a, w) -> memWriteWord Types.Word a w) m ws)

genData :: Gen [(Address, Word)]
genData =
  P.mapM (\k -> (\w -> (4 * k, w)) <$> elements [0, 1, 4, 0x10, 0x40, 0xDEADBEEF]) [0 .. 49]

-- | Programs whose control flow and addresses never depend on loaded data.
--
-- As 'LeakageSpec.genCTProg', with @x5@ and @x6@ the base registers, set only
-- from constants; loads write @x1@ to @x4@ and @x7@, and branches compare only
-- @x0@, @x5@ and @x6@, so no loaded value reaches a branch or an address. Stores
-- store @x0@, so no loaded value reaches memory either.
genSecretFreeProg :: Gen (Vec PROG_SIZE Word)
genSecretFreeProg = do
  n <- choose (2, 12)
  body <- P.mapM (genOne n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Instruction.break
  pure (map encode (unsafeFromList instrs))
  where
    roundTrips i = case encode' i of
      Just w | decode' w == i -> i
      _ -> IType (Arith ADD) 0 0 0

    genOne n i =
      frequency
        [ (2, RType <$> elements [ADD, SUB, XOR, OR, AND] <*> genDst <*> genAny <*> genAny),
          (3, (\b a -> IType (Arith ADD) b 0 (fromIntegral a)) <$> genBase <*> genAddr n),
          ( 3,
            (\size sign rd b off -> IType (Load size sign) rd b off)
              <$> elements [Byte, Half, Types.Word]
              <*> elements [Signed, Unsigned]
              <*> genDst
              <*> genBase
              <*> genOff
          ),
          (2, (\size off b -> SType size off b 0) <$> elements [Byte, Half, Types.Word] <*> genOff <*> genBase),
          ( 2,
            (\cmp t rs1 rs2 -> BType cmp (fromIntegral ((t - i) * 4)) rs1 rs2)
              <$> elements [EQ, NE, LT, GE, LTU, GEU]
              <*> choose (0, n)
              <*> genCtl
              <*> genCtl
          ),
          (1, (\rd t -> JType rd (fromIntegral ((t - i) * 4))) <$> genDst <*> choose (0, n))
        ]

    genBase = elements [5, 6]
    genCtl = elements [0, 5, 6]
    genDst = elements [1, 2, 3, 4, 7]
    genAny = elements [0, 1, 2, 3, 4, 5, 6, 7]
    genOff = (\k -> fromIntegral (k * 4)) <$> choose (0 :: Int, 2)
    genAddr n =
      oneof
        [ (\k -> 4 * k) <$> choose (0 :: Int, 40),
          (\t -> fromIntegral initPc + 4 * t) <$> choose (0, n)
        ]

-- | Programs whose loads and stores have every width and alignment, including
-- reads and writes that straddle two blocks.
--
-- Base registers @x5@ and @x6@ are set only from constants, to data addresses,
-- so no access reaches the program; offsets are any byte offset up to 11.
-- Stores store data registers, loads write them, and branches compare them, so
-- loaded values flow back into memory and into control flow.
genByteProg :: Gen (Vec PROG_SIZE Word)
genByteProg = do
  n <- choose (2, 14)
  body <- P.mapM (genOne n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Instruction.break
  pure (map encode (unsafeFromList instrs))
  where
    roundTrips i = case encode' i of
      Just w | decode' w == i -> i
      _ -> IType (Arith ADD) 0 0 0

    genOne n i =
      frequency
        [ (2, RType <$> elements [ADD, SUB, XOR, OR, AND] <*> genDst <*> genDst <*> genDst),
          (3, (\b k -> IType (Arith ADD) b 0 (fromIntegral (4 * k :: Int))) <$> genBase <*> choose (0, 40)),
          ( 4,
            (\size sign rd b off -> IType (Load size sign) rd b off)
              <$> elements [Byte, Half, Types.Word]
              <*> elements [Signed, Unsigned]
              <*> genDst
              <*> genBase
              <*> genOff
          ),
          (3, (\size off b rs2 -> SType size off b rs2) <$> elements [Byte, Half, Types.Word] <*> genOff <*> genBase <*> genDst),
          ( 1,
            (\cmp t rs1 rs2 -> BType cmp (fromIntegral ((t - i) * 4)) rs1 rs2)
              <$> elements [EQ, NE, LT, GE, LTU, GEU]
              <*> choose (i + 1, n)
              <*> genDst
              <*> genDst
          )
        ]

    genBase = elements [5, 6]
    genDst = elements [1, 2, 3, 4]
    genOff = fromIntegral <$> choose (0 :: Int, 11)

-- Server proof ------------------------------------------------------------------

-- | A memory with words of interest in its first 64 bytes, zero elsewhere.
genMem :: Gen MemBytes
genMem = do
  ws <- P.mapM (\k -> (\w -> (4 * k, w)) <$> genWord) [0 .. 15]
  pure (P.foldr (\(a, w) -> memWriteWord Types.Word a w) (Clash.Prelude.repeat 0) ws)

-- | The same, as a function, so that reads anywhere succeed: beyond the first
-- 64 bytes each byte is a function of its address.
genMemFn :: Gen MemFn
genMemFn = do
  mem <- genMem
  pure (MemFn (\a -> if a < 64 then memReadByte a mem else fromIntegral (a * 7 + 1)))

genWord :: Gen Word
genWord = oneof [elements [0, 1, 0xFF, 0x1234, 0x80000000, 0xDEADBEEF], fromIntegral <$> (arbitrary :: Gen Int)]

-- | The cache in front of memory, with no miss pending: each line invalid, or
-- valid with memory's word, mostly under a tag that puts its block in the first
-- 64 bytes; rarely a line with a wrong word, which takes the server out of its
-- invariant.
genSrv :: Gen (Srv MemFn)
genSrv = do
  mem <- genMemFn
  l0 <- genLine mem 0
  l1 <- genLine mem 1
  l2 <- genLine mem 2
  l3 <- genLine mem 3
  pure (CacheSrv (Cache4 l0 l1 l2 l3) Nothing mem)
  where
    genLine mem i =
      frequency
        [ (2, pure (CacheLine False 0 0)),
          (12, (\t -> CacheLine True t (memReadWord (t * 16 + i * 4) mem)) <$> genTag),
          (1, (\t w -> CacheLine True t w) <$> genTag <*> genWord)
        ]
    -- Mostly tags of the requested addresses, but also tags no address has
    -- (28 bits and up) and arbitrary ones.
    genTag =
      frequency
        [ (40, elements [0, 1, 2, 3]),
          (1, (\t k -> t + k * 0x10000000) <$> elements [0, 1, 2, 3] <*> elements [1, 8, 15]),
          (1, fromIntegral <$> choose (0 :: Integer, 0xFFFFFFFF))
        ]

-- | Requests near the first 64 bytes: none, fetches, and reads and writes of
-- every width and alignment, and also requests the core never makes.
genReq :: Gen Request
genReq =
  frequency
    [ (1, pure Nothing),
      (2, (\a -> Just (MemAccess True a Types.Word Nothing)) <$> genAddr),
      -- Requests the core never makes: fetches of other widths, and fetches
      -- carrying a value, which memory takes as writes.
      (1, (\a sz -> Just (MemAccess True a sz Nothing)) <$> genAddr <*> genSize),
      (1, (\a sz v -> Just (MemAccess True a sz (Just (Identity v)))) <$> genAddr <*> genSize <*> genWord),
      (6, (\a sz -> Just (MemAccess False a sz Nothing)) <$> genAddr <*> genSize),
      (3, (\a sz v -> Just (MemAccess False a sz (Just (Identity v)))) <$> genAddr <*> genSize <*> genWord)
    ]
  where
    genAddr = fromIntegral <$> choose (0 :: Int, 70)
    genSize = elements [Byte, Half, Types.Word]

-- | Operations on a cache, to compare 'Cache4' with 'DirectMapped' 4.
data CacheOp = Lookup Address | Insert Address Word | Invalidate Address
  deriving (Show)

genCacheOps :: Gen [CacheOp]
genCacheOps = listOf (oneof [Lookup <$> genA, Insert <$> genA <*> genWord, Invalidate <$> genA])
  where
    genA = fromIntegral <$> choose (0 :: Int, 200)

-- | Do the two caches answer every lookup alike, and agree on coherence with a
-- memory?
cachesAgree :: MemBytes -> [CacheOp] -> Bool
cachesAgree mem = go emptyCache4 (mkDirectMapped :: DirectMapped 4)
  where
    go c d [] = coherent4 mem c == coherent (`readWord` mem) d
    go c d (Lookup a : ops) = cacheLookup a c == cacheLookup a d P.&& go c d ops
    go c d (Insert a w : ops) = go (cacheInsert a w c) (cacheInsert a w d) ops
    go c d (Invalidate a : ops) = go (cacheInvalidate a c) (cacheInvalidate a d) ops

-- | The bus trace of the core behind the cache, computed from the leakage alone:
-- the core's simulator ('Proof.Leakage.Simulator.leakSimHop') produces each
-- hop's requests as behind ideal memory, and the cache's simulator
-- ('Proof.Cache.Server.stBus') adds the cycles in which the core waits.
compositeTrace :: Int -> Sys -> [Obs]
compositeTrace hops sys0 = go hops (archOfLeak sys0, censor sys0) emptyCache4
  where
    go 0 _ _ = []
    go k as t =
      let (as', HopObs o1 o2 o3 o4) = leakSimHop as
          (t', os) = serveAll t (catMaybes [o1, o2, o3, o4])
       in os P.++ go (k - 1) as' t'
    serveAll t [] = (t, [])
    serveAll t (o : os) =
      let (t1, xs) = stBus t o
          (t2, ys) = serveAll t1 os
       in (t2, xs P.++ ys)

-- | Does the composite trace match the cached run, cycle for cycle, until the
-- core halts?
compositeReport :: Sys -> String
compositeReport sys0
  | cached == P.take (P.length cached) composite = ""
  | otherwise = "first difference at cycle " P.++ show i P.++ ": " P.++ show (P.take 6 (P.drop i cached)) P.++ " vs " P.++ show (P.take 6 (P.drop i composite))
  where
    cached = cachedObs (fromIntegral penalty) 400 sys0
    composite = compositeTrace 400 sys0
    i = P.length (P.takeWhile id (P.zipWith (==) cached composite))

-- | What kind of request, against what cache: for the coverage checks.
reqKind :: Srv MemFn -> Request -> String
reqKind y q = case q of
  Nothing -> "none"
  Just (MemAccess _ _ _ (Just _)) -> "write"
  Just (MemAccess True _ _ Nothing) -> "fetch"
  Just (MemAccess False a sz Nothing)
    | P.not (inBlock a sz) -> "straddling read"
    | otherwise -> case cacheLookup a (srvCache y) of
        Just _ -> "hit"
        Nothing -> "miss"

-- | A load to put into the memory stage, or none.
genMeLoad :: Gen (Maybe Instruction)
genMeLoad =
  frequency
    [ (1, pure Nothing),
      ( 4,
        (\size sign rd rs1 imm -> Just (IType (Load size sign) rd rs1 imm))
          <$> elements [Byte, Half, Types.Word]
          <*> elements [Signed, Unsigned]
          <*> (fromIntegral <$> choose (0 :: Int, 31))
          <*> (fromIntegral <$> choose (0 :: Int, 31))
          <*> (fromIntegral <$> choose (0 :: Int, 63))
      )
    ]

-- | Does a cycle of the core from this state issue a data read?
issuesDataRead :: (RegFileOps r) => StateG r Identity -> Input Identity -> Bool
issuesDataRead s inp = case getFirst (outMem (snd (circuit s inp))) of
  Just (MemAccess False _ _ Nothing) -> True
  _ -> False

-- | The cycles in which the core behind the cache waits, until it halts.
stallCycles :: Sys -> Int
stallCycles sys0 = go (400 :: Int) (initCacheSys sys0 emptyCache4)
  where
    go 0 _ = 0
    go j cs
      | P.not (running (cacheSysCore cs)) = 0
      | otherwise =
          (if inputMemReady (sysInput (cacheSysCore cs)) then 0 else 1)
            + go (j - 1) (stepCached penalty cs)

-- | Coverage of the request kinds, each at least 5%.
coverKinds :: Srv MemFn -> Request -> Property -> Property
coverKinds y q =
  P.foldr
    (\k f -> cover 5 (reqKind y q == k) k . f)
    id
    ["none", "fetch", "write", "straddling read", "hit", "miss"]

-- | Coverage of states and requests the core never produces, each at least 2%.
coverCorners :: Srv MemFn -> Request -> Property -> Property
coverCorners y q =
  cover 2 farTag "a valid line with a tag no address has"
    . cover 2 valueFetch "a fetch carrying a value"
  where
    Cache4 l0 l1 l2 l3 = srvCache y
    farTag = P.any (\l -> clValid l P.&& clTag l >= 0x10000000) [l0, l1, l2, l3]
    valueFetch = case q of
      Just (MemAccess True _ _ (Just _)) -> True
      _ -> False

serverTests :: TestTree
serverTests =
  testGroup
    "Server proof for the cache"
    [ testProperty "the four-line cache agrees with DirectMapped 4" $
        withMaxSuccess 2000 $
          forAllShow genMem (const "<memory>") $ \mem -> forAll genCacheOps (cachesAgree mem),
      testProperty "fromBlock cuts a read's bytes out of its block's word" $
        forAll genWord $ \w -> forAll (choose (0 :: Int, 63)) $ \a ->
          fromBlock (fromIntegral a) w == w `shiftR` (8 * (a `P.mod` 4)),
      testProperty "the empty cache starts in the invariant, with the same tags for every memory" $
        forAllShow genMem (const "<memory>") srvInit,
      testProperty "refinement: the hop of a request ends with ideal memory's response" $
        withMaxSuccess 5000 $ checkCoverage $
          forAllShow genSrv (const "<server>") $ \y -> forAll genReq $ \q ->
            forAll (fromIntegral <$> choose (0 :: Int, 72)) $ \wa ->
              coverKinds y q (coverCorners y q (cover 60 (srvInv y) "in the invariant" (srvRefine wa y q))),
      testProperty "leakage: the ready bits and the next tags follow from the tags and the observed request" $
        withMaxSuccess 5000 $ checkCoverage $
          forAllShow genSrv (const "<server>") $ \y -> forAll genReq $ \q ->
            coverKinds y q (coverCorners y q (property (srvLeak y q))),
      testProperty "the cache answers late only to a data read" $
        withMaxSuccess 5000 $
          forAllShow genSrv (const "<server>") $ \y -> forAll genReq $ \q -> stallsOnReads y q,
      testProperty "patience: after a data read the core waits" $
        -- Arbitrary states, mostly with a load in the memory stage and none in
        -- flight, so that the cycle issues a data read.
        withMaxSuccess 5000 $ checkCoverage $
          forAllShow genArbSys1 (const "<state>") $ \(Sys s0 inp _, _, _) ->
            forAll genMeLoad $ \me ->
              let s = maybe s0 (\ir -> s0 {stateMeInstr = ir, stateLoadInFlight = False}) me
               in cover 50 (issuesDataRead s inp) "the cycle issues a data read" (waitObligation s inp),
      testProperty "composition: the cached core's bus trace follows from the leakage alone" $
        withMaxSuccess 500 $ checkCoverage $
          forAll genCTProg $ \prog ->
            let r = compositeReport (initSys prog)
             in cover 30 (stallCycles (initSys prog) > 0) "the core waits for the cache" (counterexample r (P.null r)),
      testProperty "... also with reads and writes of every width and alignment" $
        withMaxSuccess 500 $ checkCoverage $
          forAll genByteProg $ \prog -> forAll genData $ \ds ->
            let sys = withData ds (initSys prog)
                r = compositeReport sys
             in cover 30 (stallCycles sys > 0) "the core waits for the cache" (counterexample r (P.null r))
    ]

compositionTests :: TestTree
compositionTests =
  testGroup
    "Core behind a cache"
    [ serverTests, testProperty "the cached run is the ideal run with stall cycles inserted" $
        withMaxSuccess 1000 $
          forAll genCTProg $ \prog ->
            forAll (choose (1, 4)) $ \penalty ->
              let r = stutterReport penalty 300 prog
               in counterexample r (P.null r),
      testProperty "... also with reads and writes of every width and alignment" $
        withMaxSuccess 2000 $
          forAll genByteProg $ \prog ->
            forAll genData $ \ds ->
              forAll (choose (1, 4)) $ \penalty ->
                let r = stutterReport' penalty 300 (withData ds (initSys prog))
                 in counterexample r (P.null r),
      testProperty "a stall cycle is a stutter step, on arbitrary stalled states" $
        withMaxSuccess 5000 $
          forAllShow genArbSys1 (const "<state>") $ \(Sys st inp _, wr, _) ->
            isLoad (stateWbInstr st) ==>
              stallObligation wr st {stateLoadInFlight = True} inp {inputMemReady = False},
      testProperty "a secret-free program leaks nothing about its data, behind a cache" $
        withMaxSuccess 1000 $
          forAll genSecretFreeProg $ \prog ->
            forAll genData $ \da ->
              forAll genData $ \db ->
                let s1 = withData da (initSys prog)
                    s2 = withData db (initSys prog)
                 in ctTrace 100 (archOfLeak s1) == ctTrace 100 (archOfLeak s2)
                      P.&& cachedObs 3 300 s1 == cachedObs 3 300 s2,
      testProperty "a load through a loaded pointer leaks it, even to a timing attacker" $
        -- The control: @lw x5, 0(x0); lw x1, 0(x5); lw x2, 0(x0); ebreak@. The
        -- second load's address is data. With the pointer at 0 the second and
        -- third loads hit the line the first one filled; with the pointer at 16,
        -- which maps to the same line, both miss. So even an attacker who sees
        -- only in which cycles a request is made tells the two apart.
        let prog =
              map encode $
                unsafeFromList $
                  [ IType (Load Types.Word Signed) 5 0 0,
                    IType (Load Types.Word Signed) 1 5 0,
                    IType (Load Types.Word Signed) 2 0 0
                  ]
                    P.++ P.replicate 47 Instruction.break
            timing p = P.map (/= NoAccess) (cachedObs 3 100 (withData [(0, p)] (initSys prog)))
         in once (timing 0 /= timing 16)
    ]
