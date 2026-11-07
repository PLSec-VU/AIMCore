{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The cache's proof and link 2 on random states, and the core behind the
-- cache end to end: the cache's counterpart of "ProofSpec".
--
-- The statements are the ones "Proof.Memory.Cache4K.Server.Functional.Solver",
-- "Proof.Memory.Cache4K.Server.Leakage.Solver",
-- "Proof.Memory.Cache4K.CachedCore.Functional.Solver" and
-- "Proof.Memory.Cache4K.CachedCore.Leakage.Solver" prove. Here they run on concrete
-- states, without the trusted array embeddings the solver relies on.
--
-- The end-to-end runs check what no single proof states: that link 1 (the
-- core behind ideal memory refines the ISA) and link 2 (the core behind the
-- cache refines the core behind ideal memory) compose, on real programs.
module CacheProofSpec
  ( cacheProofTests,
  )
where

import Cache4KSpec (TestMem (..), genAddr, genMem, genTag, genWord)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Core.Pipeline as Core
import Core.ISA (IsaState, IsaStateG (..), StepG (..), isaStep)
import Core.Instruction
import Core.RegFile (RegFile, RegFileOps (..), RegFn (..))
import Data.Maybe (isJust, isNothing)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types
import Proof.Memory.Cache4K.CachedCore.Functional.Driver (cachedDriver, cachedDriverMatchesSpec)
import Proof.Memory.Cache4K.CachedCore.Functional.Machine
import qualified Proof.Memory.Cache4K.CachedCore.Functional.Obligation as Cached
import Proof.Memory.Cache4K.CachedCore.Leakage.Driver (simCachedDriverMatchesSpec)
import Proof.Memory.Cache4K.CachedCore.Leakage.Invariant (simCachedOf)
import qualified Proof.Memory.Cache4K.CachedCore.Leakage.Obligation as CachedLeak
import Proof.Memory.Cache4K.CachedCore.Leakage.Simulator (SimCachedSys (..), simInitCachedSys, simStepCached)
import Proof.Memory.Cache4K.SMT.Raw (RawData (..))
import Proof.Memory.Cache4K.Server.Functional.Contract
  ( corePatient,
    srvDriverMatchesSpec,
    srvReadyIffAnswered,
    srvReadyUnderInv,
  )
import Proof.Memory.Cache4K.Server.Functional.Driver (srvDriver)
import Proof.Memory.Cache4K.Server.Functional.Invariant (coherentAt)
import Proof.Memory.Cache4K.Server.Functional.Obligation (CoherentOps (..))
import qualified Proof.Memory.Cache4K.Server.Functional.Obligation as Srv
import Proof.Memory.Cache4K.Server.Leakage.Driver (simSrvDriverMatchesSpec)
import Proof.Memory.Cache4K.Server.Leakage.Invariant (simOf)
import Proof.Memory.Cache4K.Server.Leakage.Model (obsReq)
import qualified Proof.Memory.Cache4K.Server.Leakage.Obligation as SrvLeak
import Proof.Core.Functional.Machine
import Proof.Core.Leakage.Driver (simDriver)
import Proof.Core.Leakage.Model (Leak, Obs (..), leakOf, obsOf)
import Proof.Core.Leakage.Simulator (SimInput, SimState, simCircuit, simInit)
import ProofSpec (genArbSys, genArbSys1, initSysProg)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

cacheProofTests :: TestTree
cacheProofTests =
  testGroup
    "Cache proof and the core behind the cache"
    [ serverTests,
      cachedCoreTests,
      endToEndTests
    ]

-- The coherent cache ---------------------------------------------------------------

-- | A coherent cache in front of the tests' memories, as
-- 'Proof.Memory.Cache4K.Server.Functional.Obligation.mkDataMem' builds one in front of
-- the solver's: a valid line holds memory's words, an invalid line raw ones.
coherentData :: (MemOps m) => m -> TagArr -> RawData -> DataArr
coherentData mem tags (RawData raw) = DataArr (imap word raw)
  where
    word :: Index 1024 -> Word -> Word
    word i w
      | entryValid e = memReadWord (wordAddr (entryTag e) si wi 0) mem
      | otherwise = w
      where
        idx = fromIntegral i :: BitVector 10
        si = slice d9 d2 idx
        wi = slice d1 d0 idx
        e = tagRead si tags

instance CoherentOps TestMem where
  mkData = coherentData

instance CoherentOps MemFn where
  mkData = coherentData

-- Generators -----------------------------------------------------------------------

-- | Tag entries: mostly valid, with the tags of the first three 4 KiB blocks.
genTags :: Gen TagArr
genTags =
  TagArr . unsafeFromList
    <$> vectorOf 256 (frequency [(2, pure 0), (8, ((1 :: BitVector 1) ++#) <$> genTag)])

-- | The words of the invalid lines, or of any line for a cache that need not be
-- coherent.
genRaw :: Gen RawData
genRaw = RawData . unsafeFromList <$> vectorOf 1024 genWord

-- | Ready, or waiting for memory with up to 2 * 'penalty' cycles left.
genSrvState :: Gen CacheSrvState
genSrvState =
  frequency
    [ (4, pure CacheSrvReady),
      (1, CacheSrvStall <$> (fromIntegral <$> choose (0 :: Int, 7)) <*> genWord)
    ]

-- | Requests: none, fetches, and reads and writes of every width and alignment.
genReq :: Gen Request
genReq =
  frequency
    [ (1, pure Nothing),
      (2, Just . MemFetch <$> genAddr),
      (8, (\a size -> Just (MemRead a size)) <$> genAddr <*> genSize),
      (4, (\a size v -> Just (MemWrite a size v)) <$> genAddr <*> genSize <*> genWord)
    ]
  where
    genSize = elements [Byte, Half, Types.Word]

-- | The address a request accesses, 0 for none.
reqAddr :: Request -> Address
reqAddr req =
  case req of
    Just (MemFetch a) -> a
    Just (MemRead a _) -> a
    Just (MemWrite a _ _) -> a
    Nothing -> 0

-- | Witnesses, half of the time at the request's own bytes, set and word.
genWitness :: Address -> Gen (Address, SetIdx, WordIdx)
genWitness a = do
  wa <- frequency [(1, (\o -> a + fromIntegral o) <$> choose (0 :: Int, 4)), (1, genAddr)]
  wsi <- frequency [(1, pure (setOf a)), (1, fromIntegral <$> choose (0 :: Int, 255))]
  wwi <- frequency [(1, pure (wordOf a)), (1, fromIntegral <$> choose (0 :: Int, 3))]
  pure (wa, wsi, wwi)

-- | The register file of a generated state, as a 'Vec', so that cores can be
-- compared.
toRegFile :: RegFn -> RegFile
toRegFile (RegFn g) = P.foldr (\i -> modifyRFg i (g i)) initRFg [1 .. 31]

-- | An arbitrary system from "ProofSpec"'s generators, sometimes with a load in
-- the memory stage, so that the cycle makes a data read.
genSys :: Gen (SysG RegFile MemFn, Address)
genSys = do
  (Sys core inp mem, _, wa) <- oneof [genArbSys, genArbSys1]
  me <- frequency [(1, pure Nothing), (1, Just <$> genLoad)]
  let core' = maybe core (\ir -> core {stateMeInstr = ir}) me
  pure (Sys core' {stateRegFile = toRegFile (stateRegFile core)} inp mem, wa)
  where
    genLoad =
      (\size sign rd rs1 imm -> IType (Load size sign) rd rs1 imm)
        <$> elements [Byte, Half, Types.Word]
        <*> elements [Signed, Unsigned]
        <*> (fromIntegral <$> choose (0 :: Int, 31))
        <*> (fromIntegral <$> choose (0 :: Int, 31))
        <*> (fromIntegral <$> choose (0 :: Int, 63))

p :: Int
p = fromIntegral penalty

-- | Cover each hop length of the cache.
coverHops :: Int -> Property -> Property
coverHops k =
  cover 20 (k == 0) "hop of 1 cycle"
    . cover 10 (k == p) "hop of penalty + 1 cycles"
    . cover 1 (k == 2 * p) "hop of 2 * penalty + 1 cycles"

-- The cache's proof ----------------------------------------------------------------

serverTests :: TestTree
serverTests =
  testGroup
    "the cache's proof"
    [ testProperty "functional: base case" $
        forAll genMem $ \mem -> forAll (genWitness 0) $ \(wa, wsi, wwi) ->
          Srv.baseCase wa wsi wwi mem,
      -- The step for the hop length the driver gives, from a ready server whose
      -- cache is coherent by construction, as in the solver.
      testProperty "functional: the step for the driver's hop length" $
        withMaxSuccess 4000 $ checkCoverage $
          forAll genMem $ \mem -> forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
            forAll genReq $ \req -> forAll (genWitness (reqAddr req)) $ \(wa, wsi, wwi) ->
              let srv = CacheSrv (Cache4K tags (mkData mem tags raw)) mem CacheSrvReady
                  k = srvDriver srv req
                  step
                    | k == 0 = Srv.indStep0 wa wsi wwi mem tags raw req
                    | k == p =
                        Srv.indStep4 wa wsi wwi mem tags raw req
                          P.&& Srv.indStepWaits4 wa wsi wwi mem tags raw req
                    | k == 2 * p =
                        Srv.indStep8 wa wsi wwi mem tags raw req
                          P.&& Srv.indStepWaits8 wa wsi wwi mem tags raw req
                    | otherwise = False
               in coverHops k (counterexample ("driver=" P.++ show k) step),
      testProperty "the driver agrees with its operational reading" $
        withMaxSuccess 4000 $
          forAll genMem $ \mem -> forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
            forAll genSrvState $ \st -> forAll genReq $ \req ->
              srvDriverMatchesSpec (CacheSrv (Cache4K tags (DataArr (rawVec raw))) mem st) req,
      testProperty "contract: after a step, the server is ready exactly when it has answered" $
        withMaxSuccess 4000 $
          forAll genMem $ \mem -> forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
            forAll genSrvState $ \st -> forAll genReq $ \req ->
              srvReadyIffAnswered (CacheSrv (Cache4K tags (DataArr (rawVec raw))) mem st) req,
      testProperty "contract: a server related to ideal memory is ready" $
        forAll genMem $ \mem -> forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
          forAll (genWitness 0) $ \(wa, wsi, wwi) ->
            srvReadyUnderInv wa wsi wwi mem (CacheSrv (Cache4K tags (mkData mem tags raw)) mem CacheSrvReady),
      testProperty "contract: the core waits for what it asked for" $
        withMaxSuccess 4000 $ checkCoverage $
          forAllShow genSys (const "<state>") $ \(Sys core inp _, _) ->
            cover 30 (isJust (outputMem (snd (circuit core inp)))) "the cycle makes a request" $
              corePatient core inp,
      testProperty "leakage: base case" $
        forAll genMem $ \mem -> forAll (fromIntegral <$> choose (0 :: Int, 255)) $ \wsi ->
          SrvLeak.baseCaseLeak wsi mem,
      testProperty "leakage: the step, on any server" $
        withMaxSuccess 4000 $
          forAll genMem $ \mem -> forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
            forAll genSrvState $ \st -> forAll genReq $ \req -> forAll (genWitness (reqAddr req)) $ \(_, wsi, _) ->
              SrvLeak.indStepLeak wsi (CacheSrv (Cache4K tags (DataArr (rawVec raw))) mem st) req,
      testProperty "leakage: the simulator's driver agrees with its operational reading" $
        withMaxSuccess 4000 $
          forAll genMem $ \mem -> forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
            forAll genSrvState $ \st -> forAll genReq $ \req ->
              simSrvDriverMatchesSpec (simOf (CacheSrv (Cache4K tags (DataArr (rawVec raw))) mem st)) (obsReq req)
    ]
  where
    rawVec (RawData v) = v

-- Link 2 ---------------------------------------------------------------------------

cachedCoreTests :: TestTree
cachedCoreTests =
  testGroup
    "link 2: the core behind the cache"
    [ -- The step for the hop length the driver gives, functional and leakage
      -- halves together, from the cached system built from an arbitrary one.
      testProperty "the step for the driver's hop length" $
        withMaxSuccess 4000 $ checkCoverage $
          forAllShow genSys (const "<state>") $ \(sys, wa) ->
            forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
              forAll (genWitness wa) $ \(_, wsi, wwi) ->
                let k = cachedDriver (Cached.cachedOf sys tags raw)
                    step
                      | k == 0 =
                          Cached.indStep0 wa wsi wwi sys tags raw
                            P.&& CachedLeak.indStepLeak0 wa wsi wwi sys tags raw
                      | k == p =
                          Cached.indStep4 wa wsi wwi sys tags raw
                            P.&& CachedLeak.indStepLeak4 wa wsi wwi sys tags raw
                      | k == 2 * p =
                          Cached.indStep8 wa wsi wwi sys tags raw
                            P.&& CachedLeak.indStepLeak8 wa wsi wwi sys tags raw
                      | otherwise = False
                 in coverHops k (counterexample ("driver=" P.++ show k) step),
      testProperty "the driver agrees with its operational reading" $
        withMaxSuccess 4000 $
          forAllShow genSys (const "<state>") $ \(sys, wa) ->
            forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
              forAll (genWitness wa) $ \(_, wsi, wwi) ->
                cachedDriverMatchesSpec wa wsi wwi sys (Cached.cachedOf sys tags raw),
      testProperty "the simulator's driver agrees with its operational reading" $
        withMaxSuccess 4000 $
          forAllShow genSys (const "<state>") $ \(sys, _) ->
            forAll genTags $ \tags -> forAllShow genRaw (const "<raw words>") $ \raw ->
              simCachedDriverMatchesSpec
                (simCachedOf (Cached.cachedOf sys tags raw))
                (obsOf (snd (stepSysOut sys)))
    ]

-- End to end -----------------------------------------------------------------------

endToEndTests :: TestTree
endToEndTests =
  testGroup
    "end to end"
    [ testProperty "the cached run is the ideal run with stall cycles inserted" $
        withMaxSuccess 300 $ checkCoverage $
          forAll genCTProg $ \prog ->
            let sys = initSysProg prog
                r = stutterReport 300 sys
             in cover 30 (stallCycles sys > 0) "the core waits for the cache" (counterexample r (P.null r)),
      testProperty "... also with reads and writes of every width and alignment" $
        withMaxSuccess 300 $ checkCoverage $
          forAll genByteProg $ \prog -> forAll genData $ \ds ->
            let sys = withData ds (initSysProg prog)
                r = stutterReport 300 sys
             in cover 30 (stallCycles sys > 0) "the core waits for the cache" (counterexample r (P.null r)),
      testProperty "the cached core's bus trace follows from the leakage alone" $
        withMaxSuccess 300 $
          forAll genCTProg $ \prog ->
            let r = compositeReport (initSysProg prog)
             in counterexample r (P.null r),
      testProperty "... also with reads and writes of every width and alignment" $
        withMaxSuccess 300 $
          forAll genByteProg $ \prog -> forAll genData $ \ds ->
            let r = compositeReport (withData ds (initSysProg prog))
             in counterexample r (P.null r),
      testProperty "a secret-free program leaks nothing about its data, behind the cache" $
        withMaxSuccess 300 $
          forAll genSecretFreeProg $ \prog ->
            forAll genData $ \da ->
              forAll genData $ \db ->
                let s1 = withData da (initSysProg prog)
                    s2 = withData db (initSysProg prog)
                 in ctTrace 100 (isaStart s1) == ctTrace 100 (isaStart s2)
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
            timing ptr = P.map (/= NoAccess) (cachedObs 100 (withData [(0, ptr)] (initSysProg prog)))
         in once (timing 0 /= timing 16)
    ]

-- | The core behind the cache at start, for an ideal system at start: the same
-- core and input, and the cache at start in front of its memory.
cachedStart :: SysG r m -> CachedSysG r m
cachedStart (Sys core inp mem) = CachedSys core inp (initCacheSrv mem)

-- | The cached system as an ideal one: its core and input, and the memory
-- behind the cache.
idealView :: CachedSysG r m -> SysG r m
idealView (CachedSys core inp srv) = Sys core inp (srvMem srv)

-- | Has the core not halted yet?
running :: StateG r -> Bool
running = isNothing . stateHalt

-- | The ISA state a program at start corresponds to: execution begins at
-- 'initPc', with the system's register file and memory.
isaStart :: Sys -> IsaState
isaStart (Sys core _ mem) = IsaState initPc (stateRegFile core) mem

-- | The bus observations of the core behind the cache, cycle by cycle, until it
-- halts.
cachedObs :: Int -> Sys -> [Obs]
cachedObs n sys = go n (cachedStart sys)
  where
    go :: Int -> CachedSys -> [Obs]
    go 0 _ = []
    go j csys
      | P.not (running (cachedCore csys)) = []
      | otherwise = let (csys', o) = stepCachedOut csys in obsOf o : go (j - 1) csys'

-- | The cycles in which the core behind the cache waits, until it halts.
stallCycles :: Sys -> Int
stallCycles sys = go (600 :: Int) (cachedStart sys)
  where
    go 0 _ = 0
    go j csys
      | P.not (running (cachedCore csys)) = 0
      | otherwise =
          (if isNothing (inputMem (cachedInput csys)) then 1 else 0)
            + go (j - 1) (stepCached csys)

-- | The core's bus observations computed from the leakage alone, hop by hop,
-- as behind ideal memory: the core's simulator
-- ('Proof.Core.Leakage.Simulator.simCircuit') run on the ISA's leakage trace.
--
-- The hops are those of 'ProofSpec.leakReport': the first gets no leakage,
-- and every later one gets @'leakOf' isa@ on its first cycle, after which the
-- ISA steps. A halted ISA stands still.
simHops :: Sys -> [[Obs]]
simHops sys0 = go Nothing (isaStart sys0) simInit
  where
    go :: SimInput -> IsaState -> SimState -> [[Obs]]
    go inp isa sim =
      let (sim', os) = runSim (simDriver inp sim + 1) inp sim
          isa' =
            case inp of
              Nothing -> isa
              Just _ -> isaNext isa
       in os : go (Just (leakOf isa')) isa' sim'
    -- The simulator for @n@ cycles; it gets @inp@ on the first only.
    runSim :: Int -> SimInput -> SimState -> (SimState, [Obs])
    runSim 0 _ sim = (sim, [])
    runSim n inp sim =
      let (sim1, o) = simCircuit sim inp
          (sim2, os) = runSim (n - 1) Nothing sim1
       in (sim2, o : os)
    isaNext isa =
      case isaStep isa of
        Next isa' -> isa'
        IsaHalted -> isa

-- | The bus trace of the core behind the cache, from the leakage alone: the
-- core's simulator gives each cycle of the core behind ideal memory
-- ('simHops'), and link 2's simulator
-- ("Proof.Memory.Cache4K.CachedCore.Leakage.Simulator") adds the cycles in which the
-- core waits for the cache.
compositeTrace :: Int -> Sys -> [Obs]
compositeTrace n sys0 = go (P.concat (P.take n (simHops sys0))) simInitCachedSys
  where
    go :: [Obs] -> SimCachedSys -> [Obs]
    go [] _ = []
    go (obs : rest) sim =
      let (sim', bus) = simStepCached sim (Just obs)
       in bus : waiting rest sim'
    waiting rest sim
      | simCachedReady sim = go rest sim
      | otherwise =
          let (sim', bus) = simStepCached sim Nothing
           in bus : waiting rest sim'

compositeReport :: Sys -> String
compositeReport sys0
  | cached == P.take (P.length cached) composite = ""
  | otherwise =
      "first difference at cycle " P.++ show i P.++ ": "
        P.++ show (P.take 6 (P.drop i cached)) P.++ " vs "
        P.++ show (P.take 6 (P.drop i composite))
  where
    cached = cachedObs 600 sys0
    composite = compositeTrace 600 sys0
    i = P.length (P.takeWhile id (P.zipWith (==) cached composite))

-- | Is the cache coherent with memory at every set the harness's memory can
-- fill? Memory is 'MEM_SIZE' words, 400 bytes, so only sets 0 to 24.
coherentLow :: MemBytes -> Cache4K -> Bool
coherentLow mem c = P.and [coherentAt s w mem c | s <- [0 .. 24], w <- [0 .. 3]]

-- | Walk the cached system and the ideal one together for @n@ cycles of the
-- cached one, skipping the cached run's stall cycles -- those whose input is
-- 'Nothing' -- in the ideal one. Returns @\"\"@ if every stall cycle makes no
-- request and leaves the core alone, the cache stays coherent with memory, and
-- every other cycle makes the ideal run's request from the ideal run's state,
-- up to 'sameModuloCtrl'.
stutterReport :: Int -> Sys -> String
stutterReport n sys0 = go n (0 :: Int) (cachedStart sys0) sys0
  where
    go :: Int -> Int -> CachedSys -> Sys -> String
    go 0 _ _ _ = ""
    go j c csys ideal
      | P.not (running (cachedCore csys)) = ""
      | P.not (coherentLow (srvMem srv) (srvCache srv)) =
          failAt "the cache is not coherent with memory" ""
      | isNothing (inputMem (cachedInput csys)) =
          if obsOf oC /= NoAccess
            then failAt "a stall cycle made a request: " (show (obsOf oC))
            else
              if P.not (sameModuloCtrl (idealView csys) (withInput (idealView csys')))
                then
                  failAt "a stall cycle changed the core: "
                    (P.unwords (coreDiff (idealView csys) (withInput (idealView csys'))))
                else go (j - 1) (c + 1) csys' ideal
      | P.not (sameModuloCtrl (idealView csys) ideal) =
          failAt "the cached core left the ideal run: "
            (P.unwords (coreDiff ideal (idealView csys)))
      | obsOf oC /= obsOf oI = failAt "different requests: " (show (obsOf oC, obsOf oI))
      | otherwise = go (j - 1) (c + 1) csys' ideal'
      where
        srv = cachedSrv csys
        -- The answer may arrive in a stall cycle; only the core must stay.
        withInput (Sys core _ mem) = Sys core (cachedInput csys) mem
        (csys', oC) = stepCachedOut csys
        (ideal', oI) = stepSysOut ideal
        failAt what detail = "\ncycle " P.++ show c P.++ ": " P.++ what P.++ detail P.++ "\n"

-- Comparing systems ----------------------------------------------------------------

-- | Two systems equal in everything but the control lines, which the core
-- resets in every cycle before any stage reads them.
sameModuloCtrl :: Sys -> Sys -> Bool
sameModuloCtrl (Sys a ia ma) (Sys b ib mb) =
  a {stateCtrl = initCtrl} == b {stateCtrl = initCtrl}
    P.&& ia == ib
    P.&& ma == mb

-- | The fields, other than the control lines, on which two systems differ, as
-- @field(first | second)@.
coreDiff :: Sys -> Sys -> [String]
coreDiff (Sys a ia ma) (Sys b ib mb) =
  P.concat
    [ f "fePc" stateFePc,
      f "dePc" stateDePc,
      f "deExpInstr" stateDeExpInstr,
      f "exPc" stateExPc,
      f "ex" stateExInstr,
      f "me" stateMeInstr,
      f "meAddr" stateMeAddr,
      f "meRes" stateMeRes,
      f "wb" stateWbInstr,
      f "wbRes" stateWbRes,
      f "halt" stateHalt,
      ["input(" P.++ show (inputMem ia) P.++ " | " P.++ show (inputMem ib) P.++ ")" | ia /= ib],
      ["registers" | stateRegFile a /= stateRegFile b],
      ["memory" | ma /= mb]
    ]
  where
    f :: (Eq v, Show v) => String -> (State -> v) -> [String]
    f name sel
      | sel a == sel b = []
      | otherwise = [name P.++ "(" P.++ show (sel a) P.++ " | " P.++ show (sel b) P.++ ")"]

-- Programs -------------------------------------------------------------------------

-- | The ISA-level constant-time leakage trace of the first @n@ instructions.
ctTrace :: Int -> IsaState -> [Leak]
ctTrace 0 _ = []
ctTrace n isa = leakOf isa : case isaStep isa of
  Next isa' -> ctTrace (n - 1) isa'
  IsaHalted -> []

-- | Load a data image (word-aligned addresses below the program) into a system.
withData :: [(Address, Word)] -> Sys -> Sys
withData ws (Sys s i m) = Sys s i (P.foldr (\(a, w) -> memWrite a Types.Word w) m ws)

genData :: Gen [(Address, Word)]
genData =
  P.mapM (\k -> (\w -> (4 * k, w)) <$> elements [0, 1, 4, 0x10, 0x40, 0xDEADBEEF]) [0 .. 49]

-- | Random programs whose loads, stores and @jalr@s go through base registers.
--
-- @x5@ and @x6@ are reserved as base registers: they are written only by
-- @addi@ from @x0@, with a word-aligned data address (below 'initPc') or the
-- address of an instruction of the program. Stores through them therefore also
-- hit the code, which raises store hazards; they store @x0@, so an overwritten
-- instruction decodes to @Nop DecodeFail@. Every access stays inside the
-- harness's memory. A @jalr@ off @x0@, whose target fits the immediate, is
-- generated as well.
genCTProg :: Gen (Vec PROG_SIZE Word)
genCTProg = do
  n <- choose (2, 12)
  body <- P.mapM (genCTInstr n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Core.Instruction.break
  pure (map encode (unsafeFromList instrs))

genCTInstr :: Int -> Int -> Gen Instruction
genCTInstr n i =
  frequency
    [ (2, RType <$> genArith <*> genDst <*> genSrc <*> genSrc),
      (1, IType <$> (Arith <$> genArith) <*> genDst <*> genSrc <*> genSmallImm),
      (3, (\b a -> IType (Arith ADD) b 0 (fromIntegral a)) <$> genBase <*> genAddrImm),
      ( 3,
        (\size sign rd b off -> IType (Load size sign) rd b off)
          <$> genSize
          <*> elements [Signed, Unsigned]
          <*> genDst
          <*> genBase
          <*> genOff
      ),
      (3, (\size off b -> SType size off b 0) <$> genSize <*> genOff <*> genBase),
      (1, (\t -> SType Types.Word (fromIntegral (codeAddr t)) 0 0) <$> choose (0, n)),
      ( 2,
        (\cmp t rs1 rs2 -> BType cmp (fromIntegral ((t - i) * 4)) rs1 rs2)
          <$> elements [EQ, NE, LT, GE, LTU, GEU]
          <*> choose (0, n)
          <*> genSrc
          <*> genSrc
      ),
      (1, (\rd t -> JType rd (fromIntegral ((t - i) * 4))) <$> genDst <*> choose (0, n)),
      (1, (\b -> IType Jump 0 b 0) <$> genBase),
      (1, (\t -> IType Jump 0 0 (fromIntegral (codeAddr t))) <$> choose (0, n))
    ]
  where
    genArith = elements [ADD, SUB, XOR, OR, AND, SLT, SLTU]
    genSize = elements [Byte, Half, Types.Word]
    genSmallImm = fromIntegral <$> choose (0 :: Int, 15)
    -- Base registers, and everything else.
    genBase = elements [5, 6]
    genDst = elements [1, 2, 3, 4, 7]
    genSrc = elements [0, 1, 2, 3, 4, 5, 6, 7]
    genOff = (\k -> fromIntegral (k * 4)) <$> choose (0 :: Int, 2)
    codeAddr t = fromIntegral initPc + 4 * t :: Int
    genAddrImm =
      oneof
        [ (\k -> 4 * k) <$> choose (0 :: Int, 40),
          codeAddr <$> choose (0, n)
        ]

-- | Programs whose control flow and addresses never depend on loaded data.
--
-- As 'genCTProg', with @x5@ and @x6@ the base registers, set only from
-- constants; loads write @x1@ to @x4@ and @x7@, and branches compare only
-- @x0@, @x5@ and @x6@, so no loaded value reaches a branch or an address.
-- Stores store @x0@, so no loaded value reaches memory either.
genSecretFreeProg :: Gen (Vec PROG_SIZE Word)
genSecretFreeProg = do
  n <- choose (2, 12)
  body <- P.mapM (genOne n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Core.Instruction.break
  pure (map encode (unsafeFromList instrs))
  where
    genOne n i =
      frequency
        [ (2, RType <$> elements [ADD, SUB, XOR, OR, AND] <*> genDst <*> genAny <*> genAny),
          (3, (\b a -> IType (Arith ADD) b 0 (fromIntegral a)) <$> genBase <*> genAddrImm n),
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
    genAddrImm n =
      oneof
        [ (\k -> 4 * k) <$> choose (0 :: Int, 40),
          (\t -> fromIntegral initPc + 4 * t) <$> choose (0, n)
        ]

-- | Programs whose loads and stores have every width and alignment, including
-- reads and writes that straddle two lines.
--
-- Base registers @x5@ and @x6@ are set only from constants, to data addresses,
-- so no access reaches the program; offsets are any byte offset up to 11.
-- Stores store data registers, loads write them, and branches compare them, so
-- loaded values flow back into memory and into control flow.
genByteProg :: Gen (Vec PROG_SIZE Word)
genByteProg = do
  n <- choose (2, 14)
  body <- P.mapM (genOne n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Core.Instruction.break
  pure (map encode (unsafeFromList instrs))
  where
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

-- | An instruction that survives encoding, or @addi x0, x0, 0@.
roundTrips :: Instruction -> Instruction
roundTrips ir =
  case encode' ir of
    Just w | decode' w == ir -> ir
    _ -> IType (Arith ADD) 0 0 0
