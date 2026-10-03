{-# LANGUAGE PackageImports #-}

-- | QuickCheck harness for the constant-time leakage refinement.
--
-- Two shapes of test, as in "ProofSpec":
--
--   * a /walk/, which runs the implementation and the leakage-plus-simulator
--     machine in lockstep from reset over a whole program and reports the first
--     hop at which they diverge. It never re-synchronises the simulator, so an
--     error that only shows after several hops of drift is still caught.
--   * the /obligation/ 'Proof.Leakage.Obligation.leakObligation', on reachable
--     states and on arbitrary invariant-shaped ones. This is the proposition
--     "Proof.Leakage.Induction" asks the solver to discharge, so a
--     counterexample here is a counterexample there, shrunk and printable.
module LeakageSpec
  ( leakageTests,
    leakWalkReport,
    genCTProg,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Core
import Data.Functor.Identity (Identity (..))
import ISA (IsaState, IsaStateG (..))
import Instruction
import Memory.Types
import Proof.Driver (driver, driverCaseName)
import Proof.Leakage.Model
import Proof.Leakage.Obligation
import Proof.Leakage.Simulator
import Proof.Machine
import ProofSpec (genArbSys, genArbSys1, genArbSys2, genArbSys3, genProg, genTakenTransfer, invTrace, progs)
import RegFile
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.QuickCheck
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | The simulator state the concrete harness uses: @Vec@-backed register file,
-- no memory.
type ConcreteSim = SimSys RegFile

-- | Simulator states equal at every register, not just at one witness.
simEqAll :: (RegFileOps r) => SimSys r -> SimSys r -> Bool
simEqAll a b = P.all (\wr -> simEq wr a b) [0 .. 31]

-- The lockstep walk ------------------------------------------------------------

-- | Run implementation and leakage-plus-simulator in lockstep for @n@ hops.
--
-- Returns @\"\"@ if they agree the whole way, and otherwise a description of
-- the first hop at which they do not.
leakWalkReport :: Int -> Vec PROG_SIZE Word -> String
leakWalkReport n prog = go n (0 :: Int) sys0 (archOfLeak sys0) (censor sys0)
  where
    sys0 = initSys prog

    go :: Int -> Int -> Sys -> IsaState -> ConcreteSim -> String
    go 0 _ _ _ _ = ""
    go j i sys isa ss
      | P.not (simEqAll (censor sysI) ss') =
          fail' ("censored state: " P.++ P.unwords (simDiff (censor sysI) ss')) "" ""
      | obsI /= obsL = fail' "observation" (show obsI) (show obsL)
      -- The architectural state must stay in step with the one the projection
      -- computes from the implementation. Checked only while the core runs: once
      -- it has halted, the execute stage walks past the trapping instruction.
      | running sysI P.&& archOfLeak sysI /= isa' =
          fail' "architectural state" (show (isaPc (archOfLeak sysI))) (show (isaPc isa'))
      | running sysI = go (j - 1) (i + 1) sysI isa' ss'
      | otherwise = ""
      where
        (sysI, obsI) = implHop sys
        ((isa', ss'), obsL) = leakSimHop (isa, ss)

        fail' what impl leak =
          P.concat
            [ "\nleakage refinement fails at hop ",
              show i,
              " (",
              what,
              ")\n  pre-state diff:  ",
              P.unwords (simDiff (censor sys) ss),
              "\n  driver: impl=",
              show (driver sys),
              " sim=",
              show (driver (installed ss (leakOf isa))),
              "\n  driver case: impl=",
              driverCaseName sys,
              " sim=",
              driverCaseName (installed ss (leakOf isa)),
              "\n  exPc=",
              show (stateExPc (sysState sys)),
              " ex=",
              show (exInstr sys),
              " me=",
              show (stateMeInstr (sysState sys)),
              " meAddr=",
              show (stateMeAddr (sysState sys)),
              "\n  leak=",
              show (leakOf isa),
              "\n  inv=",
              show (inv (leakOf isa)),
              "\n  implementation: ",
              impl,
              "\n  simulator:      ",
              leak,
              "\n"
            ]

-- | The simulator state with the leaked word on the bus, as
-- 'Proof.Leakage.Simulator.simHop' builds it before asking the driver.
installed :: SimSys r -> L -> SimSys r
installed (Sys s i m) l = Sys s (Input (inputIsInstr i) (Identity (invWord l)) True) m

-- | The fields on which two simulator states differ, as @field(impl|sim)@.
--
-- Mirrors the scalar conjuncts of 'Proof.Leakage.Simulator.simEq', plus the
-- registers that differ.
simDiff :: SimSys RegFile -> SimSys RegFile -> [String]
simDiff (Sys a ia _) (Sys b ib _) =
  P.concat
    [ f "fePc" stateFePc,
      f "dePc" stateDePc,
      f "exPc" stateExPc,
      f "ex" stateExInstr,
      f "me" stateMeInstr,
      f "meAddr" stateMeAddr,
      f "wb" stateWbInstr,
      f "meRes" (runIdentity . stateMeRes),
      f "wbRes" (runIdentity . stateWbRes),
      f "halt" stateHalt,
      f "haltNextPc" stateHaltNextPc,
      f "loadInFlight" stateLoadInFlight,
      g "isInstr" inputIsInstr,
      g "bus" (runIdentity . inputMem),
      [ "x" P.++ show r P.++ "(" P.++ show (reg a r) P.++ " | " P.++ show (reg b r) P.++ ")"
        | r <- [0 .. 31 :: RegIdx],
          reg a r /= reg b r
      ]
    ]
  where
    reg st r = runIdentity (lookupRF r (stateRegFile st))

    f :: (Eq v, Show v) => String -> (StateG RegFile Identity -> v) -> [String]
    f name sel
      | sel a == sel b = []
      | otherwise = [name P.++ "(" P.++ show (sel a) P.++ " | " P.++ show (sel b) P.++ ")"]

    g :: (Eq v, Show v) => String -> (Input Identity -> v) -> [String]
    g name sel
      | sel ia == sel ib = []
      | otherwise = [name P.++ "(" P.++ show (sel ia) P.++ " | " P.++ show (sel ib) P.++ ")"]

-- The obligation on reachable states --------------------------------------------

-- | 'Proof.Leakage.Obligation.leakObligation' at every state a program's walk
-- lands on, at every witness register.
obligationReport :: Int -> Vec PROG_SIZE Word -> String
obligationReport n prog =
  P.concat
    [ P.concat ["\nleakObligation fails at cycle ", show c, ", witness x", show wr, ": ex=", show (exInstr sys), "\n"]
      | (c, _, sys) <- invTrace n prog,
        wr <- [0 .. 31],
        P.not (leakObligation wr 0 sys)
    ]

-- | Does the leaked word decode back to the instruction 'inv' meant?
--
-- The simulator installs @'invWord' l@ on the bus and 'Core.decode' decodes
-- it, while 'censor' writes @'inv' l@ straight into the pipeline. If those two
-- disagree the censored state and the simulator state part company one hop
-- later.
roundTripReport :: Int -> Vec PROG_SIZE Word -> String
roundTripReport n prog =
  P.concat
    [ P.concat
        [ "\ninv does not round-trip at cycle ",
          show c,
          "\n  leak:    ",
          show l,
          "\n  inv:     ",
          show (inv l),
          "\n  decoded: ",
          show (decode' (invWord l)),
          "\n"
        ]
      | (c, _, sys) <- invTrace n prog,
        let l = leakOf (archOfLeak sys),
        decode' (invWord l) /= inv l
    ]

-- | The censored states along a walk that hold a secret: a result register, the
-- bus word, or a register other than the parked one.
censorReport :: Int -> Vec PROG_SIZE Word -> String
censorReport n prog =
  P.concat
    [ "\ncensor keeps a secret at cycle " P.++ show c
      | (c, _, sys) <- invTrace n prog,
        let st = sysState (censor sys),
        let parked = parkSource =<< exLeak sys,
        let expected r = case parked of
              Just (src, a) | src == r -> pack a
              _ -> 0,
        runIdentity (stateMeRes st) /= 0
          P.|| runIdentity (stateWbRes st) /= 0
          P.|| runIdentity (inputMem (sysInput (censor sys))) /= 0
          P.|| P.any (\r -> runIdentity (lookupRF r (stateRegFile st)) /= expected r) [0 .. 31]
    ]

-- Tests --------------------------------------------------------------------------

leakageTests :: TestTree
leakageTests =
  testGroup
    "Constant-time leakage refinement"
    [ testProperty "proj holds at reset for any program" $
        withMaxSuccess 2000 $
          forAll genProg $ \prog ->
            let sys0 = initSys prog
                (a0, ss0) = proj sys0
             in isaPc a0 == initPc P.&& simEqAll ss0 (censor sys0),
      testGroup
        "lockstep walk on fixed programs"
        [ testCase name $ expectEmpty (leakWalkReport 60 prog)
          | (name, prog) <- progs
        ],
      testProperty "lockstep walk on random programs" $
        withMaxSuccess 500 $
          forAll genProg $ \prog ->
            let r = leakWalkReport 40 prog
             in counterexample r (P.null r),
      testProperty "lockstep walk on programs addressing memory through registers" $
        withMaxSuccess 1000 $
          forAll genCTProg $ \prog ->
            let r = leakWalkReport 60 prog
             in counterexample r (P.null r),
      testProperty "inv round-trips through encode/decode" $
        withMaxSuccess 500 $
          forAll (oneof [genProg, genCTProg]) $ \prog ->
            let r = roundTripReport 40 prog
             in counterexample r (P.null r),
      testProperty "censor keeps no secret but the parked address" $
        withMaxSuccess 300 $
          forAll (oneof [genProg, genCTProg]) $ \prog ->
            let r = censorReport 40 prog
             in counterexample r (P.null r),
      testGroup
        "leakObligation on reachable states of fixed programs"
        [ testCase name $ expectEmpty (obligationReport 60 prog)
          | (name, prog) <- progs
        ],
      testProperty "leakObligation on reachable states of random programs" $
        withMaxSuccess 300 $
          forAll (oneof [genProg, genCTProg]) $ \prog ->
            let r = obligationReport 40 prog
             in counterexample r (P.null r),
      -- The obligation on ARBITRARY invariant-shaped pipeline states: the
      -- premise space the symbolic leakStep properties quantify over,
      -- unreachable states included.
      testProperty "leakObligation on arbitrary k=0 pipeline states" $
        withMaxSuccess 20000 $
          forAllShow genArbSys (const "<state; see counterexample below>") $
            \(sys, wr, wa) -> arbStateCE sys wr wa,
      testProperty "leakObligation on arbitrary k=1 pipeline states" $
        withMaxSuccess 20000 $
          forAllShow genArbSys1 (const "<state; see counterexample below>") $
            \(sys, wr, wa) -> arbStateCE sys wr wa,
      testProperty "leakObligation on arbitrary k=2 pipeline states" $
        withMaxSuccess 20000 $
          forAllShow genArbSys2 (const "<state; see counterexample below>") $
            \(_, sys, wr, wa) -> arbStateCE sys wr wa,
      testProperty "leakObligation on taken-branch states" $
        withMaxSuccess 20000 $
          forAllShow (genTakenTransfer False) (const "<state; see counterexample below>") $
            \(sys, wr, wa) -> arbStateCE sys wr wa,
      testProperty "leakObligation on jalr states" $
        withMaxSuccess 20000 $
          forAllShow (genTakenTransfer True) (const "<state; see counterexample below>") $
            \(sys, wr, wa) -> arbStateCE sys wr wa,
      testProperty "leakObligation on arbitrary k=3 pipeline states" $
        withMaxSuccess 20000 $
          forAllShow genArbSys3 (const "<state; see counterexample below>") $
            \(sys, wr, wa) -> arbStateCE sys wr wa,
      -- Guard against the arbitrary-state properties passing vacuously.
      testCase "arbitrary-state generators satisfy the leakage premises" $
        let count g sel =
              P.length
                [ ()
                  | x <- unGen (vectorOf 2000 g) (mkQCGen 13) 30,
                    let (s, wr, wa) = sel x,
                    leakPremises wr wa s
                ]
            ns =
              [ count genArbSys P.id,
                count genArbSys1 P.id,
                count genArbSys2 (\(_, s, wr, wa) -> (s, wr, wa)),
                count genArbSys3 P.id,
                count (genTakenTransfer False) P.id,
                count (genTakenTransfer True) P.id
              ]
         in if P.all (P.>= 100) ns
              then pure ()
              else assertFailure ("premise hit counts too low: " P.++ show ns),
      -- Guard against the walks passing vacuously: every driver case and every
      -- leakage class, parked or not, has to show up along them.
      testCase "the walks cover every driver case and leakage class" $
        let tags = P.concatMap (walkTags 60) ([prog | (_, prog) <- progs] P.++ sampled)
            sampled =
              [ unGen g (mkQCGen seed) 30
                | g <- [genProg, genCTProg],
                  seed <- [1 .. 200]
              ]
            missingCases = [c | c <- wantedCases, c `P.notElem` P.map fst tags]
            missingClasses = [c | c <- wantedClasses, c `P.notElem` P.map snd tags]
         in if P.null missingCases P.&& P.null missingClasses
              then pure ()
              else
                assertFailure $
                  "never exercised: driver cases "
                    P.++ show missingCases
                    P.++ ", leakage classes "
                    P.++ show missingClasses
    ]
  where
    expectEmpty r = if P.null r then pure () else assertFailure r

    wantedCases =
      [ "firstCycle",
        "env",
        "jump",
        "storeHazard/mem",
        "storeHazard/nomem",
        "loadHazard",
        "steady/wb-nomem",
        "steady/me-nomem",
        "steady/ex-nomem"
      ]

    wantedClasses =
      [ "plain",
        "branch-taken",
        "branch-untaken",
        "jal",
        "jalr",
        "jalr-parked",
        "load",
        "load-parked",
        "store",
        "store-parked",
        "ebreak"
      ]

-- | The obligation on one arbitrary state, with a readable counterexample.
arbStateCE :: SysG RegFn MemFn -> RegIdx -> Address -> Property
arbStateCE sys wr wa =
  counterexample
    ( P.concat
        [ "driverCase=",
          driverCaseName sys,
          " wr=" P.++ show wr P.++ " wa=" P.++ show wa,
          "\nex=" P.++ show (exInstr sys),
          "\nme=" P.++ show (stateMeInstr (sysState sys)),
          "\nwb=" P.++ show (stateWbInstr (sysState sys)),
          "\nexPc=" P.++ show (stateExPc (sysState sys)),
          " meAddr=" P.++ show (stateMeAddr (sysState sys)),
          "\nleak=" P.++ show (leakOf (archOfLeak sys)),
          "\nobs impl=" P.++ show obsI,
          "\nobs sim= " P.++ show obsL
        ]
    )
    (leakObligation wr wa sys)
  where
    (_, obsI) = implHop sys
    (_, obsL) = leakSimHop (proj sys)

-- Programs that address memory through registers ------------------------------

-- | Random programs whose loads, stores and @jalr@s go through base registers.
--
-- 'ProofSpec.genProg' bases every load and store on @x0@, so its addresses fit
-- the immediate and the simulator never has to park one. Here @x5@ and @x6@
-- are reserved as base registers: they are written only by @addi@ from @x0@,
-- with a word-aligned data address (below 'initPc') or the address of an
-- instruction of the program. Stores through them therefore also hit the code,
-- which raises store hazards; they store @x0@, so an overwritten instruction
-- decodes to @Nop DecodeFail@. Every access stays inside the harness's memory.
-- A @jalr@ off @x0@, whose target fits the immediate, is generated as well.
genCTProg :: Gen (Vec PROG_SIZE Word)
genCTProg = do
  n <- choose (2, 12)
  body <- P.mapM (genCTInstr n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Instruction.break
  pure (map encode (unsafeFromList instrs))
  where
    roundTrips i = case encode' i of
      Just w | decode' w == i -> i
      _ -> IType (Arith ADD) 0 0 0

genCTInstr :: Int -> Int -> Gen Instruction
genCTInstr n i =
  frequency
    [ (2, RType <$> genArith <*> genDst <*> genSrc <*> genSrc),
      (1, IType <$> (Arith <$> genArith) <*> genDst <*> genSrc <*> genSmallImm),
      (3, (\b a -> IType (Arith ADD) b 0 (fromIntegral a)) <$> genBase <*> genAddr),
      ( 3,
        (\size sign rd b off -> IType (Load size sign) rd b off)
          <$> genSize
          <*> elements [Signed, Unsigned]
          <*> genDst
          <*> genBase
          <*> genOff
      ),
      (3, (\size off b -> SType size off b 0) <$> genSize <*> genOff <*> genBase),
      (1, (\t -> SType Word (fromIntegral (codeAddr t)) 0 0) <$> choose (0, n)),
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
    genSize = elements [Byte, Half, Word]
    genSmallImm = fromIntegral <$> choose (0 :: Int, 15)
    -- Base registers, and everything else.
    genBase = elements [5, 6]
    genDst = elements [1, 2, 3, 4, 7]
    genSrc = elements [0, 1, 2, 3, 4, 5, 6, 7]
    genOff = (\k -> fromIntegral (k * 4)) <$> choose (0 :: Int, 2)
    codeAddr t = fromIntegral initPc + 4 * t :: Int
    genAddr =
      oneof
        [ (\k -> 4 * k) <$> choose (0 :: Int, 40),
          codeAddr <$> choose (0, n)
        ]

-- Coverage ----------------------------------------------------------------------

-- | A tag per hop: which driver case fired, and which leakage class was emitted.
walkTags :: Int -> Vec PROG_SIZE Word -> [(String, String)]
walkTags n prog = go n sys0 (archOfLeak sys0) (censor sys0)
  where
    sys0 = initSys prog

    go :: Int -> Sys -> IsaState -> ConcreteSim -> [(String, String)]
    go 0 _ _ _ = []
    go j sys isa ss
      | running sysI = tag : go (j - 1) sysI isa' ss'
      | otherwise = [tag]
      where
        l = leakOf isa
        tag = (driverCaseName sys, classTag l)
        (sysI, _) = implHop sys
        ((isa', ss'), _) = leakSimHop (isa, ss)

classTag :: L -> String
classTag l@(L c _) = base P.++ suffix
  where
    suffix :: String
    suffix = case parkSource l of
      Just _ -> "-parked"
      Nothing -> ""
    base :: String
    base = case c of
      CPlain -> "plain"
      CBranchTaken _ -> "branch-taken"
      CBranchNotTaken -> "branch-untaken"
      CJal _ -> "jal"
      CJalr _ -> "jalr"
      CLoad {} -> "load"
      CStore {} -> "store"
      CCall -> "ecall"
      CBreak -> "ebreak"
