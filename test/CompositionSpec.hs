{-# LANGUAGE PackageImports #-}

-- | The core behind a cache: what holds for any cache, and the programs and
-- traces the cache tests ("Cache4KSpec") are built from.
--
-- The constant-time leakage proof ("Proof.Core.Leakage.Solver") is about the core
-- attached to memory that answers every request in the next cycle. Behind a
-- cache a load that misses is answered only after the miss penalty, and in the
-- meantime the core stalls. The core's side of what lets the ideal-memory result
-- carry over is checked here: after a data read the core waits
-- ('Proof.CachedCore.Obligation.waitObligation'), and a stall cycle is a stutter step
-- ('Proof.CachedCore.Obligation.stallObligation').
module CompositionSpec
  ( compositionTests,
    sameModuloCtrl,
    coreDiff,
    ctTrace,
    genByteProg,
    genCTProg,
    genData,
    genSecretFreeProg,
    genWord,
    simHops,
    withData,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Core
import Data.Functor.Identity (Identity (..))
import Core.ISA (IsaState, StepG (..), isaStep)
import Core.Instruction
import Memory.Types
import Proof.CachedCore.Obligation (stallObligation, waitObligation)
import Proof.Core.Leakage.Model
import Proof.Core.Functional.Obligation (isaAt)
import Proof.Core.Leakage.Driver (simDriver)
import Proof.Core.Leakage.Simulator (SimInput, SimState, simCircuit, simInit)
import Proof.Core.Functional.Machine
import ProofSpec (genArbSys1)
import Core.RegFile (RegFileOps)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | Two systems equal in everything but the control lines, which
-- 'Core.withCtrlReset' overwrites before any stage reads them.
sameModuloCtrl :: Sys -> Sys -> Bool
sameModuloCtrl (Sys a ia ma) (Sys b ib mb) =
  a {stateCtrl = initCtrl} == b {stateCtrl = initCtrl}
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
      f "deExpInstr" stateDeExpInstr,
      f "exPc" stateExPc,
      f "ex" stateExInstr,
      f "me" stateMeInstr,
      f "meAddr" stateMeAddr,
      f "meRes" (runIdentity . stateMeRes),
      f "wb" stateWbInstr,
      f "wbRes" (runIdentity . stateWbRes),
      f "halt" stateHalt,
      f "loadInFlight" stateLoadInFlight,
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

-- | The ISA-level constant-time leakage trace of the first @n@ instructions.
ctTrace :: Int -> IsaState -> [Leak]
ctTrace 0 _ = []
ctTrace n a = leakOf a : case isaStep a of
  Next a' -> ctTrace (n - 1) a'
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

-- | Programs whose control flow and addresses never depend on loaded data.
--
-- As 'genCTProg', with @x5@ and @x6@ the base registers, set only
-- from constants; loads write @x1@ to @x4@ and @x7@, and branches compare only
-- @x0@, @x5@ and @x6@, so no loaded value reaches a branch or an address. Stores
-- store @x0@, so no loaded value reaches memory either.
genSecretFreeProg :: Gen (Vec PROG_SIZE Word)
genSecretFreeProg = do
  n <- choose (2, 12)
  body <- P.mapM (genOne n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Core.Instruction.break
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
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Core.Instruction.break
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

genWord :: Gen Word
genWord = oneof [elements [0, 1, 0xFF, 0x1234, 0x80000000, 0xDEADBEEF], fromIntegral <$> (arbitrary :: Gen Int)]

-- | The core's bus observations computed from the leakage alone, hop by hop, as
-- behind ideal memory: the leakage simulator ('Proof.Core.Leakage.Simulator.simCircuit')
-- run on the ISA's leakage trace.
--
-- The hops are those of 'ProofSpec.leakReport': the first gets no leakage, and
-- every later one gets @'leakOf' isa@ on its first cycle, after which the ISA
-- steps. A halted ISA stands still.
simHops :: Sys -> [[Obs]]
simHops sys0 = go Nothing (isaAt (stateFePc (sysCore sys0)) sys0) simInit
  where
    go :: SimInput -> IsaState -> SimState -> [[Obs]]
    go inp a sim =
      let (sim', os) = runSim (simDriver inp sim + 1) inp sim
          a' = case inp of
            Nothing -> a
            Just _ -> isaNext a
       in os : go (Just (leakOf a')) a' sim'
    -- The simulator for @n@ cycles; it gets @inp@ on the first only.
    runSim :: Int -> SimInput -> SimState -> (SimState, [Obs])
    runSim 0 _ sim = (sim, [])
    runSim n inp sim =
      let (sim1, o) = simCircuit sim inp
          (sim2, os) = runSim (n - 1) Nothing sim1
       in (sim2, o : os)
    isaNext a = case isaStep a of
      Next a' -> a'
      IsaHalted -> a

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
issuesDataRead s inp = case outMem (snd (circuit s inp)) of
  Just (MemAccess False _ _ Nothing) -> True
  _ -> False

compositionTests :: TestTree
compositionTests =
  testGroup
    "Core behind a cache"
    [ testProperty "patience: after a data read the core waits" $
        -- Arbitrary states, mostly with a load in the memory stage and none in
        -- flight, so that the cycle issues a data read.
        withMaxSuccess 5000 $ checkCoverage $
          forAllShow genArbSys1 (const "<state>") $ \(Sys s0 inp _, _, _) ->
            forAll genMeLoad $ \me ->
              let s = maybe s0 (\ir -> s0 {stateMeInstr = ir, stateLoadInFlight = False}) me
               in cover 50 (issuesDataRead s inp) "the cycle issues a data read" (waitObligation s inp),
      testProperty "a stall cycle is a stutter step, on arbitrary stalled states" $
        withMaxSuccess 5000 $
          forAllShow genArbSys1 (const "<state>") $ \(Sys st inp _, wr, _) ->
            isLoad (stateWbInstr st) ==>
              stallObligation wr st {stateLoadInFlight = True} inp {inputMemReady = False}
    ]
