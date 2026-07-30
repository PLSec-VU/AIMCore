-- | The @k = 2@ (three core cycle) inductive step, decomposed for Pantomime.
--
-- The pre-state is running and its execute instruction is constructed as
-- @decode' iw@ from a fresh symbolic word, exactly as in "VerifyK1Split".
-- The post-state has two possible shapes:
--
--   * jumps and steady pipeline stalls return to the running invariant case;
--   * ecall/ebreak flush the pipeline and enter a halted invariant case.
--
-- Running register-file obligations are split by top-level opcode, since that
-- was the dominant k=1 query. Halted obligations need only the SYSTEM opcode:
-- 'k2EnvWordIsSystem' proves that every decoded environment instruction lies
-- in that case.
module VerifyK2Split
  ( results,
  )
where

import ArrayRF
import Axioms (arrayAxioms)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Driver (driver, isEnvInstr, isJumpInstr, isMemInstr)
import Instruction
import Invariant (HaltConj (..), HaltKind (..), RunConj (..), noStoreAlias)
import LoggedPantomime (pantomime)
import Machine
import Obligation
  ( DecodeWordCase (..),
    K2Conj (..),
    K2EmptyCase (..),
    decodeWordCasesExhaustive,
    decodeWordCaseHolds,
    decodedEnvWordIsSystem,
    indStepObligation2AtWord,
    indStepObligation2AtWordCase,
    premise2NonRunningEmptyAt,
    premise2StoreHazardNoMemEmpty,
  )
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Arbitrary scalar core state, used for the cheap empty-case claims.
data K2State = K2State
  { k2FePc :: Address,
    k2DePc :: Address,
    k2ExPc :: Address,
    k2ExIr :: Instruction,
    k2MeIr :: Instruction,
    k2MeRes :: Word,
    k2MeAddr :: Address,
    k2WbIr :: Instruction,
    k2WbRes :: Word,
    k2Ctrl :: Core.Control Identity,
    k2Halt :: Maybe Core.HaltState,
    k2HaltPending :: Maybe Core.HaltState
  }

-- | Running-state scalars. Execute is deliberately absent: 'sysOfRun'
-- constructs it from the fresh symbolic instruction word.
data K2RunState = K2RunState
  { k2rFePc :: Address,
    k2rDePc :: Address,
    k2rExPc :: Address,
    k2rMeIr :: Instruction,
    k2rMeRes :: Word,
    k2rMeAddr :: Address,
    k2rWbIr :: Instruction,
    k2rWbRes :: Word,
    k2rCtrl :: Core.Control Identity,
    k2rHalt :: Maybe Core.HaltState,
    k2rHaltPending :: Maybe Core.HaltState
  }

data MemFields = MemFields
  { mfSize :: Size,
    mfSign :: Sign,
    mfRd :: RegIdx,
    mfRs1 :: RegIdx,
    mfRs2 :: RegIdx,
    mfImm :: Imm
  }

data MemStageCase = StageLoad | StageStore

data EffectClass = EffectWrite | EffectLoad | EffectStore | EffectQuiet

data WriteStageCase
  = WriteR
  | WriteIArith
  | WriteJ
  | WriteIJump
  | WriteU

data QuietStageCase
  = QuietBranch
  | QuietEnv
  | QuietNop

data PipeFields = PipeFields
  { pfArith :: Arith,
    pfRd :: RegIdx,
    pfRs1 :: RegIdx,
    pfRs2 :: RegIdx,
    pfImm :: Imm,
    pfSize :: Size,
    pfSign :: Sign,
    pfCmp :: Comparison,
    pfBImm :: BImm,
    pfUBase :: UBase,
    pfUImm :: UImm,
    pfJImm :: JImm,
    pfEnv :: Env,
    pfReason :: Reason4Stall
  }

memInstr :: MemStageCase -> MemFields -> Instruction
memInstr sc mf =
  case sc of
    StageLoad -> IType (Load (mfSize mf) (mfSign mf)) (mfRd mf) (mfRs1 mf) (mfImm mf)
    StageStore -> SType (mfSize mf) (mfImm mf) (mfRs1 mf) (mfRs2 mf)

effectInstr ::
  EffectClass ->
  WriteStageCase ->
  QuietStageCase ->
  PipeFields ->
  Instruction
effectInstr cls wc qc pf =
  case cls of
    EffectWrite ->
      case wc of
        WriteR -> RType (pfArith pf) (pfRd pf) (pfRs1 pf) (pfRs2 pf)
        WriteIArith ->
          IType (Arith (pfArith pf)) (pfRd pf) (pfRs1 pf) (pfImm pf)
        WriteJ -> JType (pfRd pf) (pfJImm pf)
        WriteIJump -> IType Jump (pfRd pf) (pfRs1 pf) (pfImm pf)
        WriteU -> UType (pfUBase pf) (pfRd pf) (pfUImm pf)
    EffectLoad ->
      IType (Load (pfSize pf) (pfSign pf)) (pfRd pf) (pfRs1 pf) (pfImm pf)
    EffectStore ->
      SType (pfSize pf) (pfImm pf) (pfRs1 pf) (pfRs2 pf)
    EffectQuiet ->
      case qc of
        QuietBranch ->
          BType (pfCmp pf) (pfBImm pf) (pfRs1 pf) (pfRs2 pf)
        QuietEnv ->
          IType (Env (pfEnv pf)) (pfRd pf) (pfRs1 pf) (pfImm pf)
        QuietNop -> Nop (pfReason pf)

sysOf ::
  K2State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  SysG RegArrF MemArr
sysOf ss i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = k2FePc ss,
            Core.stateDePc = k2DePc ss,
            Core.stateExPc = k2ExPc ss,
            Core.stateExInstr = k2ExIr ss,
            Core.stateMeInstr = k2MeIr ss,
            Core.stateMeRes = Identity (k2MeRes ss),
            Core.stateMeAddr = k2MeAddr ss,
            Core.stateWbInstr = k2WbIr ss,
            Core.stateWbRes = Identity (k2WbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = k2Ctrl ss,
            Core.stateHalt = k2Halt ss,
            Core.stateHaltPending = k2HaltPending ss
          },
      sysInput = i,
      sysMem = ma
    }

sysOfRun ::
  K2RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  SysG RegArrF MemArr
sysOfRun ss iw i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = k2rFePc ss,
            Core.stateDePc = k2rDePc ss,
            Core.stateExPc = k2rExPc ss,
            Core.stateExInstr = decode' iw,
            Core.stateMeInstr = k2rMeIr ss,
            Core.stateMeRes = Identity (k2rMeRes ss),
            Core.stateMeAddr = k2rMeAddr ss,
            Core.stateWbInstr = k2rWbIr ss,
            Core.stateWbRes = Identity (k2rWbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = k2rCtrl ss,
            Core.stateHalt = k2rHalt ss,
            Core.stateHaltPending = k2rHaltPending ss
          },
      sysInput = i,
      sysMem = ma
    }

runProp ::
  K2Conj ->
  K2RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runProp c ss iw i ra ma wr wa =
  Pantomime.boolean $
    indStepObligation2AtWord c iw wr wa (sysOfRun ss iw i ra ma)

runCaseProp ::
  DecodeWordCase ->
  K2Conj ->
  K2RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runCaseProp wc c ss iw i ra ma wr wa =
  Pantomime.boolean $
    indStepObligation2AtWordCase wc c iw wr wa (sysOfRun ss iw i ra ma)

-- | Strong opcode specialisation. Every word in a recognised opcode class is
-- uniquely @payload ++# opcode@, so this is exhaustive for that class, but the
-- seven opcode bits are concrete before symbolic execution expands 'decode''.
-- Merely assuming @slice d6 d0 iw == opcode@ leaves all 32 bits symbolic in
-- the expanded decoder and was still taking more than eight minutes for the
-- k=2 I-arithmetic register-file query.
runFixedOpcodeProp ::
  BitVector 7 ->
  K2Conj ->
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runFixedOpcodeProp opcode c ss payload i ra ma wr wa =
  runProp c ss (payload ++# opcode) i ra ma wr wa

runFixedOpcodeMemStagesProp ::
  MemStageCase ->
  MemStageCase ->
  BitVector 7 ->
  K2Conj ->
  K2RunState ->
  MemFields ->
  MemFields ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runFixedOpcodeMemStagesProp meCase wbCase opcode c ss mef wbf payload i ra ma wr wa =
  runProp c ss' (payload ++# opcode) i ra ma wr wa
  where
    ss' =
      ss
        { k2rMeIr = memInstr meCase mef,
          k2rWbIr = memInstr wbCase wbf
        }

runFixedOpcodeFunct3MemStagesProp ::
  MemStageCase ->
  MemStageCase ->
  BitVector 7 ->
  K2Conj ->
  BitVector 3 ->
  K2RunState ->
  MemFields ->
  MemFields ->
  BitVector 17 ->
  BitVector 5 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runFixedOpcodeFunct3MemStagesProp meCase wbCase opcode c funct3 ss mef wbf upper lower i ra ma wr wa =
  runProp c ss' iw i ra ma wr wa
  where
    iw = upper ++# funct3 ++# lower ++# opcode
    ss' =
      ss
        { k2rMeIr = memInstr meCase mef,
          k2rWbIr = memInstr wbCase wbf
        }

-- | Fix the two older pipeline stages to one of the four classes that matter
-- to the register-file flush.  A jump in execute makes @driver == 2@
-- independently of those stages, so leaving both as arbitrary 'Instruction's
-- made the JALR/RF query diverge even after the execute opcode was fixed.
runFixedOpcodeEffectsProp ::
  BitVector 7 ->
  EffectClass ->
  EffectClass ->
  K2Conj ->
  K2RunState ->
  WriteStageCase ->
  QuietStageCase ->
  PipeFields ->
  WriteStageCase ->
  QuietStageCase ->
  PipeFields ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runFixedOpcodeEffectsProp opcode meClass wbClass c ss meW meQ mef wbW wbQ wbf payload i ra ma wr wa =
  runProp c ss' (payload ++# opcode) i ra ma wr wa
  where
    ss' =
      ss
        { k2rMeIr = effectInstr meClass meW meQ mef,
          k2rWbIr = effectInstr wbClass wbW wbQ wbf
        }

-- | The load/quiet JALR cell still exceeded five minutes when the quiet
-- constructor was symbolic.  Pin that final three-way choice before symbolic
-- execution; the unused write/quiet selectors disappear from the query too.
runJalrLoadQuietProp ::
  QuietStageCase ->
  K2RunState ->
  PipeFields ->
  PipeFields ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runJalrLoadQuietProp wbQ ss mef wbf payload i ra ma wr wa =
  runProp (K2Run RunFlushRf) ss' (payload ++# (0b110_0111 :: BitVector 7)) i ra ma wr wa
  where
    ss' =
      ss
        { k2rMeIr = effectInstr EffectLoad WriteR QuietNop mef,
          k2rWbIr = effectInstr EffectQuiet WriteR wbQ wbf
        }

runJalrStoreQuietProp ::
  QuietStageCase ->
  K2RunState ->
  PipeFields ->
  PipeFields ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runJalrStoreQuietProp wbQ ss mef wbf payload i ra ma wr wa =
  runProp (K2Run RunFlushRf) ss' (payload ++# (0b110_0111 :: BitVector 7)) i ra ma wr wa
  where
    ss' =
      ss
        { k2rMeIr = effectInstr EffectStore WriteR QuietNop mef,
          k2rWbIr = effectInstr EffectQuiet WriteR wbQ wbf
        }

runJalrLoadWriteProp ::
  WriteStageCase ->
  K2RunState ->
  PipeFields ->
  PipeFields ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runJalrLoadWriteProp wbW ss mef wbf payload i ra ma wr wa =
  runProp (K2Run RunFlushRf) ss' (payload ++# (0b110_0111 :: BitVector 7)) i ra ma wr wa
  where
    ss' =
      ss
        { k2rMeIr = effectInstr EffectLoad WriteR QuietNop mef,
          k2rWbIr = effectInstr EffectWrite wbW QuietNop wbf
        }

runCaseMemStagesProp ::
  DecodeWordCase ->
  MemStageCase ->
  MemStageCase ->
  K2Conj ->
  K2RunState ->
  MemFields ->
  MemFields ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runCaseMemStagesProp wc meCase wbCase c ss mef wbf iw i ra ma wr wa =
  runCaseProp wc c ss' iw i ra ma wr wa
  where
    ss' =
      ss
        { k2rMeIr = memInstr meCase mef,
          k2rWbIr = memInstr wbCase wbf
        }

nonJumpDriverWbNonMemEmptyProp ::
  BitVector 7 ->
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
nonJumpDriverWbNonMemEmptyProp opcode ss payload i ra ma =
  Pantomime.boolean $
    not
      ( driver sys == 2
          && noStoreAlias sys
          && not (isMemInstr (Core.stateWbInstr (sysState sys)))
      )
  where
    iw = payload ++# opcode
    sys = sysOfRun ss iw i ra ma

nonJumpDriverMeNonMemEmptyProp ::
  BitVector 7 ->
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
nonJumpDriverMeNonMemEmptyProp opcode ss payload i ra ma =
  Pantomime.boolean $
    not
      ( driver sys == 2
          && noStoreAlias sys
          && isMemInstr (Core.stateWbInstr (sysState sys))
          && not (isMemInstr (Core.stateMeInstr (sysState sys)))
      )
  where
    iw = payload ++# opcode
    sys = sysOfRun ss iw i ra ma

systemEnvRunProp ::
  K2Conj ->
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
systemEnvRunProp c ss payload i ra ma wr wa =
  Pantomime.boolean $
    not (isEnvInstr (decode' iw))
      || indStepObligation2AtWord c iw wr wa sys
  where
    iw = payload ++# (0b111_0011 :: BitVector 7)
    sys = sysOfRun ss iw i ra ma

systemNonEnvDriverWbNonMemEmptyProp ::
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
systemNonEnvDriverWbNonMemEmptyProp ss payload i ra ma =
  Pantomime.boolean $
    not
      ( driver sys == 2
          && noStoreAlias sys
          && not (isEnvInstr (exInstr sys))
          && not (isMemInstr (Core.stateWbInstr (sysState sys)))
      )
  where
    iw = payload ++# (0b111_0011 :: BitVector 7)
    sys = sysOfRun ss iw i ra ma

systemNonEnvDriverMeNonMemEmptyProp ::
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
systemNonEnvDriverMeNonMemEmptyProp ss payload i ra ma =
  Pantomime.boolean $
    not
      ( driver sys == 2
          && noStoreAlias sys
          && not (isEnvInstr (exInstr sys))
          && isMemInstr (Core.stateWbInstr (sysState sys))
          && not (isMemInstr (Core.stateMeInstr (sysState sys)))
      )
  where
    iw = payload ++# (0b111_0011 :: BitVector 7)
    sys = sysOfRun ss iw i ra ma

branchTakenRunProp ::
  K2Conj ->
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
branchTakenRunProp c ss payload i ra ma wr wa =
  Pantomime.boolean $
    not (isJumpInstr sys)
      || indStepObligation2AtWord c iw wr wa sys
  where
    iw = payload ++# (0b110_0011 :: BitVector 7)
    sys = sysOfRun ss iw i ra ma

runBranchTakenFunct3Prop ::
  BitVector 3 ->
  K2RunState ->
  BitVector 17 ->
  BitVector 5 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runBranchTakenFunct3Prop funct3 ss upper lower i ra ma wr wa =
  Pantomime.boolean $
    not (isJumpInstr sys)
      || indStepObligation2AtWord (K2Run RunFlushRf) iw wr wa sys
  where
    iw =
      upper
        ++# funct3
        ++# lower
        ++# (0b110_0011 :: BitVector 7)
    sys = sysOfRun ss iw i ra ma

branchFunct3CoveredProp :: Word -> Pantomime.Bool
branchFunct3CoveredProp iw =
  Pantomime.boolean $
    case decode' iw of
      BType {} ->
        funct3 == 0
          || funct3 == 1
          || funct3 == 4
          || funct3 == 5
          || funct3 == 6
          || funct3 == 7
      _ -> True
  where
    funct3 = slice d14 d12 iw

runBranchTakenEffectsProp ::
  EffectClass ->
  EffectClass ->
  K2Conj ->
  K2RunState ->
  WriteStageCase ->
  QuietStageCase ->
  PipeFields ->
  WriteStageCase ->
  QuietStageCase ->
  PipeFields ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runBranchTakenEffectsProp meClass wbClass c ss meW meQ mef wbW wbQ wbf payload i ra ma wr wa =
  Pantomime.boolean $
    not (isJumpInstr sys)
      || indStepObligation2AtWord c iw wr wa sys
  where
    iw = payload ++# (0b110_0011 :: BitVector 7)
    ss' =
      ss
        { k2rMeIr = effectInstr meClass meW meQ mef,
          k2rWbIr = effectInstr wbClass wbW wbQ wbf
        }
    sys = sysOfRun ss' iw i ra ma

type BranchWritePairProp =
  K2RunState ->
  PipeFields ->
  PipeFields ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool

runBranchTakenWritePairProp ::
  WriteStageCase -> WriteStageCase -> BranchWritePairProp
runBranchTakenWritePairProp meW wbW ss mef wbf payload i ra ma wr wa =
  Pantomime.boolean $
    not (isJumpInstr sys)
      || indStepObligation2AtWord (K2Run RunFlushRf) iw wr wa sys
  where
    iw = payload ++# (0b110_0011 :: BitVector 7)
    ss' =
      ss
        { k2rMeIr = effectInstr EffectWrite meW QuietNop mef,
          k2rWbIr = effectInstr EffectWrite wbW QuietNop wbf
        }
    sys = sysOfRun ss' iw i ra ma

effectClassesExhaustiveProp :: Instruction -> Pantomime.Bool
effectClassesExhaustiveProp ir =
  Pantomime.boolean $
    case ir of
      RType {} -> True
      IType (Arith _) _ _ _ -> True
      IType (Load _ _) _ _ _ -> True
      SType {} -> True
      BType {} -> True
      JType {} -> True
      IType Jump _ _ _ -> True
      UType {} -> True
      IType (Env _) _ _ _ -> True
      Nop {} -> True

branchNonTakenDriverWbNonMemEmptyProp ::
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
branchNonTakenDriverWbNonMemEmptyProp ss payload i ra ma =
  Pantomime.boolean $
    not
      ( driver sys == 2
          && noStoreAlias sys
          && not (isJumpInstr sys)
          && not (isMemInstr (Core.stateWbInstr (sysState sys)))
      )
  where
    iw = payload ++# (0b110_0011 :: BitVector 7)
    sys = sysOfRun ss iw i ra ma

branchNonTakenDriverMeNonMemEmptyProp ::
  K2RunState ->
  BitVector 25 ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
branchNonTakenDriverMeNonMemEmptyProp ss payload i ra ma =
  Pantomime.boolean $
    not
      ( driver sys == 2
          && noStoreAlias sys
          && not (isJumpInstr sys)
          && isMemInstr (Core.stateWbInstr (sysState sys))
          && not (isMemInstr (Core.stateMeInstr (sysState sys)))
      )
  where
    iw = payload ++# (0b110_0011 :: BitVector 7)
    sys = sysOfRun ss iw i ra ma

otherDriverWbNonMemEmptyProp ::
  K2RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
otherDriverWbNonMemEmptyProp ss iw i ra ma =
  Pantomime.boolean $
    not
      ( decodeWordCaseHolds WordOther iw
          && driver sys == 2
          && noStoreAlias sys
          && not (isMemInstr (Core.stateWbInstr (sysState sys)))
      )
  where
    sys = sysOfRun ss iw i ra ma

otherDriverMeNonMemEmptyProp ::
  K2RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
otherDriverMeNonMemEmptyProp ss iw i ra ma =
  Pantomime.boolean $
    not
      ( decodeWordCaseHolds WordOther iw
          && driver sys == 2
          && noStoreAlias sys
          && isMemInstr (Core.stateWbInstr (sysState sys))
          && not (isMemInstr (Core.stateMeInstr (sysState sys)))
      )
  where
    sys = sysOfRun ss iw i ra ma

emptyProp ::
  K2EmptyCase ->
  K2State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
emptyProp ec ss i ra ma wr wa =
  Pantomime.boolean $
    premise2NonRunningEmptyAt ec wr wa (sysOf ss i ra ma)

storeEmptyProp ::
  K2State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  Pantomime.Bool
storeEmptyProp ss i ra ma =
  Pantomime.boolean $
    premise2StoreHazardNoMemEmpty (sysOf ss i ra ma)

-- Empty pre-state/driver cases -------------------------------------------------

{-# ANN k2EmptyStartup (Theory arrayAxioms) #-}
k2EmptyStartup :: K2State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2EmptyStartup = emptyProp K2EmptyStartup

{-# ANN k2EmptyHaltedBreak (Theory arrayAxioms) #-}
k2EmptyHaltedBreak :: K2State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2EmptyHaltedBreak = emptyProp K2EmptyHaltedBreak

{-# ANN k2EmptyHaltedCall (Theory arrayAxioms) #-}
k2EmptyHaltedCall :: K2State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2EmptyHaltedCall = emptyProp K2EmptyHaltedCall

{-# ANN k2EmptyStoreHazardNoMem (Theory arrayAxioms) #-}
k2EmptyStoreHazardNoMem :: K2State -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2EmptyStoreHazardNoMem = storeEmptyProp

-- Decoder coverage facts ------------------------------------------------------

{-# ANN k2WordCasesExhaustive (Theory arrayAxioms) #-}
k2WordCasesExhaustive :: Word -> Pantomime.Bool
k2WordCasesExhaustive iw =
  Pantomime.boolean $ decodeWordCasesExhaustive iw

{-# ANN k2EnvWordIsSystem (Theory arrayAxioms) #-}
k2EnvWordIsSystem :: Word -> Pantomime.Bool
k2EnvWordIsSystem iw =
  Pantomime.boolean $ decodedEnvWordIsSystem iw

-- Formerly the over-budget JALR store/quiet cell.  Keep its refinement near
-- the front so regressions are found before the routine opcode matrix.
{-# ANN k2RunFlushRfJalrSQBranch (Theory arrayAxioms) #-}
k2RunFlushRfJalrSQBranch, k2RunFlushRfJalrSQEnv, k2RunFlushRfJalrSQNop :: K2RunState -> PipeFields -> PipeFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfJalrSQBranch = runJalrStoreQuietProp QuietBranch
{-# ANN k2RunFlushRfJalrSQEnv (Theory arrayAxioms) #-}
k2RunFlushRfJalrSQEnv = runJalrStoreQuietProp QuietEnv
{-# ANN k2RunFlushRfJalrSQNop (Theory arrayAxioms) #-}
k2RunFlushRfJalrSQNop = runJalrStoreQuietProp QuietNop

-- The load/write cell was solver-sensitive (1m25s in one run, then >36m).
-- Fixing the writeback constructor removes the remaining five-way ADT mux.
{-# ANN k2RunFlushRfJalrLWR (Theory arrayAxioms) #-}
k2RunFlushRfJalrLWR, k2RunFlushRfJalrLWI, k2RunFlushRfJalrLWJ, k2RunFlushRfJalrLWX, k2RunFlushRfJalrLWU :: K2RunState -> PipeFields -> PipeFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfJalrLWR = runJalrLoadWriteProp WriteR
{-# ANN k2RunFlushRfJalrLWI (Theory arrayAxioms) #-}
k2RunFlushRfJalrLWI = runJalrLoadWriteProp WriteIArith
{-# ANN k2RunFlushRfJalrLWJ (Theory arrayAxioms) #-}
k2RunFlushRfJalrLWJ = runJalrLoadWriteProp WriteJ
{-# ANN k2RunFlushRfJalrLWX (Theory arrayAxioms) #-}
k2RunFlushRfJalrLWX = runJalrLoadWriteProp WriteIJump
{-# ANN k2RunFlushRfJalrLWU (Theory arrayAxioms) #-}
k2RunFlushRfJalrLWU = runJalrLoadWriteProp WriteU

-- For a non-jump execute instruction at driver 2, the existing
-- @k2LoadDriver*NonMemEmpty@ obligations force both older stages to be memory
-- instructions.  Split the remaining four constructor pairs here.
{-# ANN k2RunStructLoadLoadLoad (Theory arrayAxioms) #-}
k2RunStructLoadLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructLoadLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b000_0011 (K2Run RunStruct)
{-# ANN k2RunStructLoadLoadStoreF0 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF0, k2RunStructLoadLoadStoreF1, k2RunStructLoadLoadStoreF2, k2RunStructLoadLoadStoreF3, k2RunStructLoadLoadStoreF4, k2RunStructLoadLoadStoreF5, k2RunStructLoadLoadStoreF6, k2RunStructLoadLoadStoreF7 :: K2RunState -> MemFields -> MemFields -> BitVector 17 -> BitVector 5 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructLoadLoadStoreF0 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 0
{-# ANN k2RunStructLoadLoadStoreF1 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF1 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 1
{-# ANN k2RunStructLoadLoadStoreF2 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF2 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 2
{-# ANN k2RunStructLoadLoadStoreF3 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF3 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 3
{-# ANN k2RunStructLoadLoadStoreF4 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF4 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 4
{-# ANN k2RunStructLoadLoadStoreF5 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF5 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 5
{-# ANN k2RunStructLoadLoadStoreF6 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF6 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 6
{-# ANN k2RunStructLoadLoadStoreF7 (Theory arrayAxioms) #-}
k2RunStructLoadLoadStoreF7 = runFixedOpcodeFunct3MemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunStruct) 7
{-# ANN k2RunStructLoadStoreLoad (Theory arrayAxioms) #-}
k2RunStructLoadStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructLoadStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b000_0011 (K2Run RunStruct)
{-# ANN k2RunStructLoadStoreStore (Theory arrayAxioms) #-}
k2RunStructLoadStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructLoadStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b000_0011 (K2Run RunStruct)

-- Running post-state ----------------------------------------------------------

{-# ANN k2RunStructR (Theory arrayAxioms) #-}
k2RunStructR :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructR = runFixedOpcodeProp 0b011_0011 (K2Run RunStruct)

{-# ANN k2RunStructIArith (Theory arrayAxioms) #-}
k2RunStructIArith :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructIArith = runFixedOpcodeProp 0b001_0011 (K2Run RunStruct)

{-# ANN k2RunStructJalr (Theory arrayAxioms) #-}
k2RunStructJalr :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructJalr = runFixedOpcodeProp 0b110_0111 (K2Run RunStruct)

{-# ANN k2RunStructSystem (Theory arrayAxioms) #-}
k2RunStructSystem :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructSystem = runFixedOpcodeProp 0b111_0011 (K2Run RunStruct)

{-# ANN k2RunStructStore (Theory arrayAxioms) #-}
k2RunStructStore :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructStore = runFixedOpcodeProp 0b010_0011 (K2Run RunStruct)

{-# ANN k2RunStructBranch (Theory arrayAxioms) #-}
k2RunStructBranch :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructBranch = runFixedOpcodeProp 0b110_0011 (K2Run RunStruct)

{-# ANN k2RunStructLui (Theory arrayAxioms) #-}
k2RunStructLui :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructLui = runFixedOpcodeProp 0b011_0111 (K2Run RunStruct)

{-# ANN k2RunStructAuipc (Theory arrayAxioms) #-}
k2RunStructAuipc :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructAuipc = runFixedOpcodeProp 0b001_0111 (K2Run RunStruct)

{-# ANN k2RunStructJal (Theory arrayAxioms) #-}
k2RunStructJal :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructJal = runFixedOpcodeProp 0b110_1111 (K2Run RunStruct)

{-# ANN k2RunStructOther (Theory arrayAxioms) #-}
k2RunStructOther :: K2RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunStructOther = runCaseProp WordOther (K2Run RunStruct)

{-# ANN k2RunFlushRfRLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfRLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfRLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b011_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfRLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfRLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfRLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b011_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfRStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfRStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfRStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b011_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfRStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfRStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfRStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b011_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfIArithLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfIArithLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfIArithLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b001_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfIArithLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfIArithLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfIArithLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b001_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfIArithStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfIArithStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfIArithStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b001_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfIArithStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfIArithStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfIArithStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b001_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLoadLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfLoadLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLoadLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b000_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLoadLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfLoadLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLoadLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b000_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLoadStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfLoadStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLoadStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b000_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLoadStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfLoadStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLoadStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b000_0011 (K2Run RunFlushRf)

-- JALR makes the driver take three cycles before it inspects the older stages.
-- Split those stages by their register-file effect: the former single query
-- spent more than sixteen minutes in Z3 on 2026-07-29.  The four classes are
-- exhaustive by 'k2EffectClassesExhaustive'.
{-# ANN k2RunFlushRfJalrWW (Theory arrayAxioms) #-}
k2RunFlushRfJalrWW, k2RunFlushRfJalrWL, k2RunFlushRfJalrWS, k2RunFlushRfJalrWQ, k2RunFlushRfJalrLL, k2RunFlushRfJalrLS, k2RunFlushRfJalrSW, k2RunFlushRfJalrSL, k2RunFlushRfJalrSS, k2RunFlushRfJalrQW, k2RunFlushRfJalrQL, k2RunFlushRfJalrQS, k2RunFlushRfJalrQQ :: K2RunState -> WriteStageCase -> QuietStageCase -> PipeFields -> WriteStageCase -> QuietStageCase -> PipeFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfJalrWW = runFixedOpcodeEffectsProp 0b110_0111 EffectWrite EffectWrite (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrWL (Theory arrayAxioms) #-}
k2RunFlushRfJalrWL = runFixedOpcodeEffectsProp 0b110_0111 EffectWrite EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrWS (Theory arrayAxioms) #-}
k2RunFlushRfJalrWS = runFixedOpcodeEffectsProp 0b110_0111 EffectWrite EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrWQ (Theory arrayAxioms) #-}
k2RunFlushRfJalrWQ = runFixedOpcodeEffectsProp 0b110_0111 EffectWrite EffectQuiet (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrLL (Theory arrayAxioms) #-}
k2RunFlushRfJalrLL = runFixedOpcodeEffectsProp 0b110_0111 EffectLoad EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrLS (Theory arrayAxioms) #-}
k2RunFlushRfJalrLS = runFixedOpcodeEffectsProp 0b110_0111 EffectLoad EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrLQBranch (Theory arrayAxioms) #-}
k2RunFlushRfJalrLQBranch, k2RunFlushRfJalrLQEnv, k2RunFlushRfJalrLQNop :: K2RunState -> PipeFields -> PipeFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfJalrLQBranch = runJalrLoadQuietProp QuietBranch
{-# ANN k2RunFlushRfJalrLQEnv (Theory arrayAxioms) #-}
k2RunFlushRfJalrLQEnv = runJalrLoadQuietProp QuietEnv
{-# ANN k2RunFlushRfJalrLQNop (Theory arrayAxioms) #-}
k2RunFlushRfJalrLQNop = runJalrLoadQuietProp QuietNop
{-# ANN k2RunFlushRfJalrSW (Theory arrayAxioms) #-}
k2RunFlushRfJalrSW = runFixedOpcodeEffectsProp 0b110_0111 EffectStore EffectWrite (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrSL (Theory arrayAxioms) #-}
k2RunFlushRfJalrSL = runFixedOpcodeEffectsProp 0b110_0111 EffectStore EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrSS (Theory arrayAxioms) #-}
k2RunFlushRfJalrSS = runFixedOpcodeEffectsProp 0b110_0111 EffectStore EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrQW (Theory arrayAxioms) #-}
k2RunFlushRfJalrQW = runFixedOpcodeEffectsProp 0b110_0111 EffectQuiet EffectWrite (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrQL (Theory arrayAxioms) #-}
k2RunFlushRfJalrQL = runFixedOpcodeEffectsProp 0b110_0111 EffectQuiet EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrQS (Theory arrayAxioms) #-}
k2RunFlushRfJalrQS = runFixedOpcodeEffectsProp 0b110_0111 EffectQuiet EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfJalrQQ (Theory arrayAxioms) #-}
k2RunFlushRfJalrQQ = runFixedOpcodeEffectsProp 0b110_0111 EffectQuiet EffectQuiet (K2Run RunFlushRf)

{-# ANN k2RunFlushRfSystemEnv (Theory arrayAxioms) #-}
k2RunFlushRfSystemEnv :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfSystemEnv = systemEnvRunProp (K2Run RunFlushRf)

{-# ANN k2RunFlushRfSystemLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfSystemLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfSystemLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b111_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfSystemLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfSystemLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfSystemLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b111_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfSystemStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfSystemStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfSystemStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b111_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfSystemStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfSystemStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfSystemStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b111_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfStoreLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfStoreLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfStoreLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b010_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfStoreLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfStoreLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfStoreLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b010_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfStoreStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfStoreStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfStoreStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b010_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfStoreStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfStoreStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfStoreStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b010_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfBranchTaken (Theory arrayAxioms) #-}
k2RunFlushRfBranchTaken :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfBranchTaken = branchTakenRunProp (K2Run RunFlushRf)

{-# ANN k2BranchFunct3Covered (Theory arrayAxioms) #-}
k2BranchFunct3Covered :: Word -> Pantomime.Bool
k2BranchFunct3Covered = branchFunct3CoveredProp

{-# ANN k2RunFlushRfBranchTakenEq (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenEq, k2RunFlushRfBranchTakenNe, k2RunFlushRfBranchTakenLt, k2RunFlushRfBranchTakenGe, k2RunFlushRfBranchTakenLtu, k2RunFlushRfBranchTakenGeu :: K2RunState -> BitVector 17 -> BitVector 5 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfBranchTakenEq = runBranchTakenFunct3Prop 0
{-# ANN k2RunFlushRfBranchTakenNe (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenNe = runBranchTakenFunct3Prop 1
{-# ANN k2RunFlushRfBranchTakenLt (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenLt = runBranchTakenFunct3Prop 4
{-# ANN k2RunFlushRfBranchTakenGe (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenGe = runBranchTakenFunct3Prop 5
{-# ANN k2RunFlushRfBranchTakenLtu (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenLtu = runBranchTakenFunct3Prop 6
{-# ANN k2RunFlushRfBranchTakenGeu (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenGeu = runBranchTakenFunct3Prop 7

{-# ANN k2EffectClassesExhaustive (Theory arrayAxioms) #-}
k2EffectClassesExhaustive :: Instruction -> Pantomime.Bool
k2EffectClassesExhaustive = effectClassesExhaustiveProp

{-# ANN k2RunFlushRfBranchTakenWW (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenWW, k2RunFlushRfBranchTakenWL, k2RunFlushRfBranchTakenWS, k2RunFlushRfBranchTakenWQ, k2RunFlushRfBranchTakenLW, k2RunFlushRfBranchTakenLL, k2RunFlushRfBranchTakenLS, k2RunFlushRfBranchTakenLQ, k2RunFlushRfBranchTakenSW, k2RunFlushRfBranchTakenSL, k2RunFlushRfBranchTakenSS, k2RunFlushRfBranchTakenSQ, k2RunFlushRfBranchTakenQW, k2RunFlushRfBranchTakenQL, k2RunFlushRfBranchTakenQS, k2RunFlushRfBranchTakenQQ :: K2RunState -> WriteStageCase -> QuietStageCase -> PipeFields -> WriteStageCase -> QuietStageCase -> PipeFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfBranchTakenWW = runBranchTakenEffectsProp EffectWrite EffectWrite (K2Run RunFlushRf)

{-# ANN k2RunFlushRfBranchTakenRR (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenRR, k2RunFlushRfBranchTakenRI, k2RunFlushRfBranchTakenRJ, k2RunFlushRfBranchTakenRX, k2RunFlushRfBranchTakenRU, k2RunFlushRfBranchTakenIR, k2RunFlushRfBranchTakenII, k2RunFlushRfBranchTakenIJ, k2RunFlushRfBranchTakenIX, k2RunFlushRfBranchTakenIU, k2RunFlushRfBranchTakenJR, k2RunFlushRfBranchTakenJI, k2RunFlushRfBranchTakenJJ, k2RunFlushRfBranchTakenJX, k2RunFlushRfBranchTakenJU, k2RunFlushRfBranchTakenXR, k2RunFlushRfBranchTakenXI, k2RunFlushRfBranchTakenXJ, k2RunFlushRfBranchTakenXX, k2RunFlushRfBranchTakenXU, k2RunFlushRfBranchTakenUR, k2RunFlushRfBranchTakenUI, k2RunFlushRfBranchTakenUJ, k2RunFlushRfBranchTakenUX, k2RunFlushRfBranchTakenUU :: BranchWritePairProp
k2RunFlushRfBranchTakenRR = runBranchTakenWritePairProp WriteR WriteR
{-# ANN k2RunFlushRfBranchTakenRI (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenRI = runBranchTakenWritePairProp WriteR WriteIArith
{-# ANN k2RunFlushRfBranchTakenRJ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenRJ = runBranchTakenWritePairProp WriteR WriteJ
{-# ANN k2RunFlushRfBranchTakenRX (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenRX = runBranchTakenWritePairProp WriteR WriteIJump
{-# ANN k2RunFlushRfBranchTakenRU (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenRU = runBranchTakenWritePairProp WriteR WriteU
{-# ANN k2RunFlushRfBranchTakenIR (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenIR = runBranchTakenWritePairProp WriteIArith WriteR
{-# ANN k2RunFlushRfBranchTakenII (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenII = runBranchTakenWritePairProp WriteIArith WriteIArith
{-# ANN k2RunFlushRfBranchTakenIJ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenIJ = runBranchTakenWritePairProp WriteIArith WriteJ
{-# ANN k2RunFlushRfBranchTakenIX (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenIX = runBranchTakenWritePairProp WriteIArith WriteIJump
{-# ANN k2RunFlushRfBranchTakenIU (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenIU = runBranchTakenWritePairProp WriteIArith WriteU
{-# ANN k2RunFlushRfBranchTakenJR (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenJR = runBranchTakenWritePairProp WriteJ WriteR
{-# ANN k2RunFlushRfBranchTakenJI (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenJI = runBranchTakenWritePairProp WriteJ WriteIArith
{-# ANN k2RunFlushRfBranchTakenJJ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenJJ = runBranchTakenWritePairProp WriteJ WriteJ
{-# ANN k2RunFlushRfBranchTakenJX (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenJX = runBranchTakenWritePairProp WriteJ WriteIJump
{-# ANN k2RunFlushRfBranchTakenJU (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenJU = runBranchTakenWritePairProp WriteJ WriteU
{-# ANN k2RunFlushRfBranchTakenXR (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenXR = runBranchTakenWritePairProp WriteIJump WriteR
{-# ANN k2RunFlushRfBranchTakenXI (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenXI = runBranchTakenWritePairProp WriteIJump WriteIArith
{-# ANN k2RunFlushRfBranchTakenXJ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenXJ = runBranchTakenWritePairProp WriteIJump WriteJ
{-# ANN k2RunFlushRfBranchTakenXX (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenXX = runBranchTakenWritePairProp WriteIJump WriteIJump
{-# ANN k2RunFlushRfBranchTakenXU (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenXU = runBranchTakenWritePairProp WriteIJump WriteU
{-# ANN k2RunFlushRfBranchTakenUR (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenUR = runBranchTakenWritePairProp WriteU WriteR
{-# ANN k2RunFlushRfBranchTakenUI (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenUI = runBranchTakenWritePairProp WriteU WriteIArith
{-# ANN k2RunFlushRfBranchTakenUJ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenUJ = runBranchTakenWritePairProp WriteU WriteJ
{-# ANN k2RunFlushRfBranchTakenUX (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenUX = runBranchTakenWritePairProp WriteU WriteIJump
{-# ANN k2RunFlushRfBranchTakenUU (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenUU = runBranchTakenWritePairProp WriteU WriteU
{-# ANN k2RunFlushRfBranchTakenWL (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenWL = runBranchTakenEffectsProp EffectWrite EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenWS (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenWS = runBranchTakenEffectsProp EffectWrite EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenWQ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenWQ = runBranchTakenEffectsProp EffectWrite EffectQuiet (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenLW (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenLW = runBranchTakenEffectsProp EffectLoad EffectWrite (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenLL (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenLL = runBranchTakenEffectsProp EffectLoad EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenLS (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenLS = runBranchTakenEffectsProp EffectLoad EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenLQ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenLQ = runBranchTakenEffectsProp EffectLoad EffectQuiet (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenSW (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenSW = runBranchTakenEffectsProp EffectStore EffectWrite (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenSL (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenSL = runBranchTakenEffectsProp EffectStore EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenSS (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenSS = runBranchTakenEffectsProp EffectStore EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenSQ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenSQ = runBranchTakenEffectsProp EffectStore EffectQuiet (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenQW (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenQW = runBranchTakenEffectsProp EffectQuiet EffectWrite (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenQL (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenQL = runBranchTakenEffectsProp EffectQuiet EffectLoad (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenQS (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenQS = runBranchTakenEffectsProp EffectQuiet EffectStore (K2Run RunFlushRf)
{-# ANN k2RunFlushRfBranchTakenQQ (Theory arrayAxioms) #-}
k2RunFlushRfBranchTakenQQ = runBranchTakenEffectsProp EffectQuiet EffectQuiet (K2Run RunFlushRf)

{-# ANN k2RunFlushRfBranchLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfBranchLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfBranchLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b110_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfBranchLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfBranchLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfBranchLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b110_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfBranchStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfBranchStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfBranchStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b110_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfBranchStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfBranchStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfBranchStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b110_0011 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLuiLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfLuiLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLuiLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b011_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLuiLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfLuiLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLuiLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b011_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLuiStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfLuiStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLuiStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b011_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfLuiStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfLuiStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfLuiStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b011_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfAuipcLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfAuipcLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfAuipcLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b001_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfAuipcLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfAuipcLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfAuipcLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b001_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfAuipcStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfAuipcStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfAuipcStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b001_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfAuipcStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfAuipcStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfAuipcStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b001_0111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfJal (Theory arrayAxioms) #-}
k2RunFlushRfJal :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfJal = runFixedOpcodeProp 0b110_1111 (K2Run RunFlushRf)

{-# ANN k2RunFlushRfOtherLoadLoad (Theory arrayAxioms) #-}
k2RunFlushRfOtherLoadLoad :: K2RunState -> MemFields -> MemFields -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfOtherLoadLoad = runCaseMemStagesProp WordOther StageLoad StageLoad (K2Run RunFlushRf)

{-# ANN k2RunFlushRfOtherLoadStore (Theory arrayAxioms) #-}
k2RunFlushRfOtherLoadStore :: K2RunState -> MemFields -> MemFields -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfOtherLoadStore = runCaseMemStagesProp WordOther StageLoad StageStore (K2Run RunFlushRf)

{-# ANN k2RunFlushRfOtherStoreLoad (Theory arrayAxioms) #-}
k2RunFlushRfOtherStoreLoad :: K2RunState -> MemFields -> MemFields -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfOtherStoreLoad = runCaseMemStagesProp WordOther StageStore StageLoad (K2Run RunFlushRf)

{-# ANN k2RunFlushRfOtherStoreStore (Theory arrayAxioms) #-}
k2RunFlushRfOtherStoreStore :: K2RunState -> MemFields -> MemFields -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushRfOtherStoreStore = runCaseMemStagesProp WordOther StageStore StageStore (K2Run RunFlushRf)

-- The three-cycle memory and decode conclusions also need opcode
-- specialisation. Their unsplit forms exceeded five minutes even though the
-- corresponding k=1 properties terminated in under a minute.

{-# ANN k2RDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2RDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2RDriverWbNonMemEmpty = nonJumpDriverWbNonMemEmptyProp 0b011_0011

{-# ANN k2RDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2RDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2RDriverMeNonMemEmpty = nonJumpDriverMeNonMemEmptyProp 0b011_0011

{-# ANN k2IDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2IDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2IDriverWbNonMemEmpty = nonJumpDriverWbNonMemEmptyProp 0b001_0011

{-# ANN k2IDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2IDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2IDriverMeNonMemEmpty = nonJumpDriverMeNonMemEmptyProp 0b001_0011

{-# ANN k2LoadDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2LoadDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2LoadDriverWbNonMemEmpty = nonJumpDriverWbNonMemEmptyProp 0b000_0011

{-# ANN k2LoadDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2LoadDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2LoadDriverMeNonMemEmpty = nonJumpDriverMeNonMemEmptyProp 0b000_0011

{-# ANN k2StoreDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2StoreDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2StoreDriverWbNonMemEmpty = nonJumpDriverWbNonMemEmptyProp 0b010_0011

{-# ANN k2StoreDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2StoreDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2StoreDriverMeNonMemEmpty = nonJumpDriverMeNonMemEmptyProp 0b010_0011

{-# ANN k2SystemNonEnvDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2SystemNonEnvDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2SystemNonEnvDriverWbNonMemEmpty = systemNonEnvDriverWbNonMemEmptyProp

{-# ANN k2SystemNonEnvDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2SystemNonEnvDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2SystemNonEnvDriverMeNonMemEmpty = systemNonEnvDriverMeNonMemEmptyProp

{-# ANN k2BranchNonTakenDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2BranchNonTakenDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2BranchNonTakenDriverWbNonMemEmpty = branchNonTakenDriverWbNonMemEmptyProp

{-# ANN k2BranchNonTakenDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2BranchNonTakenDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2BranchNonTakenDriverMeNonMemEmpty = branchNonTakenDriverMeNonMemEmptyProp

{-# ANN k2LuiDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2LuiDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2LuiDriverWbNonMemEmpty = nonJumpDriverWbNonMemEmptyProp 0b011_0111

{-# ANN k2LuiDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2LuiDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2LuiDriverMeNonMemEmpty = nonJumpDriverMeNonMemEmptyProp 0b011_0111

{-# ANN k2AuipcDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2AuipcDriverWbNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2AuipcDriverWbNonMemEmpty = nonJumpDriverWbNonMemEmptyProp 0b001_0111

{-# ANN k2AuipcDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2AuipcDriverMeNonMemEmpty :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2AuipcDriverMeNonMemEmpty = nonJumpDriverMeNonMemEmptyProp 0b001_0111

{-# ANN k2OtherDriverWbNonMemEmpty (Theory arrayAxioms) #-}
k2OtherDriverWbNonMemEmpty :: K2RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2OtherDriverWbNonMemEmpty = otherDriverWbNonMemEmptyProp

{-# ANN k2OtherDriverMeNonMemEmpty (Theory arrayAxioms) #-}
k2OtherDriverMeNonMemEmpty :: K2RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
k2OtherDriverMeNonMemEmpty = otherDriverMeNonMemEmptyProp

{-# ANN k2RunFlushMemRLoadLoad (Theory arrayAxioms) #-}
k2RunFlushMemRLoadLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemRLoadLoad = runFixedOpcodeMemStagesProp StageLoad StageLoad 0b011_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemRLoadStore (Theory arrayAxioms) #-}
k2RunFlushMemRLoadStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemRLoadStore = runFixedOpcodeMemStagesProp StageLoad StageStore 0b011_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemRStoreLoad (Theory arrayAxioms) #-}
k2RunFlushMemRStoreLoad :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemRStoreLoad = runFixedOpcodeMemStagesProp StageStore StageLoad 0b011_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemRStoreStore (Theory arrayAxioms) #-}
k2RunFlushMemRStoreStore :: K2RunState -> MemFields -> MemFields -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemRStoreStore = runFixedOpcodeMemStagesProp StageStore StageStore 0b011_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemIArith (Theory arrayAxioms) #-}
k2RunFlushMemIArith :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemIArith = runFixedOpcodeProp 0b001_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemLoad (Theory arrayAxioms) #-}
k2RunFlushMemLoad :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemLoad = runFixedOpcodeProp 0b000_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemJalr (Theory arrayAxioms) #-}
k2RunFlushMemJalr :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemJalr = runFixedOpcodeProp 0b110_0111 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemSystem (Theory arrayAxioms) #-}
k2RunFlushMemSystem :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemSystem = runFixedOpcodeProp 0b111_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemStore (Theory arrayAxioms) #-}
k2RunFlushMemStore :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemStore = runFixedOpcodeProp 0b010_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemBranch (Theory arrayAxioms) #-}
k2RunFlushMemBranch :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemBranch = runFixedOpcodeProp 0b110_0011 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemLui (Theory arrayAxioms) #-}
k2RunFlushMemLui :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemLui = runFixedOpcodeProp 0b011_0111 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemAuipc (Theory arrayAxioms) #-}
k2RunFlushMemAuipc :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemAuipc = runFixedOpcodeProp 0b001_0111 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemJal (Theory arrayAxioms) #-}
k2RunFlushMemJal :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemJal = runFixedOpcodeProp 0b110_1111 (K2Run RunFlushMem)

{-# ANN k2RunFlushMemOther (Theory arrayAxioms) #-}
k2RunFlushMemOther :: K2RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunFlushMemOther = runCaseProp WordOther (K2Run RunFlushMem)

{-# ANN k2RunDecodeR (Theory arrayAxioms) #-}
k2RunDecodeR :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeR = runFixedOpcodeProp 0b011_0011 (K2Run RunDecode)

{-# ANN k2RunDecodeIArith (Theory arrayAxioms) #-}
k2RunDecodeIArith :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeIArith = runFixedOpcodeProp 0b001_0011 (K2Run RunDecode)

{-# ANN k2RunDecodeLoad (Theory arrayAxioms) #-}
k2RunDecodeLoad :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeLoad = runFixedOpcodeProp 0b000_0011 (K2Run RunDecode)

{-# ANN k2RunDecodeJalr (Theory arrayAxioms) #-}
k2RunDecodeJalr :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeJalr = runFixedOpcodeProp 0b110_0111 (K2Run RunDecode)

{-# ANN k2RunDecodeSystem (Theory arrayAxioms) #-}
k2RunDecodeSystem :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeSystem = runFixedOpcodeProp 0b111_0011 (K2Run RunDecode)

{-# ANN k2RunDecodeStore (Theory arrayAxioms) #-}
k2RunDecodeStore :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeStore = runFixedOpcodeProp 0b010_0011 (K2Run RunDecode)

{-# ANN k2RunDecodeBranch (Theory arrayAxioms) #-}
k2RunDecodeBranch :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeBranch = runFixedOpcodeProp 0b110_0011 (K2Run RunDecode)

{-# ANN k2RunDecodeLui (Theory arrayAxioms) #-}
k2RunDecodeLui :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeLui = runFixedOpcodeProp 0b011_0111 (K2Run RunDecode)

{-# ANN k2RunDecodeAuipc (Theory arrayAxioms) #-}
k2RunDecodeAuipc :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeAuipc = runFixedOpcodeProp 0b001_0111 (K2Run RunDecode)

{-# ANN k2RunDecodeJal (Theory arrayAxioms) #-}
k2RunDecodeJal :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeJal = runFixedOpcodeProp 0b110_1111 (K2Run RunDecode)

{-# ANN k2RunDecodeOther (Theory arrayAxioms) #-}
k2RunDecodeOther :: K2RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2RunDecodeOther = runCaseProp WordOther (K2Run RunDecode)

-- Halted post-state -----------------------------------------------------------
--
-- Only SYSTEM words can decode to ecall/ebreak. The conditional halt
-- obligations for all other opcode cases are therefore tautologies, by
-- 'k2EnvWordIsSystem'.

{-# ANN k2HaltBreakStruct (Theory arrayAxioms) #-}
k2HaltBreakStruct :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2HaltBreakStruct = runFixedOpcodeProp 0b111_0011 (K2Halt HaltBreak HaltStruct)

{-# ANN k2HaltBreakFlushRf (Theory arrayAxioms) #-}
k2HaltBreakFlushRf :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2HaltBreakFlushRf = runFixedOpcodeProp 0b111_0011 (K2Halt HaltBreak HaltFlushRf)

{-# ANN k2HaltBreakFlushMem (Theory arrayAxioms) #-}
k2HaltBreakFlushMem :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2HaltBreakFlushMem = runFixedOpcodeProp 0b111_0011 (K2Halt HaltBreak HaltFlushMem)

{-# ANN k2HaltCallStruct (Theory arrayAxioms) #-}
k2HaltCallStruct :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2HaltCallStruct = runFixedOpcodeProp 0b111_0011 (K2Halt HaltCall HaltStruct)

{-# ANN k2HaltCallFlushRf (Theory arrayAxioms) #-}
k2HaltCallFlushRf :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2HaltCallFlushRf = runFixedOpcodeProp 0b111_0011 (K2Halt HaltCall HaltFlushRf)

{-# ANN k2HaltCallFlushMem (Theory arrayAxioms) #-}
k2HaltCallFlushMem :: K2RunState -> BitVector 25 -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k2HaltCallFlushMem = runFixedOpcodeProp 0b111_0011 (K2Halt HaltCall HaltFlushMem)

results :: [(String, Maybe String)]
results =
  [ ("k2EmptyStartup", $(pantomime 'k2EmptyStartup)),
    ("k2EmptyHaltedBreak", $(pantomime 'k2EmptyHaltedBreak)),
    ("k2EmptyHaltedCall", $(pantomime 'k2EmptyHaltedCall)),
    ("k2EmptyStoreHazardNoMem", $(pantomime 'k2EmptyStoreHazardNoMem)),
    ("k2WordCasesExhaustive", $(pantomime 'k2WordCasesExhaustive)),
    ("k2EnvWordIsSystem", $(pantomime 'k2EnvWordIsSystem)),
    -- Keep the formerly slow JALR/RF branch at the front as a regression
    -- sentinel. Valid and malformed JALR encodings take different driver rows.
    -- Each stage-effect cell has a five-minute solver budget.
    ("k2EffectClassesExhaustive", $(pantomime 'k2EffectClassesExhaustive)),
    -- The only effect cell to exceed that budget is refined and checked first.
    ("k2RunFlushRfJalrLQBranch", $(pantomime 'k2RunFlushRfJalrLQBranch)),
    ("k2RunFlushRfJalrLQEnv", $(pantomime 'k2RunFlushRfJalrLQEnv)),
    ("k2RunFlushRfJalrLQNop", $(pantomime 'k2RunFlushRfJalrLQNop)),
    ("k2RunFlushRfJalrWW", $(pantomime 'k2RunFlushRfJalrWW)),
    ("k2RunFlushRfJalrWL", $(pantomime 'k2RunFlushRfJalrWL)),
    ("k2RunFlushRfJalrWS", $(pantomime 'k2RunFlushRfJalrWS)),
    ("k2RunFlushRfJalrWQ", $(pantomime 'k2RunFlushRfJalrWQ)),
    ("k2RunFlushRfJalrLWR", $(pantomime 'k2RunFlushRfJalrLWR)),
    ("k2RunFlushRfJalrLWI", $(pantomime 'k2RunFlushRfJalrLWI)),
    ("k2RunFlushRfJalrLWJ", $(pantomime 'k2RunFlushRfJalrLWJ)),
    ("k2RunFlushRfJalrLWX", $(pantomime 'k2RunFlushRfJalrLWX)),
    ("k2RunFlushRfJalrLWU", $(pantomime 'k2RunFlushRfJalrLWU)),
    ("k2RunFlushRfJalrLL", $(pantomime 'k2RunFlushRfJalrLL)),
    ("k2RunFlushRfJalrLS", $(pantomime 'k2RunFlushRfJalrLS)),
    ("k2RunFlushRfJalrSW", $(pantomime 'k2RunFlushRfJalrSW)),
    ("k2RunFlushRfJalrSL", $(pantomime 'k2RunFlushRfJalrSL)),
    ("k2RunFlushRfJalrSS", $(pantomime 'k2RunFlushRfJalrSS)),
    ("k2RunFlushRfJalrSQBranch", $(pantomime 'k2RunFlushRfJalrSQBranch)),
    ("k2RunFlushRfJalrSQEnv", $(pantomime 'k2RunFlushRfJalrSQEnv)),
    ("k2RunFlushRfJalrSQNop", $(pantomime 'k2RunFlushRfJalrSQNop)),
    ("k2RunFlushRfJalrQW", $(pantomime 'k2RunFlushRfJalrQW)),
    ("k2RunFlushRfJalrQL", $(pantomime 'k2RunFlushRfJalrQL)),
    ("k2RunFlushRfJalrQS", $(pantomime 'k2RunFlushRfJalrQS)),
    ("k2RunFlushRfJalrQQ", $(pantomime 'k2RunFlushRfJalrQQ)),
    ("k2RunFlushRfRLoadLoad", $(pantomime 'k2RunFlushRfRLoadLoad)),
    ("k2RunFlushRfRLoadStore", $(pantomime 'k2RunFlushRfRLoadStore)),
    ("k2RunFlushRfRStoreLoad", $(pantomime 'k2RunFlushRfRStoreLoad)),
    ("k2RunFlushRfRStoreStore", $(pantomime 'k2RunFlushRfRStoreStore)),
    ("k2IDriverWbNonMemEmpty", $(pantomime 'k2IDriverWbNonMemEmpty)),
    ("k2IDriverMeNonMemEmpty", $(pantomime 'k2IDriverMeNonMemEmpty)),
    ("k2RunFlushRfIArithLoadLoad", $(pantomime 'k2RunFlushRfIArithLoadLoad)),
    ("k2RunFlushRfIArithLoadStore", $(pantomime 'k2RunFlushRfIArithLoadStore)),
    ("k2RunFlushRfIArithStoreLoad", $(pantomime 'k2RunFlushRfIArithStoreLoad)),
    ("k2RunFlushRfIArithStoreStore", $(pantomime 'k2RunFlushRfIArithStoreStore)),
    ("k2LoadDriverWbNonMemEmpty", $(pantomime 'k2LoadDriverWbNonMemEmpty)),
    ("k2LoadDriverMeNonMemEmpty", $(pantomime 'k2LoadDriverMeNonMemEmpty)),
    ("k2RunFlushRfLoadLoadLoad", $(pantomime 'k2RunFlushRfLoadLoadLoad)),
    ("k2RunFlushRfLoadLoadStore", $(pantomime 'k2RunFlushRfLoadLoadStore)),
    ("k2RunFlushRfLoadStoreLoad", $(pantomime 'k2RunFlushRfLoadStoreLoad)),
    ("k2RunFlushRfLoadStoreStore", $(pantomime 'k2RunFlushRfLoadStoreStore)),
    ("k2StoreDriverWbNonMemEmpty", $(pantomime 'k2StoreDriverWbNonMemEmpty)),
    ("k2StoreDriverMeNonMemEmpty", $(pantomime 'k2StoreDriverMeNonMemEmpty)),
    ("k2RunFlushRfStoreLoadLoad", $(pantomime 'k2RunFlushRfStoreLoadLoad)),
    ("k2RunFlushRfStoreLoadStore", $(pantomime 'k2RunFlushRfStoreLoadStore)),
    ("k2RunFlushRfStoreStoreLoad", $(pantomime 'k2RunFlushRfStoreStoreLoad)),
    ("k2RunFlushRfStoreStoreStore", $(pantomime 'k2RunFlushRfStoreStoreStore)),
    ("k2RunFlushRfSystemEnv", $(pantomime 'k2RunFlushRfSystemEnv)),
    ("k2SystemNonEnvDriverWbNonMemEmpty", $(pantomime 'k2SystemNonEnvDriverWbNonMemEmpty)),
    ("k2SystemNonEnvDriverMeNonMemEmpty", $(pantomime 'k2SystemNonEnvDriverMeNonMemEmpty)),
    ("k2RunFlushRfSystemLoadLoad", $(pantomime 'k2RunFlushRfSystemLoadLoad)),
    ("k2RunFlushRfSystemLoadStore", $(pantomime 'k2RunFlushRfSystemLoadStore)),
    ("k2RunFlushRfSystemStoreLoad", $(pantomime 'k2RunFlushRfSystemStoreLoad)),
    ("k2RunFlushRfSystemStoreStore", $(pantomime 'k2RunFlushRfSystemStoreStore)),
    ("k2BranchFunct3Covered", $(pantomime 'k2BranchFunct3Covered)),
    ("k2RunFlushRfBranchTakenEq", $(pantomime 'k2RunFlushRfBranchTakenEq)),
    ("k2RunFlushRfBranchTakenNe", $(pantomime 'k2RunFlushRfBranchTakenNe)),
    ("k2RunFlushRfBranchTakenLt", $(pantomime 'k2RunFlushRfBranchTakenLt)),
    ("k2RunFlushRfBranchTakenGe", $(pantomime 'k2RunFlushRfBranchTakenGe)),
    ("k2RunFlushRfBranchTakenLtu", $(pantomime 'k2RunFlushRfBranchTakenLtu)),
    ("k2RunFlushRfBranchTakenGeu", $(pantomime 'k2RunFlushRfBranchTakenGeu)),
    ("k2BranchNonTakenDriverWbNonMemEmpty", $(pantomime 'k2BranchNonTakenDriverWbNonMemEmpty)),
    ("k2BranchNonTakenDriverMeNonMemEmpty", $(pantomime 'k2BranchNonTakenDriverMeNonMemEmpty)),
    ("k2RunFlushRfBranchLoadLoad", $(pantomime 'k2RunFlushRfBranchLoadLoad)),
    ("k2RunFlushRfBranchLoadStore", $(pantomime 'k2RunFlushRfBranchLoadStore)),
    ("k2RunFlushRfBranchStoreLoad", $(pantomime 'k2RunFlushRfBranchStoreLoad)),
    ("k2RunFlushRfBranchStoreStore", $(pantomime 'k2RunFlushRfBranchStoreStore)),
    ("k2LuiDriverWbNonMemEmpty", $(pantomime 'k2LuiDriverWbNonMemEmpty)),
    ("k2LuiDriverMeNonMemEmpty", $(pantomime 'k2LuiDriverMeNonMemEmpty)),
    ("k2RunFlushRfLuiLoadLoad", $(pantomime 'k2RunFlushRfLuiLoadLoad)),
    ("k2RunFlushRfLuiLoadStore", $(pantomime 'k2RunFlushRfLuiLoadStore)),
    ("k2RunFlushRfLuiStoreLoad", $(pantomime 'k2RunFlushRfLuiStoreLoad)),
    ("k2RunFlushRfLuiStoreStore", $(pantomime 'k2RunFlushRfLuiStoreStore)),
    ("k2AuipcDriverWbNonMemEmpty", $(pantomime 'k2AuipcDriverWbNonMemEmpty)),
    ("k2AuipcDriverMeNonMemEmpty", $(pantomime 'k2AuipcDriverMeNonMemEmpty)),
    ("k2RunFlushRfAuipcLoadLoad", $(pantomime 'k2RunFlushRfAuipcLoadLoad)),
    ("k2RunFlushRfAuipcLoadStore", $(pantomime 'k2RunFlushRfAuipcLoadStore)),
    ("k2RunFlushRfAuipcStoreLoad", $(pantomime 'k2RunFlushRfAuipcStoreLoad)),
    ("k2RunFlushRfAuipcStoreStore", $(pantomime 'k2RunFlushRfAuipcStoreStore)),
    ("k2OtherDriverWbNonMemEmpty", $(pantomime 'k2OtherDriverWbNonMemEmpty)),
    ("k2OtherDriverMeNonMemEmpty", $(pantomime 'k2OtherDriverMeNonMemEmpty)),
    ("k2RunFlushRfOtherLoadLoad", $(pantomime 'k2RunFlushRfOtherLoadLoad)),
    ("k2RunFlushRfOtherLoadStore", $(pantomime 'k2RunFlushRfOtherLoadStore)),
    ("k2RunFlushRfOtherStoreLoad", $(pantomime 'k2RunFlushRfOtherStoreLoad)),
    ("k2RunFlushRfOtherStoreStore", $(pantomime 'k2RunFlushRfOtherStoreStore)),
    -- Put the previously hard coverage/memory cases first, so a regression is
    -- visible before the routine opcode matrix is re-executed.
    ("k2RDriverWbNonMemEmpty", $(pantomime 'k2RDriverWbNonMemEmpty)),
    ("k2RDriverMeNonMemEmpty", $(pantomime 'k2RDriverMeNonMemEmpty)),
    ("k2RunFlushMemRLoadLoad", $(pantomime 'k2RunFlushMemRLoadLoad)),
    ("k2RunFlushMemRLoadStore", $(pantomime 'k2RunFlushMemRLoadStore)),
    ("k2RunFlushMemRStoreLoad", $(pantomime 'k2RunFlushMemRStoreLoad)),
    ("k2RunFlushMemRStoreStore", $(pantomime 'k2RunFlushMemRStoreStore)),
    ("k2RunStructR", $(pantomime 'k2RunStructR)),
    ("k2RunStructIArith", $(pantomime 'k2RunStructIArith)),
    ("k2RunStructLoadLoadLoad", $(pantomime 'k2RunStructLoadLoadLoad)),
    ("k2RunStructLoadLoadStoreF0", $(pantomime 'k2RunStructLoadLoadStoreF0)),
    ("k2RunStructLoadLoadStoreF1", $(pantomime 'k2RunStructLoadLoadStoreF1)),
    ("k2RunStructLoadLoadStoreF2", $(pantomime 'k2RunStructLoadLoadStoreF2)),
    ("k2RunStructLoadLoadStoreF3", $(pantomime 'k2RunStructLoadLoadStoreF3)),
    ("k2RunStructLoadLoadStoreF4", $(pantomime 'k2RunStructLoadLoadStoreF4)),
    ("k2RunStructLoadLoadStoreF5", $(pantomime 'k2RunStructLoadLoadStoreF5)),
    ("k2RunStructLoadLoadStoreF6", $(pantomime 'k2RunStructLoadLoadStoreF6)),
    ("k2RunStructLoadLoadStoreF7", $(pantomime 'k2RunStructLoadLoadStoreF7)),
    ("k2RunStructLoadStoreLoad", $(pantomime 'k2RunStructLoadStoreLoad)),
    ("k2RunStructLoadStoreStore", $(pantomime 'k2RunStructLoadStoreStore)),
    ("k2RunStructJalr", $(pantomime 'k2RunStructJalr)),
    ("k2RunStructSystem", $(pantomime 'k2RunStructSystem)),
    ("k2RunStructStore", $(pantomime 'k2RunStructStore)),
    ("k2RunStructBranch", $(pantomime 'k2RunStructBranch)),
    ("k2RunStructLui", $(pantomime 'k2RunStructLui)),
    ("k2RunStructAuipc", $(pantomime 'k2RunStructAuipc)),
    ("k2RunStructJal", $(pantomime 'k2RunStructJal)),
    ("k2RunStructOther", $(pantomime 'k2RunStructOther)),
    ("k2RunFlushRfJal", $(pantomime 'k2RunFlushRfJal)),
    ("k2RunFlushMemIArith", $(pantomime 'k2RunFlushMemIArith)),
    ("k2RunFlushMemLoad", $(pantomime 'k2RunFlushMemLoad)),
    ("k2RunFlushMemJalr", $(pantomime 'k2RunFlushMemJalr)),
    ("k2RunFlushMemSystem", $(pantomime 'k2RunFlushMemSystem)),
    ("k2RunFlushMemStore", $(pantomime 'k2RunFlushMemStore)),
    ("k2RunFlushMemBranch", $(pantomime 'k2RunFlushMemBranch)),
    ("k2RunFlushMemLui", $(pantomime 'k2RunFlushMemLui)),
    ("k2RunFlushMemAuipc", $(pantomime 'k2RunFlushMemAuipc)),
    ("k2RunFlushMemJal", $(pantomime 'k2RunFlushMemJal)),
    ("k2RunFlushMemOther", $(pantomime 'k2RunFlushMemOther)),
    ("k2RunDecodeR", $(pantomime 'k2RunDecodeR)),
    ("k2RunDecodeIArith", $(pantomime 'k2RunDecodeIArith)),
    ("k2RunDecodeLoad", $(pantomime 'k2RunDecodeLoad)),
    ("k2RunDecodeJalr", $(pantomime 'k2RunDecodeJalr)),
    ("k2RunDecodeSystem", $(pantomime 'k2RunDecodeSystem)),
    ("k2RunDecodeStore", $(pantomime 'k2RunDecodeStore)),
    ("k2RunDecodeBranch", $(pantomime 'k2RunDecodeBranch)),
    ("k2RunDecodeLui", $(pantomime 'k2RunDecodeLui)),
    ("k2RunDecodeAuipc", $(pantomime 'k2RunDecodeAuipc)),
    ("k2RunDecodeJal", $(pantomime 'k2RunDecodeJal)),
    ("k2RunDecodeOther", $(pantomime 'k2RunDecodeOther)),
    ("k2HaltBreakStruct", $(pantomime 'k2HaltBreakStruct)),
    ("k2HaltBreakFlushRf", $(pantomime 'k2HaltBreakFlushRf)),
    ("k2HaltBreakFlushMem", $(pantomime 'k2HaltBreakFlushMem)),
    ("k2HaltCallStruct", $(pantomime 'k2HaltCallStruct)),
    ("k2HaltCallFlushRf", $(pantomime 'k2HaltCallFlushRf)),
    ("k2HaltCallFlushMem", $(pantomime 'k2HaltCallFlushMem))
  ]
