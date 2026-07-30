-- | The @k = 2@ and @k = 3@ inductive steps, one obligation each.
--
-- This module used to hold 146 obligations. They were the leaves of a case split
-- on the top-level opcode, on the register-file effect of the two older pipeline
-- stages, and in the worst cells on @funct3@ as well -- a split forced entirely
-- by Z3's behaviour on the parent queries.
--
-- That split is no longer needed. Two changes removed the need for it:
--
--   * 'Core.sllWord' and friends keep the ALU shift amount a bitvector, so a
--     query no longer carries the integer round trip that made it unreadable to
--     the bitvector-only solvers;
--   * with Bitwuzla as the backend the coarse parent queries come back in
--     seconds, where Z3 needed minutes or timed out outright.
--
-- So this module now states the obligation the way "Verify" states @k = 0@: the
-- unsplit 'Obligation.indStepObligation2', with the execute stage an arbitrary
-- 'Instruction' rather than a decoded fresh word, and the full
-- 'Invariant.invAtFree' disjunction on both sides. That is the ground-truth
-- statement -- nothing here strengthens a premise or narrows a conclusion, so
-- there is no exhaustiveness side condition to discharge and no way for a
-- decomposition to drift from the thing it stands for.
module VerifyK2Split
  ( results,
  )
where

import ArrayRF
import Axioms (arrayAxioms)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Instruction
import LoggedPantomime (pantomime)
import Machine
import Obligation (indStepObligation2, indStepObligation3)
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The pipeline registers as plain scalars: an ADT of scalars can be a
-- symbolic argument, a record containing a function cannot.
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

-- | The @k = 2@ inductive step, whole.
--
-- Unlike @k = 0@ and @k = 1@ this hop can execute an environment instruction, so
-- the conclusion covers both alternatives of the architectural step: either the
-- ISA advances and the core lands in a running case, or it halts and the core
-- lands in a halted case with the architectural state carried across unchanged.
-- 'Obligation.indStepObligation2' says exactly that.
{-# ANN indStep2 (Theory arrayAxioms) #-}
indStep2 ::
  K2State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
indStep2 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation2 wr wa (sysOf ss i ra ma)

-- | The @k = 3@ inductive step, whole: the driver's longest hop, four cycles.
--
-- Same shape as 'indStep2'; see 'Obligation.indStepObligation3'.
{-# ANN indStep3 (Theory arrayAxioms) #-}
indStep3 ::
  K2State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
indStep3 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation3 wr wa (sysOf ss i ra ma)

results :: [(String, Maybe String)]
results =
  [ ("indStep2", $(pantomime 'indStep2)),
    ("indStep3", $(pantomime 'indStep3))
  ]
