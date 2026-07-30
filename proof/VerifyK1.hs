-- | The @k = 1@ inductive step, symbolically.
--
-- In its own module deliberately. The plugin re-runs every annotated property
-- in a module whenever that module is recompiled, and these proofs cost tens
-- of minutes each, so one obligation per module keeps an edit to one from
-- re-proving the others. ("Verify" still holds the cheap probes together with
-- @indStep0@; splitting that one out is worth doing when it next changes.)
--
-- The encoding is the one "Verify" settled on: pipeline registers as symbolic
-- scalars, register file and memory as SMT arrays, the invariant compared at a
-- symbolic witness register and byte address. See "Verify" for why each of
-- those is the way it is.
module VerifyK1
  ( indStep1,
    results,
  )
where

import ArrayRF
import Axioms (arrayAxioms)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Instruction
import Machine
import Obligation (indStepObligation1)
import Pantomime (Theory (..), pantomime)
import qualified Pantomime.BuiltIn as Pantomime
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The pipeline registers, as plain scalars. An ADT of scalars can be a
-- symbolic argument; a record containing a function cannot. (Mirrors
-- @Verify.StateScalars@; duplicated rather than shared so that a change made
-- for one obligation cannot silently re-run the other.)
data K1State = K1State
  { k1FePc :: Address,
    k1DePc :: Address,
    k1ExPc :: Address,
    k1ExIr :: Instruction,
    k1MeIr :: Instruction,
    k1MeRes :: Word,
    k1MeAddr :: Address,
    k1WbIr :: Instruction,
    k1WbRes :: Word,
    k1Ctrl :: Core.Control Identity,
    k1Halt :: Maybe Core.HaltState,
    k1HaltPending :: Maybe Core.HaltState
  }

sysOf :: K1State -> Core.Input Identity -> RegArr -> MemArr -> SysG RegArrF MemArr
sysOf ss i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = k1FePc ss,
            Core.stateDePc = k1DePc ss,
            Core.stateExPc = k1ExPc ss,
            Core.stateExInstr = k1ExIr ss,
            Core.stateMeInstr = k1MeIr ss,
            Core.stateMeRes = Identity (k1MeRes ss),
            Core.stateMeAddr = k1MeAddr ss,
            Core.stateWbInstr = k1WbIr ss,
            Core.stateWbRes = Identity (k1WbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = k1Ctrl ss,
            Core.stateHalt = k1Halt ss,
            Core.stateHaltPending = k1HaltPending ss
          },
      sysInput = i,
      sysMem = ma
    }

-- | The inductive step for @k = 1@: the driver's two-cycle hop.
--
-- Covers the startup hop and the steady hop with a memory instruction in
-- writeback. Two cycles rather than one, so expect the symbolic execution and
-- the solve to cost more than @k = 0@ did (which was 36m14s to @unsat@).
-- DISABLED: the monolithic k=1 query does not scale -- Z3 ran ~3h40m of CPU
-- without terminating, and on a later attempt was killed at 457MB+ RSS. It is
-- superseded by the split obligations in "VerifyK1Split"; kept for reference
-- and as the statement those splits must together imply.
-- {-# ANN indStep1 (Theory arrayAxioms) #-}
indStep1 ::
  K1State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
indStep1 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation1 wr wa (sysOf ss i ra ma)

results :: [(String, Maybe String)]
results = [("indStep1", Nothing)] -- see the note on the disabled ANN above
