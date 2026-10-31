-- | The invariant relating the leakage simulator to the core: the leakage
-- counterpart of "Proof.Core.Functional.Invariant".
module Proof.Core.Leakage.Invariant
  ( simInv
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Maybe (isJust, isNothing)
import Core.Instruction hiding (Halted)
import Proof.Core.Leakage.Model
import Proof.Core.Leakage.Simulator
import Proof.Core.Functional.Machine
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The relation between the simulator and the core that the leakage proof
-- preserves from hop to hop.
--
-- The two machines fetch and decode at the same PCs, agree on whether decode
-- expects an instruction, are both running or both halted, and have the same
-- memory access pending in the memory stage. Which instruction the core is
-- executing is not stated here: the functional invariant pins the core's
-- execute stage to the ISA's next instruction, and the simulator is fed that
-- instruction's leakage, so the two agree by construction.
--
-- The simulator has no writeback stage; 'simStateDeExpInstr' stands in for the
-- one fact about it the driver needs. Stall states are excluded: the relation
-- is only stated at hop boundaries, where the execute stage holds a decoded
-- instruction or the core has halted.
simInv ::
  SimState ->
  SysG r m ->
  Bool
simInv sim (Sys core _ _) =
  simStateFePc sim == stateFePc core
    && simStateDePc sim == stateDePc core
    && simStateDeExpInstr sim == stateDeExpInstr core
    && exStage
    && meStage
  where
    exStage = case simStateExInstrType sim of
      DecodedInstr -> isNothing (stateHalt core)
      Halted -> isJust (stateHalt core)
      _ -> False

    meStage = case (simStateMeInstr sim, stateMeInstr core) of
      (LLoad size addr _, Core.Instruction.IType (Load size' _) _ _ _) ->
        size == size' && addr == stateMeAddr core
      (LStore size addr, Core.Instruction.SType size' _ _ _) ->
        size == size' && addr == stateMeAddr core
      (leak, ir) -> not (isMemLeak leak) && not (Core.Instruction.isMemInstr ir)
