-- | The invariant relating the leakage simulator to the core: the leakage
-- counterpart of "Proof.Core.Functional.Invariant".
module Proof.Core.Leakage.Invariant
  ( simInv,
    simInvFunctional,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.Pipeline as Core
import Data.Maybe (isJust)
import Core.Instruction hiding (Halted)
import Proof.Core.Leakage.Simulator
import Proof.Core.Functional.Machine
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The relation between the simulator and the core that the leakage proof
-- preserves from hop to hop.
--
-- The two machines fetch and decode at the same PCs, agree on whether decode
-- expects an instruction, are both running or both halted, and have the same
-- memory access pending in the memory stage. The simulator's control lines
-- are at rest, as they are after every cycle. Which instruction the core is
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
    && simStateCtrl sim == simInitCtrl
  where
    exStage
      | isJust (stateHalt core) = simStateExInstrType sim == Halted
      | otherwise = simStateExInstrType sim == DecodedInstr

    meStage =
      case stateMeInstr core of
        Core.Instruction.IType (Load size _) _ _ _ ->
          simStateMeAccess sim == SimLoad size (stateMeAddr core)
        Core.Instruction.SType size _ _ _ ->
          simStateMeAccess sim == SimStore size (stateMeAddr core)
        _ -> simStateMeAccess sim == SimNoAccess

-- | The invariant is functional: two simulators it relates to the same system
-- are equal. Every field of the simulator is pinned by the core: its PCs and
-- decode flag, its execute stage by whether the core has halted, its memory
-- stage by the core's, and its control lines, which are at rest.
simInvFunctional :: SimState -> SimState -> SysG r m -> Bool
simInvFunctional sim1 sim2 sys =
  not premises || conclusion
  where
    premises = simInv sim1 sys && simInv sim2 sys
    conclusion = sim1 == sim2
