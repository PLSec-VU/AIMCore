-- | The leakage theorem, as a preservation obligation.
--
-- 'leakObligation' says that 'Proof.Leakage.Simulator.proj' commutes with one
-- driver hop: if it relates an implementation state to an architectural state
-- and a simulator state, then after the hop it relates them again, and the two
-- machines produced the same cycle-by-cycle observations.
--
-- Together with the invariant holding at reset, that gives the leakage result:
-- an attacker watching the memory bus learns nothing that is not already in
-- 'Proof.Leakage.Model.L'. The simulator reproduces the observation from the
-- leakage alone, including the hop length -- which is the instruction timing.
--
-- Only the simulator half of the projection is checked here. The architectural
-- half commutes by construction: 'Proof.Leakage.Simulator.archOfLeak' is
-- 'Proof.Functional.Obligation.isaOfHop' composed with
-- 'Proof.Leakage.Simulator.isaNext', and the functional obligations already
-- establish that @isaOfHop@ advances by one 'isaStep' per hop.
--
-- == What is assumed
--
-- Nothing beyond the invariant. A store overlapping the instruction word in
-- decode is caught by 'Core.decode' and stalled, so the aliasing-store side
-- condition the earlier proof carried is gone.
--
-- There is deliberately no assumption on jump targets. Branch and @jal@ targets
-- are reproduced with the original immediates; a @jalr@ target is parked in a
-- register by 'Proof.Leakage.Simulator.installJump' and is exact for any 32-bit
-- address.
module Proof.Leakage.Obligation
  ( leakObligation,
    leakPremises,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Proof.Functional.Invariant (invAtFree)
import Proof.Functional.Obligation (isaOfHop)
import Proof.Leakage.Model
import Proof.Leakage.Simulator
import Memory.Types (MemOps (..))
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The premises of 'leakObligation': the functional invariant.
leakPremises ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
leakPremises wr wa sys = invAtFree wr wa (isaOfHop sys) sys

-- | @proj@ commutes with a driver hop, and the observations agree.
leakObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
leakObligation wr wa sys =
  not (leakPremises wr wa sys) || (simEq wr (censor sysI) ss' && obsI == obsL)
  where
    (sysI, obsI) = implHop sys
    ((_, ss'), obsL) = leakSimHop (proj sys)
