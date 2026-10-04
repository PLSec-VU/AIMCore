-- | The leakage theorem, as a preservation obligation.
--
-- 'leakObligation' says that 'Proof.Leakage.Simulator.proj' commutes with one
-- driver hop: if it relates an implementation state to an architectural state
-- and a simulator state, then after the hop it relates them again, and the two
-- machines produced the same cycle-by-cycle observations.
--
-- Together with the invariant holding at reset, that gives the leakage result:
-- an attacker watching the core's memory requests learns nothing that is not
-- already in 'Proof.Leakage.Model.L', the constant-time leakage. The simulator
-- reproduces the observation from the leakage alone, including the hop length
-- -- which is the instruction timing.
--
-- Only the simulator half of the projection is checked here. The architectural
-- half commutes by construction: 'Proof.Leakage.Simulator.archOfLeak' is
-- 'Proof.Functional.Obligation.isaAt' at 'Proof.Functional.Obligation.hopPc', composed with
-- 'Proof.Leakage.Simulator.isaNext', and the functional obligations already
-- establish that it advances by one 'isaStep' per hop.
--
-- == What is assumed
--
-- Nothing beyond the invariant. A store overlapping the instruction word in
-- decode is caught by 'Core.decode' and stalled; the stall depends on the
-- store's address, which the constant-time leakage carries, so the simulator
-- stalls at the same points.
--
-- There is deliberately no assumption on addresses. Branch and @jal@ targets
-- are reproduced with the original immediates; a @jalr@ target and a data
-- address are parked in a register by 'Proof.Leakage.Simulator.parkAddress' and
-- are exact for any 32-bit address.
module Proof.Leakage.Obligation
  ( leakObligation,
    leakObligationAt,
    leakPremises,
    leakBaseObligation,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Proof.Driver (driver)
import Proof.Functional.Invariant (invAtFree)
import Proof.Functional.Obligation (hopPc, isaAt)
import Proof.Leakage.Simulator
import Memory.Types (MemOps (..))
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The premises of 'leakObligation': the functional invariant.
leakPremises ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
leakPremises wr wa sys = invAtFree wr wa (isaAt (hopPc sys) sys) sys

-- | @proj@ commutes with a driver hop, and the observations agree.
leakObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
leakObligation wr wa sys =
  not (leakPremises wr wa sys) || (simEq wr (censor sysI) ss' && obsI == obsL)
  where
    (sysI, obsI) = implHop sys
    ((_, ss'), obsL) = leakSimHop (proj sys)

-- | 'leakObligation', restricted to hops of length @k + 1@.
--
-- The symbolic properties state one of these per @k@: the number of unrolled
-- cycles has to be concrete, because the symbolic executor cannot unroll a
-- symbolic count, and pinning @driver == k@ leaves each property one unrolling
-- rather than four.
leakObligationAt ::
  (RegFileOps r, MemOps m) => Int -> RegIdx -> Address -> SysG r m -> Bool
leakObligationAt k wr wa sys = driver sys /= k || leakObligation wr wa sys

-- | The base case: the first hop out of reset.
--
-- 'leakObligation' does not cover it. Its premise is the invariant, which the
-- reset state does not satisfy: nothing is in the pipeline yet.
-- 'Proof.Functional.Induction.baseCase' states the functional side of the same
-- hop. For the reset states @sys@ and @other@ of any two programs, this says
-- that the simulator starts in the same state for both -- everything it learns
-- about the program reaches it through the leakage -- and that the reset hop of
-- @sys@ commutes with the projection, with equal observations.
leakBaseObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> SysG r m -> SysG r m -> Bool
leakBaseObligation wr sys other =
  simEq wr (censor sys) (censor other)
    && driver sys == 1
    && simEq wr (censor sysI) ss'
    && obsI == obsL
  where
    (sysI, obsI) = implHop sys
    ((_, ss'), obsL) = leakSimHop (proj sys)
