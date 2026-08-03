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
-- establish that @isaOfHop@ advances by one 'Proof.ISAStep.isaStep' per hop.
--
-- == What is assumed
--
-- One thing beyond the invariant: 'Proof.Functional.Invariant.noStoreAlias', at
-- every state of the hop rather than only at its ends ('noStoreAliasHop'). It
-- says no store writes a word the fetch path is using, which RISC-V requires a
-- @FENCE.I@ for anyway. The leakage proof needs it a little stronger than the
-- functional one -- strong enough that 'Proof.Driver.storeHazard' is constantly
-- 'False' -- because a store hazard is a two-cycle stall keyed on a 32-bit
-- address match, and no instruction 'Proof.Leakage.Model.inv' can emit
-- reproduces it.
--
-- There is deliberately no assumption on jump targets. Branch and @jal@ targets
-- are reproduced with the original immediates; a @jalr@ target is parked in a
-- register by 'Proof.Leakage.Simulator.installJump' and is exact for any 32-bit
-- address.
module Proof.Leakage.Obligation
  ( leakObligation,
    leakPremises,
    noSimStoreAlias,
    noStoreAliasHop,
    noSimStoreAliasHop,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Proof.Driver (driver)
import Proof.Functional.Invariant (invAtFree, noStoreAlias)
import Proof.Functional.Obligation (isaOfHop)
import Proof.Leakage.Model
import Proof.Leakage.Simulator
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | 'Proof.Functional.Invariant.noStoreAlias' for a simulator state.
noSimStoreAlias :: (RegFileOps r) => SimSys r -> Bool
noSimStoreAlias = noStoreAlias

-- | The no-aliasing-store condition at every state of a hop, intermediates
-- included.
--
-- The ends alone are not enough: a store one stage behind the memory stage at
-- the pre-state commits /during/ the hop, so the aliasing pair is visible at
-- neither end. 'Proof.Functional.Obligation.indStepObligation1' and its
-- siblings assume it the same way.
--
-- Unrolled per driver case, because the step count has to be concrete for
-- Pantomime.
noStoreAliasHop :: (RegFileOps r, MemOps m) => SysG r m -> Bool
noStoreAliasHop sys = case driver sys of
  0 -> noStoreAlias s1
  1 -> noStoreAlias s1 && noStoreAlias s2
  2 -> noStoreAlias s1 && noStoreAlias s2 && noStoreAlias s3
  _ -> noStoreAlias s1 && noStoreAlias s2 && noStoreAlias s3 && noStoreAlias s4
  where
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2
    s4 = stepSys s3

-- | 'noStoreAliasHop' transported along the projection, for the simulator's own
-- hop.
noSimStoreAliasHop :: (RegFileOps r) => SimSys r -> L -> Bool
noSimStoreAliasHop ss l = case driver ss0 of
  0 -> noSimStoreAlias s1
  1 -> noSimStoreAlias s1 && noSimStoreAlias s2
  2 -> noSimStoreAlias s1 && noSimStoreAlias s2 && noSimStoreAlias s3
  _ ->
    noSimStoreAlias s1
      && noSimStoreAlias s2
      && noSimStoreAlias s3
      && noSimStoreAlias s4
  where
    w = invWord l
    ss0 = installLeak w ss
    s1 = fst (stepSimOut w ss0)
    s2 = fst (stepSimOut w s1)
    s3 = fst (stepSimOut w s2)
    s4 = fst (stepSimOut w s3)

-- | The premises of 'leakObligation': the functional invariant, and the
-- no-aliasing-store condition on both machines.
leakPremises ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
leakPremises wr wa sys =
  invAtFree wr wa (isaOfHop sys) sys
    && noStoreAlias sys
    && noStoreAliasHop sys
    && noSimStoreAlias ss
    && noSimStoreAliasHop ss (leakOf a)
  where
    (a, ss) = proj sys

-- | @proj@ commutes with a driver hop, and the observations agree.
leakObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
leakObligation wr wa sys =
  not (leakPremises wr wa sys) || (simEq wr (censor sysI) ss' && obsI == obsL)
  where
    (sysI, obsI) = implHop sys
    ((_, ss'), obsL) = leakSimHop (proj sys)
