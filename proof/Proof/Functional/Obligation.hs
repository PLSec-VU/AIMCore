-- | The proof obligations, in one place.
--
-- Both the symbolic properties ("Proof.Functional.Induction") and the QuickCheck harness
-- ("ProofSpec") go through these definitions, so the two cannot drift apart.
-- The only thing they are allowed to differ in is how the system state is
-- built: symbolic scalars on one side, a generator on the other. That is the
-- input space, not the property.
module Proof.Functional.Obligation
  ( isaOfG,
    isaOfHop,
    isStartupShape,
    indStepObligation,
    indStepObligation1,
    indStepObligation2,
    indStepObligation3,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Proof.Driver (driver)
import Proof.ISAStep
import Instruction
import Proof.Functional.Invariant
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The architectural state the invariant claims @sys@ corresponds to.
--
-- Derived from @sys@ by the flush rather than taken as a further input: the
-- invariant's container equalities then hold by construction, so assuming the
-- invariant at one witness reduces to assuming its scalar conjuncts -- which is
-- the full container invariant, not a weakening of it.
isaOfG :: (RegFileOps r, MemOps m) => SysG r m -> IsaStateG r m
isaOfG (Sys st inp mem) =
  IsaState {isaPc = Core.stateExPc st, isaRegFile = frf, isaMem = fm}
  where
    (fm, frf) =
      flushMeStage
        (Core.stateMeInstr st)
        (runIdentity (Core.stateMeRes st))
        (Core.stateMeAddr st)
        ( flushWbStage
            (Core.stateWbInstr st)
            (runIdentity (Core.stateWbRes st))
            (runIdentity (Core.inputMem inp))
            (mem, Core.stateRegFile st)
        )

-- | Does the pipeline have the shape the invariant's startup case describes?
--
-- The distinction matters from @k = 1@ onwards: a startup hop brings the first
-- instruction into the execute stage without executing anything, so the
-- architectural state does not advance across it. Every other hop retires one
-- instruction.
isStartupShape :: SysG r m -> Bool
isStartupShape (Sys st inp _) =
  Core.stateWbInstr st == Nop FirstCycle
    && Core.stateMeInstr st == Nop FirstCycle
    && Core.stateExInstr st == Nop FirstCycle
    && not (Core.inputIsInstr inp)

-- | The architectural state for a hop, startup included.
--
-- 'isaOfG' reads the architectural PC off the /execute/ stage, which is right
-- for a running state. A startup state has nothing in the pipe yet, and the
-- invariant's startup case correspondingly pins @isaPc@ to the /fetch/ stage;
-- deriving it from @exPc@ there would produce an architectural state that no
-- case of the invariant admits, and the obligation would hold vacuously on
-- every startup state instead of saying anything about it.
--
-- The register file and memory need no special case: on a startup-shaped state
-- both flushes are the identity, since every stage holds @Nop FirstCycle@.
isaOfHop :: (RegFileOps r, MemOps m) => SysG r m -> IsaStateG r m
isaOfHop sys
  | isStartupShape sys = (isaOfG sys) {isaPc = Core.stateFePc (sysState sys)}
  | otherwise = isaOfG sys

-- | The @k = 0@ inductive step: the driver's one-cycle hop.
--
-- > inv(a, c) /\ driver(c) = 0  ==>  inv(isaStep(a), stepSys(c))
--
-- The no-self-modifying-store side condition is /assumed/ of both the pre- and
-- post-state, which rules out transitions that create an aliasing store rather
-- than obliging us to show none can arise. See 'Proof.Functional.Invariant.noStoreAlias'.
--
-- @driver == 0@ excludes environment instructions, so the ISA cannot halt on
-- this hop and the @IsaHalted@ branch is vacuous. Startup states all have
-- @driver == 1@, so 'isaOfG' (rather than 'isaOfHop') is exact here.
indStepObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG sys
    sys' = stepSys sys

    premises =
      invAtFree wr wa isa sys
        && driver sys == 0
        && noStoreAlias sys
        && noStoreAlias sys'

    conclusion =
      case isaStep isa of
        Next isa' -> invAtFree wr wa isa' sys'
        IsaHalted -> True

-- | The @k = 1@ inductive step: the driver's two-cycle hop.
--
-- Two things differ from @k = 0@ beyond the extra cycle. First, the
-- no-aliasing-store assumption covers the /intermediate/ state as well: a
-- store in the memory stage of @s1@ commits during the @s1 -> s2@ step without
-- appearing in the pre- or post-state. Second, a startup hop does not retire
-- an instruction, so the architectural state is carried across unchanged.
indStepObligation1 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation1 wr wa sys =
  not premises || conclusion
  where
    isa = isaOfHop sys
    s1 = stepSys sys
    s2 = stepSys s1

    premises =
      invAtFree wr wa isa sys
        && driver sys == 1
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2

    conclusion
      | isStartupShape sys = invAtFree wr wa isa s2
      | otherwise =
          case isaStep isa of
            Next isa' -> invAtFree wr wa isa' s2
            -- driver == 1 excludes environment instructions (they route to a
            -- three-cycle hop), so the ISA cannot halt here either.
            IsaHalted -> True

-- | The @k = 2@ inductive step: the driver's three-cycle hop.
--
-- Unlike the shorter hops, this one can execute an environment instruction.
-- The architectural state then stays at the trapping instruction while the
-- core reaches one of the two halted invariant cases.
indStepObligation2 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation2 wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG sys
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2

    premises =
      invAtFree wr wa isa sys
        && driver sys == 2
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2
        && noStoreAlias s3

    conclusion =
      case isaStep isa of
        Next isa' -> invAtFree wr wa isa' s3
        IsaHalted -> invAtFree wr wa isa s3

-- | The @k = 3@ inductive step: the driver's four-cycle hop, the longest.
--
-- Stated exactly as 'indStepObligation2', one cycle longer. The @IsaHalted@
-- alternative is kept even though this hop should not be able to trap -- the
-- driver routes environment instructions to a three-cycle hop -- because
-- covering it costs nothing and assuming it away would be an unchecked side
-- argument.
indStepObligation3 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation3 wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG sys
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2
    s4 = stepSys s3

    premises =
      invAtFree wr wa isa sys
        && driver sys == 3
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2
        && noStoreAlias s3
        && noStoreAlias s4

    conclusion =
      case isaStep isa of
        Next isa' -> invAtFree wr wa isa' s4
        IsaHalted -> invAtFree wr wa isa s4
