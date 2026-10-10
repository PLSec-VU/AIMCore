-- | The proof obligations, in one place.
--
-- Both the symbolic properties ("Proof.Functional.Induction") and the QuickCheck harness
-- ("ProofSpec") go through these definitions, so the two cannot drift apart.
-- The only thing they are allowed to differ in is how the system state is
-- built: symbolic scalars on one side, a generator on the other. That is the
-- input space, not the property.
module Proof.Functional.Obligation
  ( isaAt,
    baseCaseObligation,
    indStepObligation,
    indStepObligation1,
    indStepObligation2,
    indStepObligation3,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Proof.Functional.Driver (driver)
import ISA (IsaStateG (..), StepG (..), isaStep)
import Proof.Functional.Invariant
import Memory.Types (MemOps (..))
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The architectural register file and memory that the invariant claims @sys@
-- corresponds to, at an architectural PC supplied by the caller.
--
-- The containers are /derived/ from @sys@ by the flush rather than taken as
-- further inputs. They have to be: the invariant compares them pointwise, at one
-- witness register and one witness byte, so a freely quantified register file
-- and memory would be tied to @sys@ only at those two points. 'ISA.isaStep'
-- reads memory at @isaPc@ and registers at @rs1@/@rs2@, which are elsewhere, so
-- the solver could pick an architectural state whose instruction at @isaPc@
-- bears no relation to the one the core is executing. Deriving them makes the
-- container equalities hold by construction, and what is left in the premise is
-- the scalar conjuncts, assumed in full.
--
-- @isaPc@ is different, and is quantified rather than derived. It is a scalar,
-- so the invariant pins it exactly -- but via a different conjunct in each case:
-- @stateExPc@ when running and @stateHalt@ once halted. Deriving it from either
-- one would make the other case unsatisfiable as a premise.
isaAt :: (RegFileOps r, MemOps m) => Address -> SysG r m -> IsaStateG r m
isaAt ipc (Sys st inp mem) =
  IsaState {isaPc = ipc, isaRegFile = frf, isaMem = fm}
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

-- | The base case: the invariant holds once the initial state has taken its
-- first hop.
--
-- Without this the four steps below say only that the invariant is /preserved/,
-- which is vacuous if it never holds anywhere. Together they give the theorem:
-- the invariant relates the core to the ISA at every state the driver lands on
-- after the start.
--
-- The initial state itself is not a case of the invariant. 'Core.init' has
-- nothing in the pipeline, so the driver gives it a two-cycle hop that fetches
-- and decodes the first instruction without executing anything. This property
-- states that hop directly: the driver does assign it two cycles, and after
-- them the running case relates the core to the ISA's /initial/ state -- zero
-- ISA steps. Handling it here rather than as a case of the invariant is what
-- lets every inductive step retire exactly one instruction.
--
-- The start PC, the register file and memory are parameters, used for both the
-- core and the ISA; every other field comes from 'Core.init' itself, so the
-- initial pipeline shape cannot drift from the real one. Taking them as
-- parameters makes the base case cover any loaded program and any initial PC
-- and register contents, such as the ELF entry point and stack pointer the ELF
-- runners start the core with.
--
-- Note what sharing them costs: memory and the register file are the same
-- values on both sides, and the first hop writes neither, so the invariant's
-- two container equalities hold by construction here and this property alone
-- would not notice if the core's initial register file and the ISA's
-- ('RegFile.initRF') disagreed. The concrete test in "ProofSpec" closes that
-- gap -- it runs on the real 'Vec'-backed state, where both files are built
-- independently.
baseCaseObligation ::
  (RegFileOps r, MemOps m) => r Identity -> m -> RegIdx -> Address -> Address -> Bool
baseCaseObligation rf mem wr wa pc =
  driver sys == 1 && invAtFree wr wa isa sys2
  where
    isa = IsaState {isaPc = pc, isaRegFile = rf, isaMem = mem}
    sys = Sys (Core.initAt pc rf) Core.initInput mem

    sys1 = stepSys sys
    sys2 = stepSys sys1

-- | The @k = 0@ inductive step: the driver's one-cycle hop.
--
-- > inv(a, c) /\ driver(c) = 0  ==>  inv(isaStep(a), stepSys(c))
--
-- There is no side condition about aliasing stores: 'Core.decode' detects a
-- store overlapping the instruction word being decoded and stalls, so a store
-- that rewrites an instruction in flight is handled by the core rather than
-- assumed away here.
--
-- @driver == 0@ covers two shapes: a running state whose hop retires one
-- instruction, and a halted one. The @IsaHalted@ branch is what says the halted
-- core stands still -- the halted cases of the invariant require the
-- architectural register file and memory to equal the core's, so carrying the
-- same @isa@ across the step asserts that neither changed.
indStepObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> Bool
indStepObligation wr wa ipc sys =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys' = stepSys sys

    premises =
      invAtFree wr wa isa sys
        && driver sys == 0

    conclusion = invAtFree wr wa isa' sys'

-- | The @k = 1@ inductive step: the driver's two-cycle hop.
--
-- The driver also gives the reset state a two-cycle hop, but no obligation
-- covers it: the invariant does not admit the reset state, and
-- 'Proof.Functional.Induction.baseCase' states that hop directly.
indStepObligation1 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> Bool
indStepObligation1 wr wa ipc sys =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys1 = stepSys sys
    sys2 = stepSys sys1

    premises =
      invAtFree wr wa isa sys
        && driver sys == 1

    conclusion = invAtFree wr wa isa' sys2

-- | The @k = 2@ inductive step: the driver's three-cycle hop.
--
-- Unlike the shorter hops, this one can execute an environment instruction.
-- The architectural state then stays at the trapping instruction while the
-- core reaches one of the two halted invariant cases.
indStepObligation2 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> Bool
indStepObligation2 wr wa ipc sys =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys1 = stepSys sys
    sys2 = stepSys sys1
    sys3 = stepSys sys2

    premises =
      invAtFree wr wa isa sys
        && driver sys == 2

    conclusion = invAtFree wr wa isa' sys3

-- | The @k = 3@ inductive step: the driver's four-cycle hop, the longest.
--
-- Stated exactly as 'indStepObligation2', one cycle longer. The @IsaHalted@
-- alternative is kept even though this hop should not be able to trap -- the
-- driver routes environment instructions to a three-cycle hop -- because
-- covering it costs nothing and assuming it away would be an unchecked side
-- argument.
indStepObligation3 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> Bool
indStepObligation3 wr wa ipc sys =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys1 = stepSys sys
    sys2 = stepSys sys1
    sys3 = stepSys sys2
    sys4 = stepSys sys3

    premises =
      invAtFree wr wa isa sys
        && driver sys == 3

    conclusion = invAtFree wr wa isa' sys4
