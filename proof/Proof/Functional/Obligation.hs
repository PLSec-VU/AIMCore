-- | The proof obligations, in one place.
--
-- Both the symbolic properties ("Proof.Functional.Induction") and the QuickCheck harness
-- ("ProofSpec") go through these definitions, so the two cannot drift apart.
-- The only thing they are allowed to differ in is how the system state is
-- built: symbolic scalars on one side, a generator on the other. That is the
-- input space, not the property.
module Proof.Functional.Obligation
  ( isaAt,
    hopPc,
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
import ISA (IsaStateG (..), IsaState, StepG (..), Step, isaStep, isaStepDecoded, isaRun, isaInstrAt)
import Instruction
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
-- @stateExPc@ when running, @stateFePc@ at startup, and @stateHalt@ once halted.
-- Deriving it from any one of those would make the other two cases
-- unsatisfiable as premises.
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

-- | The architectural PC a hop-aligned state corresponds to.
--
-- The symbolic obligations do not use this -- they quantify @isaPc@ and let the
-- invariant pin it, which is the whole point of quantifying rather than
-- deriving. It exists for the callers that need a concrete architectural state
-- rather than a quantified one: the QuickCheck harness and the leakage
-- projection. It reproduces, per case, the conjunct that pins @isaPc@ in the
-- invariant: the fetch stage at startup, the trapping instruction once halted,
-- the execute stage otherwise.
hopPc :: SysG r m -> Address
hopPc sys@(Sys st _ _)
  | isStartupShape sys = Core.stateFePc st
  | otherwise = case Core.stateHalt st of
      Just (Core.EBreak a) -> a - 4
      Just (Core.Syscall a) -> a - 4
      _ -> Core.stateExPc st

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
    sys' = stepSys sys

    premises =
      invAtFree wr wa isa sys
        && driver sys == 0

    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    conclusion = invAtFree wr wa isa' sys'

-- | The @k = 1@ inductive step: the driver's two-cycle hop.
--
-- One thing differs from @k = 0@ beyond the extra cycle: a startup hop does not
-- retire an instruction, so the architectural state is carried across unchanged.
indStepObligation1 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> Bool
indStepObligation1 wr wa ipc sys =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    s1 = stepSys sys
    s2 = stepSys s1

    premises =
      invAtFree wr wa isa sys
        && driver sys == 1

    isa'
      | isStartupShape sys = isa
      | otherwise = case isaStep isa of
          Next next -> next
          IsaHalted -> isa

    conclusion = invAtFree wr wa isa' s2

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
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2

    premises =
      invAtFree wr wa isa sys
        && driver sys == 2

    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    conclusion = invAtFree wr wa isa' s3

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
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2
    s4 = stepSys s3

    premises =
      invAtFree wr wa isa sys
        && driver sys == 3

    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    conclusion = invAtFree wr wa isa' s4
