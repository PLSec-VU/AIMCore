-- | The proof obligations, in one place.
--
-- Both the symbolic properties ("Proof.Core.Functional.Solver") and the
-- QuickCheck harness ("ProofSpec") go through these definitions, so the two
-- cannot drift apart. The only thing they are allowed to differ in is how the
-- system state is built: symbolic scalars on one side, a generator on the
-- other. That is the input space, not the property.
module Proof.Core.Functional.Obligation
  ( baseCase,
    indStep0,
    indStep1,
    indStep2,
    indStep3,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Proof.Core.Functional.Driver (driver)
import Core.ISA (IsaStateG (..), StepG (..), isaStep)
import Proof.Core.Functional.Invariant
import Memory.Types (MemOps (..))
import Proof.Core.Functional.Machine
import Core.RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The base case ----------------------------------------------------------------

-- | The base case: the invariant holds once the initial state has taken its
-- first hop.
--
-- Without this the four steps below say only that the invariant is /preserved/,
-- which is vacuous if it never holds anywhere. Together they give the theorem:
-- the invariant relates the core to the ISA at every state the driver lands on
-- after the start.
--
-- The initial state itself is not a case of the invariant. 'Core.Pipeline.init'
-- has nothing in the pipeline, so the driver gives it a two-cycle hop that
-- fetches and decodes the first instruction without executing anything. This
-- property states that hop directly: the driver does assign it two cycles, and
-- after them the running case relates the core to the ISA's /initial/ state --
-- zero ISA steps. Handling it here rather than as a case of the invariant is
-- what lets every inductive step retire exactly one instruction.
--
-- The start PC, the register file and memory are parameters, used for both the
-- core and the ISA; every other field comes from 'Core.Pipeline.init' itself,
-- so the initial pipeline shape cannot drift from the real one. Taking them as
-- parameters makes the base case cover any loaded program and any initial PC
-- and register contents, such as the ELF entry point and stack pointer the ELF
-- runners start the core with.
--
-- Note what sharing them costs: memory and the register file are the same
-- values on both sides, and the first hop writes neither, so the invariant's
-- two container equalities hold by construction here and this property alone
-- would not notice if the core's initial register file and the ISA's
-- ('Core.RegFile.initRF') disagreed. The concrete test in "ProofSpec" closes
-- that gap -- it runs on the real 'Vec'-backed state, where both files are
-- built independently.
baseCase ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> r -> m -> Bool
baseCase wr wa pc rf mem =
  driver sys == 1 && invAt wr wa isa sys'
  where
    isa = IsaState {isaPc = pc, isaRegFile = rf, isaMem = mem}
    sys = initSys pc rf mem
    sys' = stepSysN 1 sys

-- The inductive steps ----------------------------------------------------------

-- | The @k = 0@ inductive step: the driver's one-cycle hop.
--
-- > inv(a, c) /\ driver(c) = 0  ==>  inv(isaStep(a), stepSys(c))
--
-- There is no side condition about aliasing stores: 'Core.Pipeline.decode'
-- detects a store overlapping the instruction word being decoded and stalls, so
-- a store that rewrites an instruction in flight is handled by the core rather
-- than assumed away here.
--
-- @driver == 0@ covers two shapes: a running state whose hop retires one
-- instruction, and a halted one. The @IsaHalted@ branch is what says the halted
-- core stands still -- the halted cases of the invariant require the
-- architectural register file and memory to equal the core's, so carrying the
-- same @isa@ across the step asserts that neither changed.
indStep0 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStep0 wr wa sys =
  not premises || conclusion
  where
    isa = isaOf sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys' = stepSys sys

    premises =
      invAt wr wa isa sys
        && driver sys == 0

    conclusion = invAt wr wa isa' sys'

-- | The @k = 1@ inductive step: the driver's two-cycle hop.
--
-- The driver also gives the reset state a two-cycle hop, but no obligation
-- covers it: the invariant does not admit the reset state, and
-- 'Proof.Core.Functional.Solver.baseCase' states that hop directly.
indStep1 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStep1 wr wa sys =
  not premises || conclusion
  where
    isa = isaOf sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys' = stepSysN 1 sys

    premises =
      invAt wr wa isa sys
        && driver sys == 1

    conclusion = invAt wr wa isa' sys'

-- | The @k = 2@ inductive step: the driver's three-cycle hop.
--
-- Unlike the shorter hops, this one can execute an environment instruction.
-- The architectural state then stays at the trapping instruction while the
-- core reaches one of the two halted invariant cases.
indStep2 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStep2 wr wa sys =
  not premises || conclusion
  where
    isa = isaOf sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys' = stepSysN 2 sys

    premises =
      invAt wr wa isa sys
        && driver sys == 2

    conclusion = invAt wr wa isa' sys'

-- | The @k = 3@ inductive step: the driver's four-cycle hop, the longest.
--
-- Stated exactly as 'indStep2', one cycle longer. The @IsaHalted@
-- alternative is kept even though this hop should not be able to trap -- the
-- driver routes environment instructions to a three-cycle hop -- because
-- covering it costs nothing and assuming it away would be an unchecked side
-- argument.
indStep3 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStep3 wr wa sys =
  not premises || conclusion
  where
    isa = isaOf sys
    isa' = case isaStep isa of
      Next next -> next
      IsaHalted -> isa

    sys' = stepSysN 3 sys

    premises =
      invAt wr wa isa sys
        && driver sys == 3

    conclusion = invAt wr wa isa' sys'

-- Helpers ----------------------------------------------------------------------

-- | A hop of the system: @n + 1@ steps, the cycle the hop starts with and the
-- @n@ cycles the driver counts after it. Recursive on @n@, which each step
-- fixes to a literal.
stepSysN :: (RegFileOps r, MemOps m) => Int -> SysG r m -> SysG r m
stepSysN n sys
  | n == 0 = stepSys sys
  | otherwise = stepSysN (n - 1) (stepSys sys)
