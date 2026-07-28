-- | The proof obligation itself, in one place.
--
-- Both the symbolic property ('Verify.indStep0') and the QuickCheck harness
-- ('ProofSpec') go through these definitions, so the two cannot drift apart.
-- They previously had separate copies of the premise and conclusion, and had
-- already diverged in the @IsaHalted@ branch without anyone noticing -- which
-- made it impossible to argue about why one found counterexamples the other
-- did not.
--
-- The only thing the two are allowed to differ in is how the system state is
-- built: symbolic scalars on one side, a generator on the other. That is the
-- input space, not the property.
module Obligation
  ( isaOfG,
    indStepObligation,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Driver (driver)
import ISAStep
import Invariant
import Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The architectural state the invariant claims @sys@ corresponds to.
--
-- Derived from @sys@ by the flush rather than taken as a further input: the
-- invariant's container equalities then hold by construction, so assuming the
-- invariant at one witness reduces to assuming its scalar conjuncts -- which is
-- the full container invariant, not a weakening of it.
isaOfG :: (RegFileOps r, MemOps m) => InvConfig -> SysG r m -> IsaStateG r m
isaOfG cfg (Sys st inp mem) =
  IsaState {isaPc = Core.stateExPc st, isaRegFile = frf, isaMem = fm}
  where
    (fm, frf) =
      flushMeStage
        (jumpsWriteRdInMe cfg)
        (Core.stateMeInstr st)
        (runIdentity (Core.stateMeRes st))
        (Core.stateMeAddr st)
        ( flushWbStage
            (Core.stateWbInstr st)
            (runIdentity (Core.stateWbRes st))
            (runIdentity (Core.inputMem inp))
            (mem, Core.stateRegFile st)
        )

-- | The k = 0 inductive step, at witness register @wr@ and witness byte
-- address @wa@:
--
-- > inv(a, c) /\ driver(c) = 0  ==>  inv(isaStep(a), stepSys(c))
--
-- The no-self-modifying-store side condition is /assumed/ of both the pre- and
-- post-state, which rules out transitions that create an aliasing store rather
-- than obliging us to show none can arise. See 'Invariant.noStoreAlias'.
indStepObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG proposed sys
    sys' = stepSys sys

    premises =
      invAtFree proposed wr wa isa sys
        && driver sys == 0
        && noStoreAlias sys
        && noStoreAlias sys'

    conclusion =
      case isaStep isa of
        Next isa' -> invAtFree proposed wr wa isa' sys'
        -- driver == 0 excludes environment instructions, so the ISA cannot
        -- halt on this hop.
        IsaHalted -> True
