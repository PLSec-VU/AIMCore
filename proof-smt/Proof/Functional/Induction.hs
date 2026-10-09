-- | The inductive steps of the refinement proof, checked symbolically.
--
-- 'baseCase' says the invariant holds once the reset state has taken its first
-- hop. Then one property per driver
-- delay: if the invariant relates @(isa, sys)@ and the driver says the hop takes
-- @k + 1@ cycles, then after those cycles (and one ISA step, where the hop
-- retires an instruction) the invariant relates them again. Base case plus the
-- four steps is the whole refinement theorem.
--
-- Checking one @k@ at a time keeps the number of unrolled cycles concrete,
-- which sidesteps Pantomime's termination check: @stepSysN (driver sys + 1)@
-- would recurse on a symbolic count.
--
-- Each property is checked by the plugin at compile time and spliced into
-- 'results': 'Nothing' when valid, @'Just' counterexample@ when not. The
-- statements themselves live in "Proof.Functional.Obligation", shared with the QuickCheck
-- harness so the two cannot drift.
--
-- The pipeline state is passed as a 'KState' plus SMT-array register file and
-- memory; see "Proof.SMT.System".
module Proof.Functional.Induction
  ( arrRoundTrip,
    shiftsSane,
    baseCase,
    indStep,
    indStep1,
    indStep2,
    indStep3,
    results,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Functional.Obligation
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Proof.SMT.System
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Sanity probes for the trusted embeddings -------------------------------------
--
-- The term axioms in "Proof.SMT.Axioms" replace Haskell functions by hand-written SMT
-- counterparts, so they are trusted, not proved. These two probes check each
-- embedding against facts a broken one would get wrong.

-- | The register-file array embedding: a read after a write at the same index
-- gives the written value.
{-# ANN arrRoundTrip (Theory arrayAxioms) #-}
arrRoundTrip :: RegArr -> RegIdx -> Word -> Pantomime.Bool
arrRoundTrip a i v = Pantomime.boolean $ loadRA (storeRA a i v) i == v

-- | The shift embeddings: identities that would fail if the three shifts were
-- mixed up, the zero-extension of the amount were wrong, or the arithmetic
-- shift lost its sign.
{-# ANN shiftsSane (Theory arrayAxioms) #-}
shiftsSane :: Word -> Pantomime.Bool
shiftsSane x =
  Pantomime.boolean $
    Core.sllWord x 0 == x
      && Core.srlWord x 0 == x
      && Core.sraWord x 0 == x
      && Core.sllWord x 1 == x + x
      && Core.srlWord x 31 == (if sign == 1 then 1 else 0)
      && Core.sraWord x 31 == (if sign == 1 then 0xFFFFFFFF else 0)
  where
    sign = slice d31 d31 x

-- The base case ----------------------------------------------------------------

{-# ANN baseCase (Theory arrayAxioms) #-}
baseCase :: RegArr -> MemArr -> RegIdx -> Address -> Address -> Pantomime.Bool
baseCase ra ma wr wa pc =
  Pantomime.boolean $ baseCaseObligation (RegArrF ra) ma wr wa pc

-- The inductive steps ----------------------------------------------------------

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStep (Theory arrayAxioms) #-}
indStep :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> Pantomime.Bool
indStep ss i ra ma wr wa ipc =
  Pantomime.boolean $ indStepObligation wr wa ipc (sysOf ss i ra ma)

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStep1 (Theory arrayAxioms) #-}
indStep1 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> Pantomime.Bool
indStep1 ss i ra ma wr wa ipc =
  Pantomime.boolean $ indStepObligation1 wr wa ipc (sysOf ss i ra ma)

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStep2 (Theory arrayAxioms) #-}
indStep2 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> Pantomime.Bool
indStep2 ss i ra ma wr wa ipc =
  Pantomime.boolean $ indStepObligation2 wr wa ipc (sysOf ss i ra ma)

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStep3 (Theory arrayAxioms) #-}
indStep3 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> Pantomime.Bool
indStep3 ss i ra ma wr wa ipc =
  Pantomime.boolean $ indStepObligation3 wr wa ipc (sysOf ss i ra ma)

results :: [(String, Maybe String)]
results =
  [ ("arrRoundTrip", $(pantomime 'arrRoundTrip)),
    ("shiftsSane", $(pantomime 'shiftsSane)),
    ("baseCase", $(pantomime 'baseCase)),
    ("indStep", $(pantomime 'indStep)),
    ("indStep1", $(pantomime 'indStep1)),
    ("indStep2", $(pantomime 'indStep2)),
    ("indStep3", $(pantomime 'indStep3))
  ]
