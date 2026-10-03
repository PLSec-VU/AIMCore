-- | The leakage step for the three-cycle hop: an environment instruction, a
-- taken jump, a store hazard with a non-memory instruction in execute, or memory
-- instructions in both older stages.
--
-- One property per module, so that a recompile or a failed check of one hop
-- length does not cost the solver time of the others.
module Proof.Leakage.Induction.Step2
  ( leakStep2,
    results,
  )
where

import qualified Core
import Data.Functor.Identity
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Functional.Induction (KState, sysOf)
import Proof.Leakage.Obligation (leakObligationAt)
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Types
import Prelude hiding (Word)

{-# ANN leakStep2 (Theory arrayAxioms) #-}
leakStep2 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep2 ss i ra ma wr wa =
  Pantomime.boolean $ leakObligationAt 2 wr wa (sysOf ss i ra ma)

-- | The verdict, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results = [("leakStep2", $(pantomime 'leakStep2))]
