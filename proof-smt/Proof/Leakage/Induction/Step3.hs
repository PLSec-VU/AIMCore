-- | The leakage step for the four-cycle hop: a store hazard with a memory
-- instruction in execute, a load-use hazard, or memory instructions in all three
-- older stages.
--
-- One property per module, so that a recompile or a failed check of one hop
-- length does not cost the solver time of the others.
module Proof.Leakage.Induction.Step3
  ( leakStep3,
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

{-# ANN leakStep3 (Theory arrayAxioms) #-}
leakStep3 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep3 ss i ra ma wr wa =
  Pantomime.boolean $ leakObligationAt 3 wr wa (sysOf ss i ra ma)

-- | The verdict, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results = [("leakStep3", $(pantomime 'leakStep3))]
