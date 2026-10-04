-- | The leakage base case: the first hop out of reset
-- ('Proof.Leakage.Obligation.leakBaseObligation'), for the reset states of any
-- two programs and register files.
module Proof.Leakage.Induction.Base
  ( leakBase,
    results,
  )
where

import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Functional.Induction (resetSys)
import Proof.Leakage.Obligation (leakBaseObligation)
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Types

{-# ANN leakBase (Theory arrayAxioms) #-}
leakBase :: RegArr -> MemArr -> RegArr -> MemArr -> RegIdx -> Pantomime.Bool
leakBase ra ma rb mb wr =
  Pantomime.boolean $ leakBaseObligation wr (resetSys ra ma) (resetSys rb mb)

-- | The verdict, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results = [("leakBase", $(pantomime 'leakBase))]
