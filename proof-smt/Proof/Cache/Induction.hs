-- | The stall lemma, checked symbolically: with a load waiting in writeback and
-- the bus not ready, a cycle of the core is a stutter step
-- ('Proof.Cache.Obligation.stallObligation'). Over the same symbolic states as
-- the functional proof, without its invariant.
module Proof.Cache.Induction
  ( stallStep,
    results,
  )
where

import qualified Core
import Data.Functor.Identity
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.Obligation (stallObligation)
import Proof.Functional.Induction (KState, sysOf)
import Proof.Machine (SysG (..))
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Types
import Prelude hiding (Word)

{-# ANN stallStep (Theory arrayAxioms) #-}
stallStep :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Pantomime.Bool
stallStep ss i ra ma wr =
  Pantomime.boolean $ stallObligation wr (sysState sys) (sysInput sys)
  where
    sys = sysOf ss i ra ma

-- | The verdict, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results = [("stallStep", $(pantomime 'stallStep))]
