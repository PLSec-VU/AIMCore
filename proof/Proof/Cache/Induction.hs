-- | The cache's obligations, checked symbolically.
--
-- 'stallStep': with a load waiting in writeback and the bus not ready, a cycle
-- of the core is a stutter step ('Proof.Cache.Obligation.stallObligation').
-- 'waitStep': after a data read the core waits
-- ('Proof.Cache.Obligation.waitObligation'). Together, the core is patient. Both
-- over the same symbolic states as the functional proof, without its invariant.
-- The cache's own server proof is in "Proof.Cache.Induction4K".
module Proof.Cache.Induction
  ( stallStep,
    waitStep,
    results,
  )
where

import qualified Core
import Data.Functor.Identity
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.Obligation (stallObligation, waitObligation)
import Proof.Machine (SysG (..))
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Proof.SMT.System (KState, sysOf)
import Types
import Prelude hiding (Word)

{-# ANN stallStep (Theory arrayAxioms) #-}
stallStep :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Pantomime.Bool
stallStep ss i ra ma wr =
  Pantomime.boolean $ stallObligation wr (sysState sys) (sysInput sys)
  where
    sys = sysOf ss i ra ma

{-# ANN waitStep (Theory arrayAxioms) #-}
waitStep :: KState -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
waitStep ss i ra ma =
  Pantomime.boolean $ waitObligation (sysState sys) (sysInput sys)
  where
    sys = sysOf ss i ra ma

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("stallStep", $(pantomime 'stallStep)),
    ("waitStep", $(pantomime 'waitStep))
  ]
