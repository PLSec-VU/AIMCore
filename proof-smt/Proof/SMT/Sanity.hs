-- | Sanity check that the Pantomime plugin is correctly wired into this
-- project. The properties below are checked at compile time by the plugin,
-- which discharges them via Z3.
--
-- IMPORTANT: as of pantomime 1821a71, an annotation on its own does NOT fail
-- the build when a property is invalid -- the plugin just logs the
-- counterexample and compilation succeeds. To actually observe the result you
-- must splice in @$(pantomime 'name)@, which the plugin rewrites to
-- @Nothing@ (valid) or @Just counterexample@ (invalid). See 'results'.
--
-- NOTE: this module -- and every other property the plugin checks -- only
-- builds at @-O1@ or above. Under @stack build --fast@ (@-O0@) GHC creates no
-- unfoldings, so the plugin cannot see through @$@ or @Pantomime.boolean@ and
-- stops with @Unbound variable in symbolise@, which fails the build rather
-- than reporting a failed proof. Use a plain @stack build@ for anything that
-- touches the proof.
module Proof.SMT.Sanity
  ( deMorgan
  , doubling
  , bogus
  , results
  ) where

import Pantomime (Theory (..), pantomime)
import qualified Pantomime.Base as Base
import qualified Pantomime.BuiltIn as Pantomime

-- | The example from the Pantomime README: de Morgan over all booleans.
--
-- NOTE: unlike the README, this needs 'Base.axioms'. GHC compiles @(==) \@Bool@
-- down to the @dataToTagSmall#@ primop, which only 'Base.axioms' maps onto a
-- Pantomime primitive.
{-# ANN deMorgan (Theory Base.axioms) #-}
deMorgan :: Bool -> Bool -> Pantomime.Bool
deMorgan x y = Pantomime.boolean $ (not x && not y) == not (x || y)

-- | The example from the Pantomime install guide, which additionally exercises
-- the 'base' axioms (i.e. the embedding of 'Int' into the theory of bitvectors).
{-# ANN doubling (Theory Base.axioms) #-}
doubling :: Int -> Pantomime.Bool
doubling x = Pantomime.boolean $ x + x == 2 * x

-- | Negative control: this is false (e.g. at @x = -1@). It is kept deliberately
-- so that 'results' demonstrates the checker reporting a counterexample rather
-- than silently accepting everything.
{-# ANN bogus (Theory Base.axioms) #-}
bogus :: Int -> Pantomime.Bool
bogus x = Pantomime.boolean $ x + x == 3 * x

-- | Verification verdicts, filled in by the plugin at compile time.
-- 'Nothing' means valid; 'Just' carries the counterexample.
results :: [(String, Maybe String)]
results =
  [ ("deMorgan", $(pantomime 'deMorgan))
  , ("doubling", $(pantomime 'doubling))
  , ("bogus"   , $(pantomime 'bogus))
  ]
