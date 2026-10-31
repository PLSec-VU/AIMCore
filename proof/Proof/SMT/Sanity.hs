{-# LANGUAGE ImportQualifiedPost #-}

-- | Sanity check that the SymFC plugin is correctly wired into this project.
-- The properties below are checked at compile time by the plugin, which
-- discharges them via Z3, and a property that halts fails the build.
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
  ) where

import SymFC (SymFC (..))
import SymFC.Base qualified as Base
import SymFC.BuiltIn (Sym)
import SymFC.BuiltIn qualified as SymFC

-- | The example from the Pantomime README: de Morgan over all booleans.
--
-- NOTE: unlike the README, this needs 'Base.axioms'. GHC compiles @(==) \@Bool@
-- down to the @dataToTagSmall#@ primop, which only 'Base.axioms' maps onto a
-- Pantomime primitive.
{-# ANN deMorgan (SymFC ['Base.embeddings]) #-}
deMorgan :: Sym ()
deMorgan = do
  x <- SymFC.fresh
  y <- SymFC.fresh
  let prop = (not x && not y) == not (x || y)
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.None -> pure ()
    _ -> SymFC.halt

-- | The example from the Pantomime install guide, which additionally exercises
-- the 'base' axioms (i.e. the embedding of 'Int' into the theory of bitvectors).
{-# ANN doubling (SymFC ['Base.embeddings]) #-}
doubling :: Sym ()
doubling = do
  x <- SymFC.fresh @Int
  let prop = x + x == 2 * x
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.None -> pure ()
    _ -> SymFC.halt

-- | Negative control: this is false (e.g. at @x = -1@), so the build passes
-- only if the checker finds a counterexample rather than silently accepting
-- everything.
{-# ANN bogus (SymFC ['Base.embeddings]) #-}
bogus :: Sym ()
bogus = do
  x <- SymFC.fresh @Int
  let prop = x + x == 3 * x
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.Instance -> pure ()
    _ -> SymFC.halt
