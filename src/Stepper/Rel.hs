{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.Rel
  ( Rel (..)
  , left
  , right
  , agree
  ) where

import Control.Comonad (Comonad (..))
import Stepper.Concrete (knownBool)
import SymFC.BuiltIn qualified as SymFC

data Rel a
  = Same a
  | Pair a a

instance Functor Rel where
  fmap f (Same a) = Same (f a)
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Rel where
  pure = Same
  Same f <*> Same a = Same (f a)
  Same f <*> Pair a b = Pair (f a) (f b)
  Pair f g <*> Same a = Pair (f a) (g a)
  Pair f g <*> Pair a b = Pair (f a) (g b)

-- | A two-run value read as a function from the run to its value, with the
-- first run in focus. 'extract' gives the first run's value; it stands for
-- the value only where both runs agree.
instance Comonad Rel where
  extract = left
  duplicate (Same a) = Same (Same a)
  duplicate (Pair a b) = Pair (Pair a b) (Same b)

left :: Rel a -> a
left (Same a) = a
left (Pair a _) = a

right :: Rel a -> a
right (Same a) = a
right (Pair _ b) = b

agree :: Eq a => Bool -> Rel a -> SymFC.Sym (Maybe a)
agree _ (Same a) = pure (Just a)
agree path (Pair a b) = do
  known <- knownBool (a == b)
  case known of
    Just True -> pure (Just a)
    _ -> do
      result <- SymFC.valid $ SymFC.boolean $ not path || a == b
      pure case result of
        SymFC.None -> Just a
        _ -> Nothing
