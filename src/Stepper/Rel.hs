{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.Rel
  ( Rel (..)
  , left
  , right
  , agree
  ) where

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

left :: Rel a -> a
left (Same a) = a
left (Pair a _) = a

right :: Rel a -> a
right (Same a) = a
right (Pair _ b) = b

agree :: Eq a => Bool -> Rel a -> SymFC.Sym (Maybe a)
agree _ (Same a) = pure (Just a)
agree path (Pair a b)
  | knownBool (a == b) == Just True = pure (Just a)
  | otherwise = do
      result <- SymFC.valid $ SymFC.boolean $ not path || a == b
      pure case result of
        SymFC.None -> Just a
        _ -> Nothing
