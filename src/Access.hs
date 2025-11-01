{-# LANGUAGE DeriveFunctor #-}

module Access where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Data.Functor.Identity
import Data.Maybe
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))

class (Applicative f) => Access f where
  unAccess :: f a -> a
  fromPublic :: f a -> Maybe a
  fromSecret :: f a -> Maybe a

isPublic :: (Access f) => f a -> Bool
isPublic = isJust . fromPublic

isSecret :: (Access f) => f a -> Bool
isSecret = isJust . fromSecret

-- | No secrets here, buddy: unwrap a word. If it's public, we gucci. If it's
-- private, die.
noSecrets :: (Access f) => f a -> (a -> m b) -> m b
noSecrets w m =
  maybe (error "noSecrets: it was secret!") m $ fromPublic w

data PubSec a
  = Public a
  | Secret a
  deriving (Functor, Show, Eq, Generic, NFDataX)

fromPubSec :: a -> PubSec a -> a
fromPubSec _ (Public a) = a
fromPubSec a Secret {} = a

instance Access PubSec where
  unAccess (Public a) = a
  unAccess (Secret a) = a

  fromPublic (Public a) = pure a
  fromPublic _ = empty

  fromSecret (Secret a) = pure a
  fromSecret _ = empty

instance Applicative PubSec where
  pure = Public
  f <*> x
    | isSecret f || isSecret x = Secret $ unAccess f $ unAccess x
    | otherwise = Public $ unAccess f $ unAccess x

instance Monad PubSec where
  (Public x) >>= f = f x
  (Secret x) >>= f = Secret $ unAccess $ f x

instance Access Identity where
  unAccess = runIdentity
  fromPublic = pure . unAccess
  fromSecret = const empty
