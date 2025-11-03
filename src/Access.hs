{-# LANGUAGE DeriveFunctor #-}

module Access where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Data.Functor.Identity
import Data.Maybe
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))

class (Applicative f) => Access f where
  -- | Access the underlying value, regardless of secrecy.
  unAccess :: f a -> a
  -- | Try to extract a public value. Returns 'Nothing' if the value is secret.
  fromPublic :: f a -> Maybe a

isPublic :: (Access f) => f a -> Bool
isPublic = isJust . fromPublic

-- | No secrets here, buddy: unwrap a word. If it's public, we gucci. If it's
-- private, die.
noSecrets :: (Applicative m, Access f) => f a -> b -> (a -> m b) -> m b
noSecrets w a m = case fromPublic w of
  Just v -> m v
  Nothing -> pure a

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

instance Applicative PubSec where
  pure = Public
  (Public f) <*> (Public x) = Public (f x)
  f <*> x = Secret (unAccess f (unAccess x))

instance Monad PubSec where
  (Public x) >>= f = f x
  (Secret x) >>= f = Secret $ unAccess $ f x

instance Access Identity where
  unAccess = runIdentity
  fromPublic = pure . unAccess
