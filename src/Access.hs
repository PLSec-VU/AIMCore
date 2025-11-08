{-# LANGUAGE DeriveFunctor #-}

module Access where

import Clash.Prelude hiding (Ordering (..), Word, init, lift)
import Data.Functor.Identity
import Data.Maybe
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))

class (Applicative f) => Access f where
  -- | Access the underlying value, regardless of secrecy.
  unAccess :: f a -> a
  -- | Try to extract a public value. Returns 'Nothing' if the value is secret.
  fromPublic :: f a -> Maybe a
  -- | Create a secret value if the boolean is True, otherwise create a public value
  conditionalSecret :: Bool -> a -> f a

isPublic :: (Access f) => f a -> Bool
isPublic = isJust . fromPublic

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
  
  conditionalSecret True a = Secret a
  conditionalSecret False a = Public a

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
  conditionalSecret _ = pure

censor :: (Access f, Default a) => PubSec a -> f a
censor x = conditionalSecret (not $ isPublic x) (fromPubSec def x)
