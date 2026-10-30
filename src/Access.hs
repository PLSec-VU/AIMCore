module Access where

import Data.Functor.Identity

class (Applicative f) => Access f where
  unAccess :: f a -> a

instance Access Identity where
  unAccess = runIdentity
