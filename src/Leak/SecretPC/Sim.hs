{-# LANGUAGE StandaloneDeriving #-}

module Leak.SecretPC.Sim where

import Core
import Data.Functor.Identity (Identity(Identity))

type State = Core.State Identity

deriving instance Eq Leak.SecretPC.Sim.State