{-# LANGUAGE StandaloneDeriving #-}

module Leak.SecretPC.Sim
  ( circuit,
    init,
    State,
  )
where

import qualified Core
import Data.Functor.Identity (Identity (Identity))
import Prelude hiding (init)

type Input = Core.Input Identity

type State = Core.State Identity

type Output = Core.Output Identity

type SimM = Core.CPUM Identity

deriving instance Eq Leak.SecretPC.Sim.State

init :: State
init = Core.init

circuit :: State -> Input -> (State, Output)
circuit = Core.circuit
