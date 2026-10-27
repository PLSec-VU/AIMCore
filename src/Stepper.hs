module Stepper
  ( IsaSys
  , IsaSysStep
  , Fuel (..)
  ) where

import ISA (IsaStateG, StepG)
import Stepper.Memory (Memory, RegFile)

type IsaSys = IsaStateG RegFile (Memory 32 8)

type IsaSysStep = StepG RegFile (Memory 32 8)

data Fuel = Done | More Fuel
