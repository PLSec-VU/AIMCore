{-# LANGUAGE StandaloneDeriving #-}

module Leak.SecretPC.Sim
  ( circuit,
    init,
    State,
  )
where

import Control.Monad
import qualified Core
import Data.Bifunctor
import Data.Functor.Identity (Identity (Identity))
import Data.Monoid
import Types
import Prelude hiding (init)
import Access

type Input = Core.Input Identity

type State = Core.State PubSec

type Output = Core.Output Identity

type SimM = Core.CPUM Identity

deriving instance Eq Leak.SecretPC.Sim.State

init :: State
init = Core.init

circuit :: State -> Input -> (State, Maybe Address)
circuit = undefined -- TODO
-- circuit s i = second obs' $ Core.circuit s i
--   where
--     obs' :: Output -> Maybe Address
--     obs' o_sim = do
--       mem <- getFirst $ Core.outMem o_sim
--       guard $ Core.memIsInstr mem
--       pure $ Core.memAddress mem
-- 