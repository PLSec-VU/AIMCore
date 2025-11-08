{-# LANGUAGE StandaloneDeriving #-}

module Leak.SecretPC.Sim
  ( circuit,
    init,
    proj,
    State,
  )
where

import Access
import Control.Monad
import qualified Core
import Data.Bifunctor
import Data.Monoid
import Types
import Prelude hiding (init)

type Input = Core.Input PubSec

type State = Core.State PubSec

type Output = Core.Output PubSec

type SimM = Core.CPUM PubSec

deriving instance Eq Leak.SecretPC.Sim.State

init :: State
init = Core.init

circuit :: State -> Input -> (State, Maybe Address)
circuit s i = bimap proj obs' $ Core.circuit s i
  where
    obs' :: Output -> Maybe Address
    obs' o_sim = do
      mem <- getFirst $ Core.outMem o_sim
      guard $ Core.memIsInstr mem
      pure $ Core.memAddress mem

proj :: State -> State
proj s = ss
  where
    ss =
      s
        { Core.stateMemRes = censor (Core.stateMemRes s),
          Core.stateMemVal = censor (Core.stateMemVal s),
          Core.stateWbRes = censor (Core.stateWbRes s),
          Core.stateCtrl =
            (Core.stateCtrl s)
              { Core.ctrlMeRegFwd =
                  fmap
                    (\(idx, val) -> (idx, censor val))
                    (Core.ctrlMeRegFwd (Core.stateCtrl s)),
                Core.ctrlWbRegFwd =
                  fmap
                    (\(idx, val) -> (idx, censor val))
                    (Core.ctrlWbRegFwd (Core.stateCtrl s))
              }
        }
