{-# LANGUAGE UndecidableInstances #-}

module Leak.SecretPC.PC
  ( obs,
    leak,
    sim,
    circuit,
    proj,
    implementation,
    theoryNonInterference0,
    theoryNonInterference1,
  )
where

import Access (PubSec, censor)
import Control.Monad (guard)
import Core (Control (..), Input (..), MemAccess (..), Output (..), State (..), initInput)
import qualified Core
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)
import Data.Monoid (First (..), getFirst)
import qualified Leak.SecretPC.Leak as Leak
import qualified Leak.SecretPC.Sim as Sim
import Types
import qualified Pantomime as P
import qualified Pantomime.Clash as Clash
import qualified Pantomime.Base as Base
import Data.Bifunctor (second)
import Data.Composition

{-# ANN theory (P.Theory $ Base.axioms <> Clash.axioms) #-}
theory :: Core.State PubSec -> Input PubSec -> Bool
theory = P.pantomime P.Pantomime
  { observation = obs'
  , implementation = implementation
  , leakage = leak
  , simulator = sim
  , projection = proj
  }

implementation :: Core.State PubSec -> Input PubSec -> (Core.State PubSec, Output PubSec)
implementation = Core.circuit

circuits :: P.NonInterference (Core.State PubSec) () Sim.State (Input PubSec) Leak.Out (Maybe Address)
circuits = P.NonInterference
  { P.implementation = second obs' .: implementation
  , P.leakage = leak
  , P.projection = proj
  }

{-# ANN theoryNonInterference0 (P.Theory $ Base.axioms <> Clash.axioms) #-}
theoryNonInterference0 :: Core.State PubSec -> Input PubSec -> Bool
theoryNonInterference0 = P.nonInterference0 circuits

{-# ANN theoryNonInterference1 (P.Theory $ Base.axioms <> Clash.axioms) #-}
theoryNonInterference1 :: Core.State PubSec -> Input PubSec -> Core.State PubSec -> Input PubSec -> Bool
theoryNonInterference1 = P.nonInterference1 circuits

stateless :: (a -> b) -> () -> a -> ((), b)
stateless f _ x = ((), f x)

obs :: () -> Output PubSec -> ((), Maybe Address)
obs = stateless obs'

obs' :: Output PubSec -> Maybe Address
obs' o_sim = do
  mem <- getFirst $ outMem o_sim
  guard $ memIsInstr mem
  pure $ memAddress mem

leak :: () -> Input PubSec -> ((), Leak.Out)
leak = Leak.circuit

sim :: Sim.State -> Leak.Out -> (Sim.State, Maybe Address)
sim = Sim.circuit

circuit ::
  ((), Sim.State) ->
  Input PubSec ->
  (((), Sim.State), Maybe Address)
circuit (ts, ss) input = ((ts', ss'), addr)
  where
    (ts', o_leak) = leak ts input
    (ss', addr) = sim ss o_leak

proj :: Core.State PubSec -> ((), Sim.State)
proj s = ((), ss)
  where
    ss =
      s
        { stateMemRes = censor (stateMemRes s),
          stateMemVal = censor (stateMemVal s),
          stateWbRes = censor (stateWbRes s),
          stateCtrl =
            (stateCtrl s)
              { ctrlMeRegFwd = fmap (\(idx, val) -> (idx, censor val)) (ctrlMeRegFwd (stateCtrl s)),
                ctrlWbRegFwd = fmap (\(idx, val) -> (idx, censor val)) (ctrlWbRegFwd (stateCtrl s))
              }
        }
