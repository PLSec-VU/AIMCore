module Leak.SecretPC.PC where

import Core (Input (..), MemAccess (..), Output (..), State (..), Control (..), initInput)
import qualified Core
import Access (PubSec, censor)
import Types
import qualified Leak.SecretPC.Leak as Leak
import qualified Leak.SecretPC.Sim as Sim
import Data.Functor.Identity (Identity)
import Data.Monoid (First(..), getFirst)
import Control.Monad (guard)
import Data.Maybe (isJust)

implementation :: Core.State PubSec -> Input PubSec -> (Core.State PubSec, Output PubSec)
implementation = Core.circuit

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
sim = undefined

circuit ::
  ((), Sim.State) ->
  Input PubSec ->
  (((), Sim.State), Maybe Address)
circuit (ts, ss) input = ((ts', ss'), addr)
  where
    (ts', o_leak) = leak ts input
    (ss', addr) = sim ss o_leak

proj :: (Core.State PubSec, ()) -> ((), Sim.State)
proj (s, _) = ((), ss)
  where
    ss = s { stateMemRes = censor (stateMemRes s),
             stateMemVal = censor (stateMemVal s),
             stateWbRes = censor (stateWbRes s),
             stateCtrl = (stateCtrl s) { ctrlMeRegFwd = fmap (\(idx, val) -> (idx, censor val)) (ctrlMeRegFwd (stateCtrl s)),
                                         ctrlWbRegFwd = fmap (\(idx, val) -> (idx, censor val)) (ctrlWbRegFwd (stateCtrl s)) }
           }
