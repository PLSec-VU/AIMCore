{-# LANGUAGE UndecidableInstances #-}

module Leak.SecretPC.PC
  ( obs,
    leak,
    sim,
    circuit,
    proj,
    pcsEqual,
    implementation,
    -- comment them out to disable Pantomime checks for faster compilation
    -- theory,
    -- tickStateCorrespondence,
    -- projectionCoherence,
  )
where

import Access (PubSec, censor)
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad (guard)
import Control.Monad.State
import Core (Control (..), Input (..), MemAccess (..), Output (..), initInput)
import qualified Core
import Data.Bifunctor (second)
import Data.Composition
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)
import Data.Monoid (First (..), getFirst)
import Instruction (Instruction)
import qualified Leak.SecretPC.Leak as Leak
import qualified Leak.SecretPC.Sim as Sim
import qualified Pantomime as P
import qualified Pantomime.Base as Base
import qualified Pantomime.Clash as Clash
import RegFile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

{-# ANN theory (P.Theory $ Base.axioms <> Clash.axioms) #-}
theory :: Core.State PubSec -> Input PubSec -> Bool
theory =
  P.pantomime
    P.Pantomime
      { observation = obs',
        implementation = implementation,
        leakage = leak,
        simulator = sim,
        projection = proj
      }

implementation :: Core.State PubSec -> Input PubSec -> (Core.State PubSec, Output PubSec)
implementation = Core.circuit

circuits :: P.NonInterference (Core.State PubSec) () Sim.State (Input PubSec) Leak.Out (Maybe Address)
circuits =
  P.NonInterference
    { P.implementation = second obs' .: implementation,
      P.leakage = leak,
      P.projection = proj
    }

{-# ANN tickStateCorrespondence (P.Theory $ Base.axioms <> Clash.axioms) #-}
tickStateCorrespondence :: Core.State PubSec -> Input PubSec -> Bool
tickStateCorrespondence = P.tickStateCorrespondence circuits

{-# ANN projectionCoherence (P.Theory $ Base.axioms <> Clash.axioms) #-}
projectionCoherence :: Core.State PubSec -> Input PubSec -> Core.State PubSec -> Input PubSec -> Bool
projectionCoherence = P.projectionCoherence circuits

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
proj s = ((), Sim.proj s)

simulator ::
  forall m.
  (MonadState ((Core.State PubSec, Output PubSec), Simulate.Mem MEM_SIZE_BYTES) m) =>
  CircuitSim m (Input PubSec) (Leak.State, Sim.State) (Maybe Address, Maybe Address)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = (Leak.init, Sim.init),
      circuitStep = step,
      circuitNext = next
    }
  where
    step ::
      Input PubSec ->
      (Leak.State, Sim.State) ->
      m ((Leak.State, Sim.State), (Maybe Address, Maybe Address))
    step i s = do
      ((s_sim, _), mem) <- get
      let (res_sim@(_, o_sim), mem') = runState (circuitStep Simulate.simulator i s_sim) mem
      put (res_sim, mem')
      let (s', o) = circuit s i
      pure (s', (o, obs' o_sim))

    next :: (Maybe Address, Maybe Address) -> m (Maybe (Input PubSec))
    next (_o, _addr_sim) = do
      ((_, o_sim), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_sim) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  ( CircuitSim
      (State ((Core.State PubSec, Output PubSec), Simulate.Mem MEM_SIZE_BYTES))
      (Input PubSec)
      (Leak.State, Sim.State)
      (Maybe Address, Maybe Address) ->
    State ((Core.State PubSec, Output PubSec), Simulate.Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f prog = evalState (f Leak.SecretPC.PC.simulator) s
  where
    s = ((Core.init, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  Vec PROG_SIZE Word ->
  [((Leak.State, Sim.State), (Maybe Address, Maybe Address), Maybe (Input PubSec))]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'
