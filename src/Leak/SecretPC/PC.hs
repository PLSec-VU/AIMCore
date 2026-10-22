{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Leak.SecretPC.PC
  ( obs,
    leak,
    sim,
    circuit,
    proj,
    pcsEqual,
    implementation,
    -- comment them out to disable Pantomime checks for faster compilation
    theory,
    tickStateCorrespondence,
    projectionCoherence,
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
import Data.Monoid (First (..), getFirst)
import qualified Leak.Existence as Existence
import qualified Leak.SecretPC.Leak as Leak
import RegFile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))
import SymFC.Clash.NonInterference qualified as Clash
import SymFC.BuiltIn qualified as SymFC

type SimState = Core.State PubSec

-- {-# ANN theory (Theory $ Base.embeddings <> Clash.embeddings) #-}
theory :: SymFC.Sym SymFC.Bool
theory = do
  let s = undefined
  let i = undefined
  pure $ pantomime s i

pantomime :: Core.State PubSec -> Input PubSec -> SymFC.Bool
pantomime = Clash.simulationNI Clash.SimulationNI
  { observation = obs'
  , implementation = implementation
  , leakage = leak
  , simulator = sim
  , projection = proj
  }

implementation :: Core.State PubSec -> Input PubSec -> (Core.State PubSec, Output PubSec)
implementation = Core.circuit

circuits :: Clash.SimulatorExistNI (Core.State PubSec) () SimState (Input PubSec) Leak.Out (Maybe Address)
circuits = Clash.SimulatorExistNI
  { implementation = second obs' .: implementation
  , leakage = leak
  , projection = proj
  }

-- {-# ANN tickStateCorrespondence (P.Theory $ Base.axioms <> Clash.axioms) #-}
tickStateCorrespondence :: Core.State PubSec -> Input PubSec -> SymFC.Bool
tickStateCorrespondence = Clash.tickStateCorrespondence circuits

-- {-# ANN projectionCoherence (P.Theory $ Base.axioms <> Clash.axioms) #-}
projectionCoherence :: Core.State PubSec -> Input PubSec -> Core.State PubSec -> Input PubSec -> SymFC.Bool
projectionCoherence = Clash.projectionCoherence circuits

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

sim :: SimState -> Leak.Out -> (SimState, Maybe Address)
sim = curry (Existence.simulatorFor circuits id)

circuit ::
  ((), SimState) ->
  Input PubSec ->
  (((), SimState), Maybe Address)
circuit (ts, ss) input = ((ts', ss'), addr)
  where
    (ts', o_leak) = leak ts input
    (ss', addr) = sim ss o_leak

proj' :: SimState -> SimState
proj' s = ss
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

proj :: Core.State PubSec -> ((), SimState)
proj s = ((), proj' s)

simulator ::
  forall m.
  (MonadState ((Core.State PubSec, Output PubSec), Simulate.Mem MEM_SIZE_BYTES) m) =>
  CircuitSim m (Input PubSec) (Leak.State, SimState) (Maybe Address, Maybe Address)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = (Leak.init, Core.init),
      circuitStep = step,
      circuitNext = next
    }
  where
    step ::
      Input PubSec ->
      (Leak.State, SimState) ->
      m ((Leak.State, SimState), (Maybe Address, Maybe Address))
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
      (Leak.State, SimState)
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
  [((Leak.State, SimState), (Maybe Address, Maybe Address), Maybe (Input PubSec))]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'
