{-# LANGUAGE UndecidableInstances #-}

module Leak.PC.PC
  ( obs,
    leak,
    sim,
    circuit,
    proj,
    Leak.PC.PC.simulator,
    runSimulator,
    watchSim,
    pcsEqual,
    implementation,
    -- theory,
    -- stateProjectionPreservation,
    -- leakageDeterminism,
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Core (Input (..), MemAccess (..), Output (..), initInput)
import qualified Core
import Data.Functor.Identity
import Data.Maybe (isJust)
import Data.Monoid
import Instruction (Instruction)
import qualified Leak.PC.Leak as Leak
import qualified Leak.PC.Sim as Sim
import RegFile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))
import qualified Pantomime as P
import qualified Pantomime.Clash as Clash
import qualified Pantomime.Base as Base
import Data.Bifunctor (second)
import Data.Composition

{-# ANN theory (P.Theory $ Base.axioms <> Clash.axioms) #-}
theory :: Core.State Identity -> Input Identity -> Bool
theory = P.pantomime P.Pantomime
  { observation = obs'
  , implementation = implementation
  , leakage = leak
  , simulator = sim
  , projection = proj
  }

implementation :: Core.State Identity -> Input Identity -> (Core.State Identity, Output Identity)
implementation = Core.circuit

circuits :: P.NonInterference   (Core.State Identity)   Leak.State   Sim.State   (Input Identity)   Leak.Out   (Maybe Address)
circuits = P.NonInterference
  { P.implementation = second obs' .: implementation
  , P.leakage = leak
  , P.projection = proj
  }

{-# ANN stateProjectionPreservation (P.Theory $ Base.axioms <> Clash.axioms) #-}
stateProjectionPreservation :: Core.State Identity -> Input Identity -> Bool
stateProjectionPreservation = P.stateProjectionPreservation circuits

{-# ANN leakageDeterminism (P.Theory $ Base.axioms <> Clash.axioms) #-}
leakageDeterminism :: Core.State Identity -> Input Identity -> Core.State Identity -> Input Identity -> Bool
leakageDeterminism = P.leakageDeterminism circuits

stateless :: (a -> b) -> () -> a -> ((), b)
stateless f _ x = ((), f x)

obs :: () -> Output Identity -> ((), Maybe Address)
obs = stateless obs'

obs' :: Output Identity -> Maybe Address
obs' o_sim = do
  mem <- getFirst $ outMem o_sim
  guard $ memIsInstr mem
  pure $ memAddress mem

leak :: Leak.State -> Input Identity -> (Leak.State, Leak.Out)
leak = Leak.circuit

sim :: Sim.State -> Leak.Out -> (Sim.State, Maybe Address)
sim = Sim.circuit

circuit ::
  (Leak.State, Sim.State) ->
  Input Identity ->
  ((Leak.State, Sim.State), Maybe Address)
circuit (ts, ss) input = ((ts', ss'), addr)
  where
    (ts', o_leak) = leak ts input
    (ss', addr) = sim ss o_leak

proj :: Core.State Identity -> (Leak.State, Sim.State)
proj s = (ts, ss)
  where
    ts =
      Leak.State
        { Leak.stateFePc = Core.stateFePc s,
          Leak.stateDePc = Core.stateDePc s,
          Leak.stateExPc = Core.stateExPc s,
          Leak.stateExInstr = Core.stateExInstr s,
          Leak.stateMemInstr = Core.stateMemInstr s,
          Leak.stateMemRes = runIdentity $ Core.stateMemRes s,
          Leak.stateWbInstr = Core.stateWbInstr s,
          Leak.stateWbRes = runIdentity $ Core.stateWbRes s,
          Leak.stateStallFetch = toStallFetch $ Core.stateCtrl s,
          Leak.stateStallDecode = toStallDecode $ Core.stateCtrl s,
          Leak.stateHalt = Core.stateHalt s /= Core.Running,
          Leak.stateMeRegFwd = fmap (fmap runIdentity) $ Core.ctrlMeRegFwd $ Core.stateCtrl s,
          Leak.stateWbRegFwd = fmap (fmap runIdentity) $ Core.ctrlWbRegFwd $ Core.stateCtrl s,
          Leak.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s,
          Leak.stateFirstCycle = Core.ctrlFirstCycle $ Core.stateCtrl s
        }
    ss =
      Sim.State
        { Sim.stateFePc = Core.stateFePc s,
          Sim.stateDePc = Core.stateDePc s,
          Sim.stateExPc = Core.stateExPc s,
          Sim.stateExInstr = toLeakInstr $ Core.stateExInstr s,
          Sim.stateMemInstr = killJump $ toLeakInstr $ Core.stateMemInstr s,
          Sim.stateWbInstr = killJump $ toLeakInstr $ Core.stateWbInstr s,
          Sim.stateHalt = Core.stateHalt s /= Core.Running,
          Sim.stateStallFetch = toStallFetch $ Core.stateCtrl s,
          Sim.stateStallDecode = toStallDecode $ Core.stateCtrl s,
          Sim.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s,
          Sim.stateFirstCycle = Core.ctrlFirstCycle $ Core.stateCtrl s
        }

    killJump :: Leak.Instr -> Leak.Instr
    killJump (Leak.Instr (Leak.Jump {}) _) = Leak.nop
    killJump i = i

    toLeakInstr :: Instruction -> Leak.Instr
    toLeakInstr instr =
      Leak.Instr
        (Leak.mkInstr instr)
        (Leak.mkDeps instr)

    toStallFetch :: Core.Control Identity -> Bool
    toStallFetch ctrl =
      Core.ctrlDecodeLoad ctrl
        || Core.ctrlMemOutputActive ctrl
        || isJust (Core.ctrlExBranch ctrl)

    toStallDecode :: Core.Control Identity -> Bool
    toStallDecode ctrl =
      Core.ctrlFirstCycle ctrl
        || isJust (Core.ctrlExBranch ctrl)
        || Core.ctrlMemInputActive ctrl

simulator ::
  forall m.
  ( MonadState ((Core.State Identity, Output Identity), Simulate.Mem MEM_SIZE_BYTES) m
  ) =>
  CircuitSim m (Input Identity) (Leak.State, Sim.State) (Maybe Address, Maybe Address)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = (Leak.init, Sim.init),
      circuitStep = step,
      circuitNext = next
    }
  where
    step ::
      Input Identity ->
      (Leak.State, Sim.State) ->
      m ((Leak.State, Sim.State), (Maybe Address, Maybe Address))
    step i s = do
      ((s_sim, _), mem) <- get
      let (res_sim@(_, o_sim), mem') = runState (circuitStep Simulate.simulator i s_sim) mem
      put (res_sim, mem')
      let (s', o) = circuit s i
      pure (s', (o, obs' o_sim))

    next :: (Maybe Address, Maybe Address) -> m (Maybe (Input Identity))
    next (_o, _addr_sim) = do
      ((_, o_sim), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_sim) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  ( CircuitSim
      (State ((Core.State Identity, Output Identity), Simulate.Mem MEM_SIZE_BYTES))
      (Input Identity)
      (Leak.State, Sim.State)
      (Maybe Address, Maybe Address) ->
    State ((Core.State Identity, Output Identity), Simulate.Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f prog = evalState (f Leak.PC.PC.simulator) s
  where
    s = ((Core.init, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  Vec PROG_SIZE Word ->
  [((Leak.State, Sim.State), (Maybe Address, Maybe Address), Maybe (Input Identity))]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'
