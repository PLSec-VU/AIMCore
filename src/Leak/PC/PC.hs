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
  )
where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Core (Input (..), MemAccess (..), Output (..), initInput)
import qualified Core
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

-- Uncomment this to check the leakage.
-- import UC (Spec (..))
--
-- {-# ANN
--   implementation
--   Spec
--     { observation' = 'obs,
--       leakage' = 'leak,
--       simulator' = 'sim,
--       projection' = 'proj
--     }
--   #-}
implementation :: (Access f) => Core.State f -> Input f -> (Core.State f, Output f)
implementation = Core.circuit

stateless :: (a -> b) -> () -> a -> ((), b)
stateless f _ x = ((), f x)

obs :: () -> Output f -> ((), Maybe Address)
obs = stateless obs'

obs' :: Output f -> Maybe Address
obs' o_sim = do
  mem <- getFirst $ outMem o_sim
  guard $ memIsInstr mem
  pure $ memAddress mem

leak :: (Access f) => Leak.State f -> Input f -> (Leak.State f, Leak.Out)
leak = Leak.circuit

sim :: Sim.State -> Leak.Out -> (Sim.State, Maybe Address)
sim = Sim.circuit

circuit ::
  (Access f) =>
  (Leak.State f, Sim.State) ->
  Input f ->
  ((Leak.State f, Sim.State), Maybe Address)
circuit (ts, ss) input = ((ts', ss'), addr)
  where
    (ts', o_leak) = leak ts input
    (ss', addr) = sim ss o_leak

proj :: (Core.State f, ()) -> (Leak.State f, Sim.State)
proj (s, _) = (ts, ss)
  where
    ts =
      Leak.State
        { Leak.stateFePc = Core.stateFePc s,
          Leak.stateDePc = Core.stateDePc s,
          Leak.stateExPc = Core.stateExPc s,
          Leak.stateExInstr = Core.stateExInstr s,
          Leak.stateMemInstr = Core.stateMemInstr s,
          Leak.stateMemRes = Core.stateMemRes s,
          Leak.stateWbInstr = Core.stateWbInstr s,
          Leak.stateWbRes = Core.stateWbRes s,
          Leak.stateStallFetch = toStallFetch $ Core.stateCtrl s,
          Leak.stateStallDecode = toStallDecode $ Core.stateCtrl s,
          Leak.stateHalt = Core.stateHalt s,
          Leak.stateMeRegFwd = Core.ctrlMeRegFwd $ Core.stateCtrl s,
          Leak.stateWbRegFwd = Core.ctrlWbRegFwd $ Core.stateCtrl s,
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
          Sim.stateHalt = Core.stateHalt s,
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

    toStallFetch :: Core.Control f -> Bool
    toStallFetch ctrl =
      Core.ctrlDecodeLoad ctrl
        || Core.ctrlMemOutputActive ctrl
        || isJust (Core.ctrlExBranch ctrl)

    toStallDecode :: Core.Control f -> Bool
    toStallDecode ctrl =
      Core.ctrlFirstCycle ctrl
        || isJust (Core.ctrlExBranch ctrl)
        || Core.ctrlMemInputActive ctrl

simulator ::
  forall m f.
  ( Access f,
    MonadState ((Core.State f, Output f), Simulate.Mem f MEM_SIZE_BYTES) m
  ) =>
  CircuitSim m (Input f) (Leak.State f, Sim.State) (Maybe Address, Maybe Address)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = (Leak.init, Sim.init),
      circuitStep = step,
      circuitNext = next
    }
  where
    step ::
      Input f ->
      (Leak.State f, Sim.State) ->
      m ((Leak.State f, Sim.State), (Maybe Address, Maybe Address))
    step i s = do
      ((s_sim, _), mem) <- get
      let (res_sim@(_, o_sim), mem') = runState (circuitStep Simulate.simulator i s_sim) mem
      put (res_sim, mem')
      let (s', o) = circuit s i
      pure (s', (o, obs' o_sim))

    next :: (Maybe Address, Maybe Address) -> m (Maybe (Input f))
    next (_o, _addr_sim) = do
      ((_, o_sim), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_sim) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  (Access f) =>
  ( CircuitSim
      (State ((Core.State f, Output f), Simulate.Mem f MEM_SIZE_BYTES))
      (Input f)
      (Leak.State f, Sim.State)
      (Maybe Address, Maybe Address) ->
    State ((Core.State f, Output f), Simulate.Mem f MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE (f Word) ->
  a
runSimulator f prog = evalState (f Leak.PC.PC.simulator) s
  where
    s = ((Core.init, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  (Access f) =>
  Vec PROG_SIZE (f Word) ->
  [((Leak.State f, Sim.State), (Maybe Address, Maybe Address), Maybe (Input f))]
watchSim = runSimulator watch

pcsEqual :: (Access f) => Vec PROG_SIZE (f Word) -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'
