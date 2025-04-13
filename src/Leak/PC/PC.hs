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

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Core (Input (..), MemAccess (..), Output (..), initInput)
import qualified Core
import Data.Maybe (isJust)
import Data.Monoid
import Instruction (Instruction)
import qualified Instruction
import qualified Leak.PC.Leak as Leak
import qualified Leak.PC.Sim as Sim
import Regfile
import qualified Simulate
import Types
import UC
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

-- Uncomment this to check the leakage.
-- import UC (Spec (..))
--
{-# ANN
  implementation
  Spec
    { observation' = 'obs,
      leakage' = 'leak,
      simulator' = 'sim,
      projection' = 'proj
    }
  #-}
implementation :: Core.State -> Input -> (Core.State, Output)
implementation = Core.circuit

stateless :: (a -> b) -> () -> a -> ((), b)
stateless f _ x = ((), f x)

obs :: () -> Output -> ((), Maybe Address)
obs = stateless obs'

obs' :: Output -> Maybe Address
obs' o_sim = do
  mem <- getFirst $ outMem o_sim
  guard $ memIsInstr mem
  pure $ memAddress mem

leak :: Leak.State -> Input -> (Leak.State, Leak.Out)
leak = Leak.circuit

sim :: Sim.State -> Leak.Out -> (Sim.State, Maybe Address)
sim = Sim.circuit

circuit ::
  (Leak.State, Sim.State) ->
  Input ->
  ((Leak.State, Sim.State), Maybe Address)
circuit (ts, ss) input = ((ts', ss'), addr)
  where
    (ts', o_leak) = leak ts input
    (ss', addr) = sim ss o_leak

proj :: (Core.State, ()) -> (Leak.State, Sim.State)
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
          Leak.stateMemBranch = Core.stateMemBranch s,
          Leak.stateWbInstr = Core.stateWbInstr s,
          Leak.stateWbRes = Core.stateWbRes s,
          Leak.stateStallFetch = toStallFetch $ Core.stateCtrl s,
          Leak.stateStallDecode = toStallDecode $ Core.stateCtrl s,
          Leak.stateHalt = Core.stateHalt s,
          Leak.stateMeRegFwd = Core.ctrlMeRegFwd $ Core.stateCtrl s,
          Leak.stateWbRegFwd = Core.ctrlWbRegFwd $ Core.stateCtrl s,
          Leak.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s
        }
    ss =
      Sim.State
        { Sim.stateFePc = Core.stateFePc s,
          Sim.stateDePc = Core.stateDePc s,
          Sim.stateExPc = Core.stateExPc s,
          Sim.stateExInstr = toLeakInstr $ Core.stateExInstr s,
          Sim.stateMemInstr = toLeakInstrDone (Core.stateMemInstr s) (Core.stateMemBranch s),
          Sim.stateWbInstr = killJump $ toLeakInstrDone (Core.stateWbInstr s) False,
          Sim.stateHalt = Core.stateHalt s,
          Sim.stateStallFetch = toStallFetch $ Core.stateCtrl s,
          Sim.stateStallDecode = toStallDecode $ Core.stateCtrl s,
          Sim.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s
        }

    killJump :: Leak.Instr -> Leak.Instr
    killJump (Leak.Instr (Leak.Jump {}) _) = Leak.nop
    killJump i = i

    toLeakInstr :: Instruction -> Leak.Instr
    toLeakInstr instr =
      Leak.Instr
        (Leak.mkInstr instr)
        (Leak.mkDeps instr)

    toLeakInstrDone :: Instruction -> Bool -> Leak.Instr
    toLeakInstrDone inst branched = leak_inst
      where
        leak_inst =
          case inst of
            Instruction.BType {}
              | not branched -> Leak.nop
            _ -> Leak.Instr (Leak.mkInstr inst) (Leak.mkDeps inst)

    toStallFetch :: Core.Control -> Bool
    toStallFetch ctrl =
      Core.ctrlDecodeLoad ctrl
        || Core.ctrlMemOutputActive ctrl

    toStallDecode :: Core.Control -> Bool
    toStallDecode ctrl =
      Core.ctrlFirstCycle ctrl
        || isJust (Core.ctrlExBranch ctrl)
        || Core.ctrlMemInputActive ctrl
        || Core.ctrlMemBranch ctrl

simulator ::
  forall m.
  ( MonadState ((Core.State, Output), Simulate.Mem MEM_SIZE_BYTES) m
  ) =>
  CircuitSim m Input (Leak.State, Sim.State) (Maybe Address, Maybe Address)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = (Leak.init, Sim.init),
      circuitStep = step,
      circuitNext = next
    }
  where
    step ::
      Input ->
      (Leak.State, Sim.State) ->
      m ((Leak.State, Sim.State), (Maybe Address, Maybe Address))
    step i s = do
      ((s_sim, _), mem) <- get
      let (res_sim@(_, o_sim), mem') = runState (circuitStep Simulate.simulator i s_sim) mem
      put (res_sim, mem')
      let (s', o) = circuit s i
      pure (s', (o, obs' o_sim))

    next :: (Maybe Address, Maybe Address) -> m (Maybe Input)
    next (_o, _addr_sim) = do
      ((_, o_sim), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_sim) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  ( CircuitSim
      (State ((Core.State, Output), Simulate.Mem MEM_SIZE_BYTES))
      Input
      (Leak.State, Sim.State)
      (Maybe Address, Maybe Address) ->
    State ((Core.State, Output), Simulate.Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f prog = evalState (f Leak.PC.PC.simulator) s
  where
    s = ((Core.init, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  Vec PROG_SIZE Word ->
  [((Leak.State, Sim.State), (Maybe Address, Maybe Address), Maybe Input)]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'
