{-# LANGUAGE UndecidableInstances #-}
-- touch to force rebuild

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
    -- comment them out to disable Pantomime checks for faster compilation
    -- theory,
    -- circuits,
    -- tickStateCorrespondence,
    -- projectionCoherence,
  )
where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Core (Input (..), MemAccess (..), Output (..), initInput)
import qualified Core
import Data.Bifunctor (second)
import Data.Functor.Identity
import Data.Monoid
import qualified Instruction as Instr
import qualified Leak.PC.Leak as Leak
import qualified Leak.PC.Sim as Sim
import qualified Simulate
import Memory.Types
import Memory.Vec ()
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

stateless :: (a -> b) -> () -> a -> ((), b)
stateless f _ x = ((), f x)

implementation :: Core.State Identity -> Input Identity -> (Core.State Identity, Output Identity)
implementation = Core.circuit

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
sim ss l_out =
  let (ss', addr) = Sim.circuit ss l_out
   in (ss', join (getFirst addr))

circuit ::
  (Leak.State, Sim.State) ->
  Input Identity ->
  ((Leak.State, Sim.State), Maybe Address)
circuit (ts, ss) i = ((ts', ss'), addr)
  where
    (ts', o_leak) = leak ts i
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
          Leak.stateMemRes = unAccess $ Core.stateMemRes s,
          Leak.stateMemVal = unAccess $ Core.stateMemVal s,
          Leak.stateWbInstr = Core.stateWbInstr s,
          Leak.stateWbRes = unAccess $ Core.stateWbRes s,
          Leak.stateRegFile = Core.stateRegFile s,
          Leak.stateMeMemInstr = Core.ctrlMeMemInstr $ Core.stateCtrl s,
          Leak.stateHalt = Core.stateHalt s,
          Leak.stateMeRegFwd = fmap (second unAccess) $ Core.ctrlMeRegFwd $ Core.stateCtrl s,
          Leak.stateWbRegFwd = fmap (second unAccess) $ Core.ctrlWbRegFwd $ Core.stateCtrl s,
          Leak.stateJumpAddr = Core.ctrlExAddress $ Core.stateCtrl s,
          Leak.stateDeLoadHazard = Core.ctrlDeLoadHazard $ Core.stateCtrl s,
          Leak.stateDeCall = Core.ctrlDeCall $ Core.stateCtrl s,
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
          Sim.stateMeMemInstr = Core.ctrlMeMemInstr $ Core.stateCtrl s,
          Sim.stateJumpAddr = Core.ctrlExAddress $ Core.stateCtrl s,
          Sim.stateDeLoadHazard = Core.ctrlDeLoadHazard $ Core.stateCtrl s,
          Sim.stateDeCall = Core.ctrlDeCall $ Core.stateCtrl s,
          Sim.stateFirstCycle = Core.ctrlFirstCycle $ Core.stateCtrl s
        }

    killJump :: Leak.Instr -> Leak.Instr
    killJump (Leak.Instr Leak.Jump _) = Leak.nop
    killJump i = i

    toLeakInstr :: Instr.Instruction -> Leak.Instr
    toLeakInstr instr =
      Leak.Instr
        (Leak.mkInstr instr)
        (Leak.mkDeps instr)

simulator ::
  forall m.
  (MonadState ((Core.State Identity, Output Identity), Simulate.Mem MEM_SIZE_BYTES) m) =>
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
    step i (ts, ss) = do
      ((s_core_old, _), mem) <- get
      let (s_core', o_core) = implementation s_core_old i

      -- Update memory manually (register file is now in s_core')
      let mem' = case getFirst (outMem o_core) of
            Just (MemAccess _ addr size (Just val)) ->
              mem {Simulate.memRAM = write size addr (runIdentity val) (Simulate.memRAM mem)}
            _ -> mem

      put ((s_core', o_core), mem')

      let ((ts', ss'), addr) = circuit (ts, ss) i
      pure ((ts', ss'), (obs' o_core, addr))

    next :: (Maybe Address, Maybe Address) -> m (Maybe (Input Identity))
    next (_o, _addr_sim) = do
      ((_, o_core), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_core) mem
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
    s = ((Core.init, mempty), Simulate.Mem (mkRAM' prog))
    mkRAM' p = Memory.Types.mkRAM @PROG_SIZE @RAM_SIZE_BYTES p

watchSim ::
  Vec PROG_SIZE Word ->
  [((Leak.State, Sim.State), (Maybe Address, Maybe Address), Maybe (Input Identity))]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'
