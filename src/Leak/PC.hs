{-# LANGUAGE UndecidableInstances #-}

module Leak.PC where

-- ( leakTimeSimRun,
--   simulator,
--   runSimulator,
--   watchSim,
--   pcsEqual,
--   proj,
-- )

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import qualified ISA
import Instruction (Instruction)
import qualified Instruction
import qualified Leak.PC.Sim as Sim
import qualified Leak.PC.Time as Time
import Pipe
import Regfile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

circuit ::
  Input ->
  (Time.State, Sim.State) ->
  ((Time.State, Sim.State), Maybe Address)
circuit input (ts, ss) = ((ts', ss'), addr)
  where
    isa_instr = ISA.interp input
    (ts', leak) = Time.circuit ts (isa_instr, input)
    (ss', addr) = Sim.circuit ss leak

simulator ::
  forall m.
  ( MonadState ((Pipe, Output), Simulate.Mem MEM_SIZE_BYTES) m
  ) =>
  CircuitSim m Input (Time.State, Sim.State) (Maybe Address, Maybe Address)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = (Time.init, Sim.init),
      circuitStep = step,
      circuitNext = next
    }
  where
    obs :: Output -> Maybe Address
    obs o_sim = do
      mem <- getFirst $ outMem o_sim
      guard $ memIsInstr mem
      pure $ memAddress mem

    step ::
      Input ->
      (Time.State, Sim.State) ->
      m ((Time.State, Sim.State), (Maybe Address, Maybe Address))
    step i s = do
      ((s_sim, _), mem) <- get
      let (res_sim@(_, o_sim), mem') = runState (circuitStep Simulate.simulator i s_sim) mem
      put (res_sim, mem')
      let (s', o) = circuit i s
      pure (s', (o, obs o_sim))

    next :: (Maybe Address, Maybe Address) -> m (Maybe Input)
    next (_o, _addr_sim) = do
      ((_, o_sim), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_sim) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  ( CircuitSim
      (State ((Pipe, Output), Simulate.Mem MEM_SIZE_BYTES))
      Input
      (Time.State, Sim.State)
      (Maybe Address, Maybe Address) ->
    State ((Pipe, Output), Simulate.Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f prog = evalState (f simulator) s
  where
    s = ((initPipe, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  Vec PROG_SIZE Word ->
  [((Time.State, Sim.State), (Maybe Address, Maybe Address), Maybe Input)]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'

proj :: Pipe -> (Time.State, Sim.State)
proj s = (ts, ss)
  where
    ts =
      Time.State
        { Time.stateFePc = fePc s,
          Time.stateDePc = dePc s,
          Time.stateExPc = exPc s,
          Time.stateExInstr = convertInst $ exIr s,
          Time.stateMemInstr = convertInstDone (meIr s) Time.Mem,
          Time.stateWbInstr = convertInstDone (wbIr s) Time.Wb,
          Time.stateStall = convertStall $ pipeCtrl s,
          Time.stateHalt = pipeHalt s,
          Time.stateMeRegFwd = ctrlMeRegFwd $ pipeCtrl s,
          Time.stateWbRegFwd = ctrlWbRegFwd $ pipeCtrl s,
          Time.stateJumpAddr = ctrlExBranch $ pipeCtrl s
        }
    ss =
      Sim.State
        { Sim.stateFePc = fePc s,
          Sim.stateDePc = dePc s,
          Sim.stateExPc = exPc s,
          Sim.stateExInstr = convertTime $ exIr s,
          Sim.stateMemInstr = convertTime $ meIr s,
          Sim.stateWbInstr = convertTime $ wbIr s,
          Sim.stateHalt = pipeHalt s,
          Sim.stateStall = convertStall $ pipeCtrl s,
          Sim.stateJumpAddr = ctrlExBranch $ pipeCtrl s
        }

    convertTime :: Instruction -> Time.Instr
    convertTime inst = Time.Instr (Time.mkInstr $ convertInst inst) (ISA.depSet $ convertInst inst)

    convertInst :: Instruction -> ISA.Instr ISA.Func -- fix
    convertInst i =
      ISA.interp $
        Input
          { inputIsInstr = True,
            inputMem = Instruction.encode i,
            inputRs1 = 0,
            inputRs2 = 0
          }
    convertInstDone :: Instruction -> Time.Stage -> ISA.Instr ISA.Done -- fix
    convertInstDone i stage =
      case li of
        ISA.Reg rd _ -> ISA.Reg rd $ ISA.Done res
        ISA.Load size rd _ -> ISA.Load size rd $ ISA.Done $ bitCoerce res
        ISA.Jump rd _ _ -> ISA.Jump rd (ISA.Done $ bitCoerce res) lol
        ISA.JumpReg rd _ _ -> ISA.JumpReg rd (ISA.Done $ bitCoerce res) lol
        ISA.Store size _ r2 -> ISA.Store size (ISA.Done $ bitCoerce res) r2
        ISA.Branch _ _ -> ISA.Branch (ISA.Done branched) lol
        ISA.Nop -> ISA.Nop
        ISA.Break -> ISA.Break
      where
        li = convertInst i
        res
          | stage == Time.Mem = meRe s
          | otherwise = wbRe s
        branched = (stage == Time.Mem) && meBranch s
        lol = ISA.Done 0

    convertStall :: Control -> Set Time.Stage
    convertStall ctrl =
      S.fromList [stage | (stage, conds) <- stallConditions, any ($ ctrl) conds]
      where
        stallConditions =
          [ (Time.Fe, [ctrlDecodeLoad, ctrlMemOutputActive]),
            (Time.De, [ctrlFirstCycle, isJust . ctrlExBranch, ctrlMemInputActive, ctrlMemBranch])
          ]
