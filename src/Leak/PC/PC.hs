{-# LANGUAGE UndecidableInstances #-}

module Leak.PC.PC
  ( circuit,
    simulator,
    runSimulator,
    watchSim,
    pcsEqual,
    proj,
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
import Data.Set (Set)
import qualified Data.Set as S
import qualified ISA
import Instruction (Instruction)
import qualified Instruction
import qualified Leak.PC.Sim as Sim
import qualified Leak.PC.Time as Time
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
  ( MonadState ((Core.State, Output), Simulate.Mem MEM_SIZE_BYTES) m
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
      (State ((Core.State, Output), Simulate.Mem MEM_SIZE_BYTES))
      Input
      (Time.State, Sim.State)
      (Maybe Address, Maybe Address) ->
    State ((Core.State, Output), Simulate.Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f prog = evalState (f simulator) s
  where
    s = ((Core.init, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  Vec PROG_SIZE Word ->
  [((Time.State, Sim.State), (Maybe Address, Maybe Address), Maybe Input)]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'

proj :: Core.State -> (Time.State, Sim.State)
proj s = (ts, ss)
  where
    ts =
      Time.State
        { Time.stateFePc = Core.stateFePc s,
          Time.stateDePc = Core.stateDePc s,
          Time.stateExPc = Core.stateExPc s,
          Time.stateExInstr = toISAFunc $ Core.stateExInstr s,
          Time.stateMemInstr = toISADone (Core.stateMemInstr s) Time.Mem,
          Time.stateWbInstr = toISADone (Core.stateWbInstr s) Time.Wb,
          Time.stateStall = toStallStages $ Core.stateCtrl s,
          Time.stateHalt = Core.stateHalt s,
          Time.stateMeRegFwd = Core.ctrlMeRegFwd $ Core.stateCtrl s,
          Time.stateWbRegFwd = Core.ctrlWbRegFwd $ Core.stateCtrl s,
          Time.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s
        }
    ss =
      Sim.State
        { Sim.stateFePc = Core.stateFePc s,
          Sim.stateDePc = Core.stateDePc s,
          Sim.stateExPc = Core.stateExPc s,
          Sim.stateExInstr = toTimeInstr $ Core.stateExInstr s,
          Sim.stateMemInstr = toTimeInstr $ Core.stateMemInstr s,
          Sim.stateWbInstr = toTimeInstr $ Core.stateWbInstr s,
          Sim.stateHalt = Core.stateHalt s,
          Sim.stateStall = toStallStages $ Core.stateCtrl s,
          Sim.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s
        }

    toTimeInstr :: Instruction -> Time.Instr
    toTimeInstr inst = Time.Instr (Time.mkInstr $ toISAFunc inst) (ISA.depSet $ toISAFunc inst)

    toISAFunc :: Instruction -> ISA.Instr ISA.Func
    toISAFunc i =
      ISA.interp $
        Input
          { inputIsInstr = True,
            inputMem = Instruction.encode i,
            inputRs1 = 0,
            inputRs2 = 0
          }

    toISADone :: Instruction -> Time.Stage -> ISA.Instr ISA.Done
    toISADone i stage =
      case li of
        ISA.Reg rd _ -> ISA.Reg rd $ ISA.Done res
        ISA.Load size rd _ -> ISA.Load size rd $ ISA.Done $ bitCoerce res
        ISA.Jump rd _ _ -> ISA.Jump rd (ISA.Done $ bitCoerce res) dontCare
        ISA.Store size _ r2 -> ISA.Store size (ISA.Done $ bitCoerce res) r2
        ISA.Branch _ _ -> ISA.Branch (ISA.Done branched) dontCare
        ISA.Nop -> ISA.Nop
        ISA.Break -> ISA.Break
      where
        li = toISAFunc i
        res
          | stage == Time.Mem = Core.stateMemRes s
          | otherwise = Core.stateWbRes s
        branched = (stage == Time.Mem) && Core.stateMemBranch s
        dontCare = ISA.Done 0

    toStallStages :: Core.Control -> Set Time.Stage
    toStallStages ctrl =
      S.fromList [stage | (stage, conds) <- stallConditions, any ($ ctrl) conds]
      where
        stallConditions =
          [ ( Time.Fe,
              [ Core.ctrlDecodeLoad,
                Core.ctrlMemOutputActive
              ]
            ),
            ( Time.De,
              [ Core.ctrlFirstCycle,
                isJust . Core.ctrlExBranch,
                Core.ctrlMemInputActive,
                Core.ctrlMemBranch
              ]
            )
          ]
