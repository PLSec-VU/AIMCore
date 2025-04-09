{-# LANGUAGE UndecidableInstances #-}

module Leak.PC.PC
  ( obs,
    leak,
    sim,
    circuit,
    proj,
    simulator,
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
import Data.Set (Set)
import qualified Data.Set as S
import qualified ISA
import Instruction (Instruction)
import qualified Instruction
import qualified Leak.PC.Leak as Leak
import qualified Leak.PC.Sim as Sim
import Regfile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

-- Uncomment this to check the leakage.
-- import UC (Spec (..))
--
-- {-# ANN implementation Spec
--   { observation' = 'obs
--   , leakage' = 'leak
--   , simulator' = 'sim
--   , projection' = 'proj
--   } #-}
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
    (ts', leakage) = leak ts input
    (ss', addr) = sim ss leakage

proj :: (Core.State, ()) -> (Leak.State, Sim.State)
proj (s, _) = (ts, ss)
  where
    ts =
      Leak.State
        { Leak.stateFePc = Core.stateFePc s,
          Leak.stateDePc = Core.stateDePc s,
          Leak.stateExPc = Core.stateExPc s,
          Leak.stateExInstr = Core.stateExInstr s,
          Leak.stateMemInstr = toISADone (Core.stateMemInstr s) Leak.Mem,
          Leak.stateWbInstr = killBranch $ toISADone (Core.stateWbInstr s) Leak.Wb,
          Leak.stateStall = toStallStages $ Core.stateCtrl s,
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
          Sim.stateExInstr = toLeakInstrFunc $ Core.stateExInstr s,
          Sim.stateMemInstr = toLeakInstrDone (Core.stateMemInstr s) Leak.Mem,
          Sim.stateWbInstr = killJump $ toLeakInstrDone (Core.stateWbInstr s) Leak.Wb,
          Sim.stateHalt = Core.stateHalt s,
          Sim.stateStall = toStallStages $ Core.stateCtrl s,
          Sim.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s
        }

    killJump :: Leak.Instr -> Leak.Instr
    killJump (Leak.Instr (Leak.Jump {}) _) = Leak.nop
    killJump i = i

    killBranch :: ISA.Instr ISA.Done -> ISA.Instr ISA.Done
    killBranch (ISA.Branch {}) = ISA.Nop
    killBranch i = i

    toLeakInstrFunc :: Instruction -> Leak.Instr
    toLeakInstrFunc inst =
      Leak.Instr
        (Leak.mkInstr $ toISAFunc inst)
        (ISA.depSet $ toISAFunc inst)

    toLeakInstrDone :: Instruction -> Leak.Stage -> Leak.Instr
    toLeakInstrDone inst stage = leak_inst
      where
        leak_inst =
          case isa_inst of
            ISA.Branch (ISA.Done branched) _
              | not branched -> Leak.nop
            _ -> Leak.Instr (Leak.mkInstr isa_inst) (ISA.depSet $ toISAFunc inst)
        isa_inst = toISADone inst stage

    toISAFunc :: Instruction -> ISA.Instr ISA.Func
    toISAFunc i =
      ISA.interp $
        Input
          { inputIsInstr = True,
            inputMem = Instruction.encode i,
            inputRs1 = 0,
            inputRs2 = 0
          }
    toISADone :: Instruction -> Leak.Stage -> ISA.Instr ISA.Done
    toISADone i stage =
      case li of
        ISA.Reg rd _ -> ISA.Reg rd $ ISA.Done res
        ISA.Load size sign rd _ -> ISA.Load size sign rd $ ISA.Done $ bitCoerce res
        ISA.Jump rd _ _ -> ISA.Jump rd (ISA.Done $ bitCoerce res) dontCare
        ISA.Store size _ r2 -> ISA.Store size (ISA.Done $ bitCoerce res) r2
        ISA.Branch _ _ -> ISA.Branch (ISA.Done branched) dontCare
        ISA.Nop -> ISA.Nop
        ISA.Break -> ISA.Break
      where
        li = toISAFunc i
        res
          | stage == Leak.Mem = Core.stateMemRes s
          | otherwise = Core.stateWbRes s
        branched = (stage == Leak.Mem) && Core.stateMemBranch s
        dontCare = ISA.Done 0

    toStallStages :: Core.Control -> Set Leak.Stage
    toStallStages ctrl =
      S.fromList [stage | (stage, conds) <- stallConditions, any ($ ctrl) conds]
      where
        stallConditions =
          [ ( Leak.Fe,
              [ Core.ctrlDecodeLoad,
                Core.ctrlMemOutputActive
              ]
            ),
            ( Leak.De,
              [ Core.ctrlFirstCycle,
                isJust . Core.ctrlExBranch,
                Core.ctrlMemInputActive,
                Core.ctrlMemBranch
              ]
            )
          ]

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
runSimulator f prog = evalState (f simulator) s
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
