module Leak.MonitorPC.Sim where

import Types
import Leak.MonitorPC.MonitorLeak as Leak

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Leak.Instr,
    stateMemInstr :: Leak.Instr,
    stateWbInstr :: Leak.Instr,
    stateJumpAddr :: Maybe Address,
    stateStallFetch :: Bool,
    stateStallDecode :: Bool,
    stateHalt :: Bool,
    stateFirstCycle :: Bool
  }
  deriving (Show, Eq)
