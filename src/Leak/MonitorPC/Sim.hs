module Leak.MonitorPC.Sim where

import Leak.MonitorPC.MonitorLeak as Leak
import Types

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Leak.Instr,
    stateMemInstr :: Leak.Instr,
    stateMemRes :: Types.Word,
    stateWbInstr :: Leak.Instr,
    stateWbRes :: Types.Word,
    stateJumpAddr :: Maybe Address,
    stateStallFetch :: Bool,
    stateStallDecode :: Bool,
    stateHalt :: Bool,
    stateFirstCycle :: Bool
  }
  deriving (Show, Eq)
