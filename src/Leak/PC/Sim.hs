module Leak.PC.Sim
  ( pipe,
    circuit,
    init,
    State (..),
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import qualified Core as AimCore
import qualified Instruction as Core
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import qualified Leak.PC.Leak as Leak
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Leak.Instr,
    stateMemInstr :: Leak.Instr,
    stateWbInstr :: Leak.Instr,
    stateJumpAddr :: Maybe Address,
    stateDecodeLoad :: Bool,
    stateMemOutputActive :: Bool,
    stateMeMemInstr :: Bool,
    stateStallFetch :: Bool,
    stateStallDecode :: Bool,
    stateHalt :: AimCore.HaltState,
    stateDeLoadHazard :: Maybe Address,
    stateDeCall :: Bool,
    stateFirstCycle :: Bool
  }
  deriving (Show, Eq)

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = Leak.nop,
      stateMemInstr = Leak.nop,
      stateWbInstr = Leak.nop,
      stateHalt = AimCore.Running,
      stateDecodeLoad = False,
      stateMemOutputActive = False,
      stateMeMemInstr = False,
      stateStallFetch = False,
      stateStallDecode = False,
      stateJumpAddr = Nothing,
      stateDeLoadHazard = Nothing,
      stateDeCall = False,
      stateFirstCycle = True
    }

type SimM = RWS Leak.Out (First (Maybe Address)) State

setDecodeLoad :: SimM ()
setDecodeLoad = modify $ \s -> s {stateDecodeLoad = True}

setMemOutputActive :: SimM ()
setMemOutputActive = modify $ \s -> s {stateMemOutputActive = True}

setMeMemInstr :: SimM ()
setMeMemInstr = modify $ \s -> s {stateMeMemInstr = True}

stallFetch :: SimM ()
stallFetch = modify $ \s -> s {stateStallFetch = True}

stallDecode :: SimM ()
stallDecode = modify $ \s -> s {stateStallDecode = True}

outputPc :: Address -> SimM ()
outputPc addr =
  tell $ pure $ pure addr

outputNothing :: SimM ()
outputNothing = tell $ pure Nothing

fetch :: SimM ()
fetch = do
  pc <- gets stateFePc
  mJumpAddr <- gets stateJumpAddr
  deLoadHazard <- gets stateDeLoadHazard
  deCall <- gets stateDeCall
  meMemInstr <- gets stateMeMemInstr

  unless meMemInstr $
    outputPc pc

  let stall = deCall || meMemInstr

  let next_pc =
        fromMaybe
          (fromMaybe
             (if stall then pc else pc + 4)
             deLoadHazard)
          mJumpAddr

  modify $ \s ->
    s
      { stateFePc = next_pc,
        stateDePc = pc
      }

decode :: SimM ()
decode = do
  instr <- fromMaybe (Leak.Instr (Leak.Nop Core.MemoryBusBusy) (Nothing, Nothing)) . getFirst <$> asks Leak.outInstr
  when (isLoad instr || isCall instr) $ do
    setDecodeLoad

  exInstr <- gets stateExInstr
  mJumpAddr <- gets stateJumpAddr
  firstCycle <- gets stateFirstCycle

  let branch_first_cycle = isNopBranchFirstCycle exInstr
  let load_hazard_current_cycle = Leak.loadHazard instr exInstr
  let load_hazard_first_cycle = isNopLoadHazardFirstCycle exInstr
  let call_current_cycle = isCall exInstr

  let ir' =
        -- If a branch was taken in this cycle, we stall.
        if isJust mJumpAddr then Leak.Instr (Leak.Nop Core.BranchFirstCycle) (Nothing, Nothing)
        -- If a branch was taken in the previous cycle, we stall.
        else if branch_first_cycle then Leak.Instr (Leak.Nop Core.BranchSecondCycle) (Nothing, Nothing)
        -- If there is a load hazard with the instruction executed in this cycle, we stall.
        else if load_hazard_current_cycle then Leak.Instr (Leak.Nop Core.LoadHazardFirstCycle) (Nothing, Nothing)
        -- If there was a load hazard in the previous cycle, we stall.
        else if load_hazard_first_cycle then Leak.Instr (Leak.Nop Core.LoadHazardSecondCycle) (Nothing, Nothing)
        -- If a syscall is executed in this cycle, we stall.
        else if call_current_cycle then Leak.Instr (Leak.Nop Core.SyscallFirstCycle) (Nothing, Nothing)
        -- If this is the first cycle, the instruction to decode is gibberish from memory.
        else if firstCycle then Leak.Instr (Leak.Nop Core.FirstCycle) (Nothing, Nothing)
        -- If memory is busy, we stall.
        else if instrBase instr == Leak.Nop Core.MemoryBusBusy then instr
        -- Otherwise we process the decoded instruction.
        else instr

  when load_hazard_current_cycle $ do
    pc <- gets stateDePc
    modify $ \s -> s {stateDeLoadHazard = Just pc}

  when (isCall ir') $
    modify $ \s -> s {stateDeCall = True}

  modify $ \s ->
    s
      { stateExInstr = ir',
        stateExPc = stateDePc s
      }
  where
    instrBase (Leak.Instr b _) = b
    isNopBranchFirstCycle (Leak.Instr (Leak.Nop Core.BranchFirstCycle) _) = True
    isNopBranchFirstCycle _ = False
    isNopLoadHazardFirstCycle (Leak.Instr (Leak.Nop Core.LoadHazardFirstCycle) _) = True
    isNopLoadHazardFirstCycle _ = False
    isLoad (Leak.Instr (Leak.Load {}) _) = True
    isLoad _ = False
    isCall (Leak.Instr Leak.Call _) = True
    isCall _ = False

execute :: SimM ()
execute = do
  instr <- gets stateExInstr
  when (isLoad instr || isStore instr || isCall instr) $
    setMemOutputActive
  mjmpAddr <- getFirst <$> asks Leak.outJumpAddr

  modify $ \s ->
    s
      { stateJumpAddr = mjmpAddr,
        stateMemInstr = instr
      }
  where
    isLoad (Leak.Instr (Leak.Load {}) _) = True
    isLoad _ = False
    isStore (Leak.Instr Leak.Store _) = True
    isStore _ = False
    isCall (Leak.Instr Leak.Call _) = True
    isCall _ = False

memory :: SimM ()
memory = do
  instr <- gets stateMemInstr
  modify $ \s -> s {stateWbInstr = instr}
  case Leak.instrBase instr of
    Leak.Load {} -> do
      setMemOutputActive
      setMeMemInstr
      outputNothing
    Leak.Call {} -> do
      setMemOutputActive
      setMeMemInstr
      outputNothing
    Leak.Store -> do
      setMemOutputActive
      setMeMemInstr
      outputNothing
    _ -> pure ()

writeback :: SimM ()
writeback = do
  instr <- gets stateWbInstr
  halted <- gets stateHalt

  when
    (halted /= AimCore.Running)
    outputNothing

  case Leak.instrBase instr of
    Leak.Break -> do
      outputNothing
      modify $ \s ->
        s
          { stateMemInstr = Leak.nop,
            stateExInstr = Leak.nop,
            stateHalt = AimCore.EBreak
          }
    _ -> pure ()

pipe :: SimM ()
pipe = withCtrlReset $ do
  writeback
  memory
  execute
  decode
  fetch
  where
    withCtrlReset m = do
      firstCycle <- gets stateFirstCycle
      modify $ \s ->
        s
          { stateDecodeLoad = False,
            stateMemOutputActive = False,
            stateMeMemInstr = False,
            stateJumpAddr = Nothing,
            stateDeLoadHazard = Nothing,
            stateDeCall = False,
            stateFirstCycle = firstCycle
          }
      void m
      modify $ \s -> s {stateFirstCycle = False}

circuit :: State -> Leak.Out -> (State, First (Maybe Address))
circuit = flip $ execRWS pipe
