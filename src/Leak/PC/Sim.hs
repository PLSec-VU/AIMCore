{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Leak.PC.Sim
  ( State (..),
    init,
    circuit,
  )
where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Control.Monad
import Control.Monad.RWS
import Core (Input (..), initInput)
import qualified Core as AimCore
import qualified Instruction as Instr
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid
import qualified Leak.PC.Leak as Leak
import Memory.Types
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Leak.Instr,
    stateMemInstr :: Leak.Instr,
    stateWbInstr :: Leak.Instr,
    stateJumpAddr :: Maybe Address,
    stateMeMemInstr :: Bool,
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
      stateMeMemInstr = False,
      stateJumpAddr = Nothing,
      stateDeLoadHazard = Nothing,
      stateDeCall = False,
      stateFirstCycle = True
    }

type SimM = RWS Leak.Out (First (Maybe Address)) State

pipe :: SimM ()
pipe = do
  writeback
  memory
  execute
  decode
  fetch

fetch :: SimM ()
fetch = do
  pc <- gets stateFePc
  mJumpAddr <- gets stateJumpAddr
  mDeLoadHazard <- gets stateDeLoadHazard
  mDeCall <- gets stateDeCall
  mMeMemInstr <- gets stateMeMemInstr

  let stall = mDeCall || mMeMemInstr
  let next_pc = fromMaybe (fromMaybe (if stall then pc else pc + 4) mDeLoadHazard) mJumpAddr

  modify $ \s ->
    s
      { stateFePc = next_pc,
        stateDePc = pc
      }

  status <- gets stateHalt
  unless (status /= AimCore.Running) $
    tell $ First $ Just $ Just pc

decode :: SimM ()
decode = do
  instr <- fromMaybe (Leak.Instr (Leak.Nop Instr.MemoryBusBusy) (Nothing, Nothing)) . getFirst <$> asks Leak.outInstr

  exInstr <- gets stateExInstr
  mJumpAddr <- gets stateJumpAddr
  firstCycle <- gets stateFirstCycle

  let branch_first_cycle = isNopBranchFirstCycle exInstr
  let load_hazard_first_cycle = isNopLoadHazardFirstCycle exInstr
  let load_hazard_current_cycle = case instrBase instr of
        Leak.Nop Instr.LoadHazardFirstCycle -> True
        _ -> False
  
  let call_current_cycle = Instr.isCall (toCoreInstr exInstr)
  let break_current_cycle = Instr.isBreak (toCoreInstr exInstr)
  status <- gets stateHalt

  let ir' =
        -- If a branch was taken in this cycle, we stall.
        if isJust mJumpAddr then Leak.Instr (Leak.Nop Instr.BranchFirstCycle) (Nothing, Nothing)
        -- If a branch was taken in the previous cycle, we stall.
        else if branch_first_cycle then Leak.Instr (Leak.Nop Instr.BranchSecondCycle) (Nothing, Nothing)
        -- If there is a load hazard with the instruction executed in this cycle, we stall.
        else if load_hazard_current_cycle then Leak.Instr (Leak.Nop Instr.LoadHazardFirstCycle) (Nothing, Nothing)
        -- If there was a load hazard in the previous cycle, we stall.
        else if load_hazard_first_cycle then Leak.Instr (Leak.Nop Instr.LoadHazardSecondCycle) (Nothing, Nothing)
        -- If a syscall is executed in this cycle, we stall.
        else if call_current_cycle then Leak.Instr (Leak.Nop Instr.SyscallFirstCycle) (Nothing, Nothing)
        -- If we are halting in the current cycle, we stall.
        else if break_current_cycle then Leak.Instr (Leak.Nop Instr.Halted) (Nothing, Nothing)
        -- If the core is not running anymore, we stall.
        else if status /= AimCore.Running then Leak.Instr (Leak.Nop Instr.Halted) (Nothing, Nothing)
        -- If this is the first cycle, the instruction to decode is gibberish from memory.
        else if firstCycle then Leak.Instr (Leak.Nop Instr.FirstCycle) (Nothing, Nothing)
        -- Otherwise we process the instruction from leakage.
        else instr

  when (instrBase ir' == Leak.Nop Instr.SecurityViolation) $
    modify $ \s -> s {stateHalt = AimCore.SecurityViolation}

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
    isNopBranchFirstCycle (Leak.Instr (Leak.Nop Instr.BranchFirstCycle) _) = True
    isNopBranchFirstCycle _ = False
    isNopLoadHazardFirstCycle (Leak.Instr (Leak.Nop Instr.LoadHazardFirstCycle) _) = True
    isNopLoadHazardFirstCycle _ = False
    isCall (Leak.Instr Leak.Call _) = True
    isCall _ = False

execute :: SimM ()
execute = do
  instr <- gets stateExInstr
  mjmpAddr <- getFirst <$> asks Leak.outJumpAddr
  mJumpValid <- getFirst <$> asks Leak.outJumpAddrValid

  modify $ \s ->
    s
      { stateJumpAddr = mjmpAddr,
        stateMemInstr = killJump instr
      }

  case Leak.instrBase instr of
    Leak.Jump ->
      when (isNothing mjmpAddr && isNothing mJumpValid) $
        modify $ \s -> s {stateHalt = AimCore.SecurityViolation}
    Leak.Branch ->
      when (isNothing mjmpAddr && isNothing mJumpValid) $
        modify $ \s -> s {stateHalt = AimCore.SecurityViolation}
    _ -> pure ()
  where
    killJump (Leak.Instr Leak.Jump _) = Leak.nop
    killJump i = i

memory :: SimM ()
memory = do
  instr <- gets stateMemInstr
  modify $ \s -> s {stateWbInstr = instr}

  mMeMemInstr <- getFirst . Leak.outMeMemInstr <$> ask
  case mMeMemInstr of
    Just True -> modify (\s -> s {stateMeMemInstr = True}) >> outputNothing
    Just False -> modify $ \s -> s {stateHalt = AimCore.SecurityViolation}
    Nothing -> pure ()

writeback :: SimM ()
writeback = do
  mHalt <- getFirst . Leak.outHalt <$> ask
  case mHalt of
    Just True -> modify $ \s -> s {stateHalt = AimCore.EBreak}
    _ -> pure ()

withCtrlReset :: SimM () -> SimM ()
withCtrlReset m = do
  firstCycle <- gets stateFirstCycle
  modify $ \s ->
    s
      { stateFirstCycle = firstCycle,
        stateJumpAddr = Nothing,
        stateDeLoadHazard = Nothing,
        stateDeCall = False,
        stateMeMemInstr = False
      }
  void m
  modify $ \s -> s {stateFirstCycle = False}

circuit :: State -> Leak.Out -> (State, First (Maybe Address))
circuit = flip $ execRWS pipe

outputNothing :: SimM ()
outputNothing = tell mempty

toCoreInstr :: Leak.Instr -> Instr.Instruction
toCoreInstr (Leak.Instr b _) = case b of
  Leak.Jump -> Instr.IType Instr.Jump 0 0 0
  Leak.Branch -> Instr.BType Instr.EQ 0 0 0
  Leak.Load r -> Instr.IType (Instr.Load Types.Word Instr.Unsigned) r 0 0
  Leak.Store -> Instr.SType Types.Word 0 0 0
  Leak.Other -> Instr.RType Instr.ADD 0 0 0
  Leak.Call -> Instr.IType (Instr.Env Instr.Call) 0 0 0
  Leak.Break -> Instr.IType (Instr.Env Instr.Break) 0 0 0
  Leak.Nop r -> Instr.Nop r
