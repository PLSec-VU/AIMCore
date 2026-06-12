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
    stateHalt :: Maybe AimCore.HaltState,
    stateHaltPending :: Maybe AimCore.HaltState,
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
      stateHalt = Nothing,
      stateHaltPending = Nothing,
      stateMeMemInstr = False,
      stateJumpAddr = Nothing,
      stateDeLoadHazard = Nothing,
      stateDeCall = False,
      stateFirstCycle = True
    }

type SimM = RWS Leak.Out (First (Maybe Address)) State

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
  status <- gets stateHalt
  pending <- gets stateHaltPending

  let isHalted = isJust status || isJust pending

  unless (meMemInstr || isHalted) $
    outputPc pc

  let stall = deCall || meMemInstr || isHalted

  let next_pc =
        fromMaybe
          (fromMaybe
             (if stall then pc else pc + 4)
             deLoadHazard)
          mJumpAddr

  modify $ \s ->
    s
      { stateFePc = next_pc,
        stateDePc = if stall then stateDePc s else pc
      }

decode :: SimM ()
decode = do
  instr <- fromMaybe (Leak.Instr (Leak.Nop Instr.MemoryBusBusy) (Nothing, Nothing)) . getFirst <$> asks Leak.outInstr

  exInstr <- gets stateExInstr
  mJumpAddr <- gets stateJumpAddr
  firstCycle <- gets stateFirstCycle
  status <- gets stateHalt
  pending <- gets stateHaltPending

  let branch_first_cycle = isNopBranchFirstCycle exInstr
  let load_hazard_first_cycle = isNopLoadHazardFirstCycle exInstr
  let call_current_cycle = isCall exInstr
  let halt_pending = isJust pending

  -- In Sim, we don't have the real instruction, but we know if it was stalled.
  let load_hazard_current_cycle = case instrBase instr of
        Leak.Nop Instr.LoadHazardFirstCycle -> True
        _ -> Leak.loadHazard instr exInstr

  let ir' =
        -- If a branch was taken in this cycle, we stall.
        if isJust mJumpAddr then Leak.Instr (Leak.Nop Instr.JumpFirstCycle) (Nothing, Nothing)
        -- If a branch was taken in the previous cycle, we stall.
        else if branch_first_cycle then Leak.Instr (Leak.Nop Instr.JumpSecondCycle) (Nothing, Nothing)
        -- If there is a load hazard with the instruction executed in this cycle, we stall.
        else if load_hazard_current_cycle then Leak.Instr (Leak.Nop Instr.LoadHazardFirstCycle) (Nothing, Nothing)
        -- If there was a load hazard in the previous cycle, we stall.
        else if load_hazard_first_cycle then Leak.Instr (Leak.Nop Instr.LoadHazardSecondCycle) (Nothing, Nothing)
        -- If a syscall is executed in this cycle, we stall.
        else if call_current_cycle then Leak.Instr (Leak.Nop Instr.Halted) (Nothing, Nothing)
        -- If the core is not running anymore, we halt.
        else if isJust status || halt_pending then Leak.Instr (Leak.Nop Instr.Halted) (Nothing, Nothing)
        -- If this is the first cycle, the instruction to decode is gibberish from memory.
        else if firstCycle then Leak.Instr (Leak.Nop Instr.FirstCycle) (Nothing, Nothing)
        -- Otherwise we process the instruction from leakage.
        else instr

  when (instrBase ir' == Leak.Nop Instr.Halted) $
    modify $ \s -> s {stateHalt = Just AimCore.SecurityViolation}

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
    isNopBranchFirstCycle (Leak.Instr (Leak.Nop Instr.JumpFirstCycle) _) = True
    isNopBranchFirstCycle _ = False
    isNopLoadHazardFirstCycle (Leak.Instr (Leak.Nop Instr.LoadHazardFirstCycle) _) = True
    isNopLoadHazardFirstCycle _ = False
    isCall (Leak.Instr Leak.Call _) = True
    isCall _ = False

execute :: SimM ()
execute = do
  instr <- gets stateExInstr
  mjmpAddr <- getFirst <$> asks Leak.outJumpAddr
  mBranchTaken <- getFirst . Leak.outBranchTaken <$> ask
  mJumpValid <- getFirst . Leak.outJumpAddrValid <$> ask

  case Leak.instrBase instr of
    Leak.Break -> do
      pc <- gets stateExPc
      modify $ \s ->
        s
          { stateJumpAddr = Nothing,
            stateHaltPending = Just (AimCore.EBreak (pc + 4)),
            stateMemInstr = killJump instr
          }
    _ -> do
      modify $ \s ->
        s
          { stateJumpAddr = mjmpAddr,
            stateMemInstr = killJump instr
          }
      case Leak.instrBase instr of
        Leak.Jump ->
          when (isNothing mjmpAddr && isNothing mJumpValid) $
            modify $ \s -> s {stateHalt = Just AimCore.SecurityViolation}
        Leak.Branch ->
          when (isNothing mBranchTaken) $
            modify $ \s -> s {stateHalt = Just AimCore.SecurityViolation}
        _ -> pure ()
  where
    dummy = ()

memory :: SimM ()
memory = do
  instr <- gets stateMemInstr
  modify $ \s -> s {stateWbInstr = killJump instr}
  
  pending <- gets stateHaltPending
  case pending of
    Just hlt -> modify $ \s -> s {stateHalt = Just hlt, stateHaltPending = Nothing}
    Nothing -> pure ()

  mMeMemInstr <- getFirst . Leak.outMeMemInstr <$> ask
  case mMeMemInstr of
    Just True -> modify (\s -> s {stateMeMemInstr = True}) >> outputNothing
    Just False -> modify $ \s -> s {stateHalt = Just AimCore.SecurityViolation}
    Nothing -> pure ()

killJump :: Leak.Instr -> Leak.Instr
killJump (Leak.Instr Leak.Jump _) = Leak.nop
killJump i = i

writeback :: SimM ()
writeback = do
  instr <- gets stateWbInstr
  halted <- gets stateHalt

  mLeakedHalt <- getFirst . Leak.outHalt <$> ask
  when (isJust halted || isJust mLeakedHalt) $
    outputNothing

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
