-- | The leakage simulator: a pipeline with the same stage structure and timing
-- as "Core.Pipeline", which sees only the leakage -- never the program, the
-- register file or memory contents.
--
-- Each cycle it may receive the 'Leak' of the instruction entering its execute
-- stage ('SimInput'; 'Nothing' when there is none) and emits the bus
-- observation the core would make that cycle ('SimOutput'). The leakage proof
-- shows that, fed the ISA's leakage, it produces the same observations as the
-- core, so everything an attacker on the bus sees is a function of the leakage
-- alone.
module Proof.Core.Leakage.Simulator
  ( SimInput,
    SimOutput,
    SimExInstrType (..),
    SimDataAccess (..),
    SimState (..),
    SimControl (..),
    SimM,
    simCircuit,
    simInit,
    simInitCtrl,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Data.Maybe (fromMaybe, isJust)
import Core.Instruction (storeHazard)
import Memory.Types
import Proof.Core.Leakage.Model
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The input of the simulator: the leakage of the instruction a hop starts
-- with, or 'Nothing' in the middle of a driver hop.
type SimInput = Maybe Leak

-- | The output of the simulator.
type SimOutput = Obs

data SimExInstrType
  = -- | A decoded instruction, whose leakage is the input.
    DecodedInstr
  | -- | First instruction discarded because of a jump.
    StallJumpFirstCycle
  | -- | Second instruction discarded because of a jump.
    StallJumpSecondCycle
  | -- | First instruction discarded because of a load hazard.
    StallLoadHazardFirstCycle
  | -- | Second instruction discarded because of a load hazard.
    StallLoadHazardSecondCycle
  | -- | First instruction discarded because of a store hazard.
    StallStoreHazardFirstCycle
  | -- | Second instruction discarded because of a store hazard.
    StallStoreHazardSecondCycle
  | -- | No instruction this cycle: the fetch stage did not fetch, because the
    -- memory stage used the bus.
    StallNoInstruction
  | -- | First cycle.
    FirstCycle
  | -- | First cycle after an environment instruction: the halt is on its way
    -- through the pipeline, in the memory stage.
    PendingHaltFirstCycle
  | -- | Second cycle after an environment instruction: the halt is in the
    -- writeback stage, which halts the core.
    PendingHaltSecondCycle
  | -- | The core has halted.
    Halted
  deriving (Eq, Show, Generic, NFDataX)

-- | What the instruction in the memory stage does with memory: nothing, or a
-- load or a store of a size at an address. The only part of an instruction's
-- leakage the memory stage uses, so the simulator keeps no more of it.
data SimDataAccess
  = SimNoDataAccess
  | SimLoad Size Address
  | SimStore Size Address
  deriving (Eq, Show, Generic, NFDataX)

-- | The internal state of the simulator.
data SimState = SimState
  { -- | Program counter fetch stage.
    simStateFePc :: Address,
    -- | Program counter decode stage.
    simStateDePc :: Address,
    -- | Does the decode stage expect an instruction?
    simStateDeExpInstr :: Bool,
    -- | Instruction execute stage.
    simStateExInstrType :: SimExInstrType,
    -- | What the instruction in the memory stage does with memory.
    simStateMeAccess :: SimDataAccess,
    -- | Control/forwarding lines.
    simStateCtrl :: SimControl
  }
  deriving (Eq, Show, Generic, NFDataX)

-- | Control lines.
data SimControl = SimControl
  { -- | Stores `stateDePc` when the instruction in the `decode` stage has
    --   a load hazard with the instruction in the `execute` stage.
    simCtrlDeLoadHazard :: Maybe Address,
    -- | Stores `stateDePc` when the instruction in the `decode` stage has
    --   a store hazard with the instruction in the `execute` stage or the
    --   instruction in the `memory` stage.
    simCtrlDeStoreHazard :: Maybe Address,
    -- | Stores the type of instruction in the `execute` stage (if it's a
    -- decoded instruction versus various stalls).
    simCtrlExInstrType :: Maybe SimExInstrType,
    -- | `True` when the leaked instruction is an environment instruction. To
    -- maintain consistency with the core, the execute stage is the only one
    -- that examines the input, which is why this control line is needed.
    simCtrlExEnvInstr :: Bool,
    -- | Stores the jump address if the instruction in the `execute` stage
    --   results in a jump.
    simCtrlExJumpAddr :: Maybe Address,
    -- | Stores the write address and size if the instruction in the `execute`
    -- stage
    --   is a store.
    simCtrlExStoreAddrSize :: Maybe (Address, Size),
    -- | Stores the load hazard bit if the instruction in the `execute` stage
    --   is a load.
    simCtrlExLoadHazard :: Maybe Bool,
    -- | `True` when the instruction in the `memory` stage is a store or a load.
    simCtrlMeMemInstr :: Bool,
    -- | Stores the write address and size if the instruction in the `memory`
    -- stage
    --   is a store.
    simCtrlMeStoreAddrSize :: Maybe (Address, Size)
  }
  deriving (Eq, Show, Generic, NFDataX)

type SimM = RWS SimInput SimOutput SimState

simSetLines :: (SimControl -> SimControl) -> SimM ()
simSetLines f = modify $ \s -> s {simStateCtrl = f (simStateCtrl s)}

-- | Run the simulator for one step.
simCircuit :: SimState -> SimInput -> (SimState, SimOutput)
simCircuit = flip $ execRWS simPipe

-- | The simulator, composed of each stage.
simPipe :: SimM ()
simPipe = do
  -- The control lines need to be reset every tick.
  modify $ \s -> s {simStateCtrl = simInitCtrl}
  simWriteback
  simMemory
  simExecute
  simDecode
  simFetch
  -- The lines only carry values within a tick, so none of them survives it,
  -- as in 'Core.Pipeline.pipe'.
  modify $ \s -> s {simStateCtrl = simInitCtrl}

simInit :: SimState
simInit =
  SimState
    { simStateFePc = initPc,
      simStateDePc = 0,
      simStateDeExpInstr = False,
      simStateExInstrType = FirstCycle,
      simStateMeAccess = SimNoDataAccess,
      simStateCtrl = simInitCtrl
    }

-- | Initial control lines.
simInitCtrl :: SimControl
simInitCtrl =
  SimControl
    { simCtrlDeLoadHazard = Nothing,
      simCtrlDeStoreHazard = Nothing,
      simCtrlExInstrType = Nothing,
      simCtrlExEnvInstr = False,
      simCtrlExJumpAddr = Nothing,
      simCtrlExStoreAddrSize = Nothing,
      simCtrlExLoadHazard = Nothing,
      simCtrlMeMemInstr = False,
      simCtrlMeStoreAddrSize = Nothing
    }

-- | Fetch stage.
simFetch :: SimM ()
simFetch = do
  pc <- gets simStateFePc
  ctrl <- gets simStateCtrl

  -- We stall if the instruction in the `memory` stage is a load or a store.
  let stall = simCtrlMeMemInstr ctrl

  -- Always try to read unless we stall.
  unless stall $
    simReadPC pc

  let next_pc =
        fromMaybe
          (fromMaybe
             (fromMaybe
                (if stall then pc else pc + 4)
                (simCtrlDeLoadHazard ctrl))
             (simCtrlDeStoreHazard ctrl))
          (simCtrlExJumpAddr ctrl)

  modify $ \s ->
    s { -- Increment program counter for next fetch.
        simStateFePc = next_pc,
        -- Propagate program counter to next stage.
        simStateDePc = pc,
        -- Decode expects an instruction next cycle iff we fetch now.
        simStateDeExpInstr = not stall
      }

-- | Decode stage.
simDecode :: SimM ()
simDecode = do
  pc <- gets simStateDePc
  expInstr <- gets simStateDeExpInstr
  ctrl <- gets simStateCtrl

  let irt = if expInstr then DecodedInstr else StallNoInstruction

  let halted = simCtrlExInstrType ctrl == Just Halted
  let env_instr_two_cycles_ago =
        simCtrlExInstrType ctrl == Just PendingHaltSecondCycle
  let env_instr_previous_cycle =
        simCtrlExInstrType ctrl == Just PendingHaltFirstCycle
  let jump_previous_cycle = simCtrlExInstrType ctrl == Just StallJumpFirstCycle
  let store_hazard_previous_cycle =
        simCtrlExInstrType ctrl == Just StallStoreHazardFirstCycle
  let load_hazard_previous_cycle =
        simCtrlExInstrType ctrl == Just StallLoadHazardFirstCycle

  let env_instr_current_cycle = simCtrlExEnvInstr ctrl
  let jump_current_cycle = isJust (simCtrlExJumpAddr ctrl)

  let store_hazard_current_cycle =
        maybe False (storeHazard pc) (simCtrlExStoreAddrSize ctrl) ||
        maybe False (storeHazard pc) (simCtrlMeStoreAddrSize ctrl)

  let load_hazard_current_cycle =
        expInstr && fromMaybe False (simCtrlExLoadHazard ctrl)

  let irt'
        -- Stay halted once the core has halted.
        | halted = Halted
        -- The halt reaches the writeback stage after the second pending cycle.
        | env_instr_two_cycles_ago = Halted
        -- The halt moves on from the memory stage to the writeback stage.
        | env_instr_previous_cycle = PendingHaltSecondCycle
        -- Stall if there was a jump in the previous cycle.
        | jump_previous_cycle = StallJumpSecondCycle
        -- Stall if there was a store hazard in the previous cycle.
        | store_hazard_previous_cycle = StallStoreHazardSecondCycle
        -- Stall if there was a load hazard in the previous cycle.
        | load_hazard_previous_cycle = StallLoadHazardSecondCycle
        -- An environment instruction in this cycle starts the halt.
        | env_instr_current_cycle = PendingHaltFirstCycle
        -- Stall if there is a jump in this cycle.
        | jump_current_cycle = StallJumpFirstCycle
        -- Stall if there is a store hazard in this cycle.
        | store_hazard_current_cycle = StallStoreHazardFirstCycle
        -- Stall if there is a load hazard in this cycle.
        | load_hazard_current_cycle = StallLoadHazardFirstCycle
        -- Otherwise we process the decoded instruction.
        | otherwise = irt

  modify $ \s -> s {simStateExInstrType = irt'}

  when (irt' == StallStoreHazardFirstCycle) $ do
    simSetLines $ \c -> c {simCtrlDeStoreHazard = Just pc}

  when (irt' == StallLoadHazardFirstCycle) $ do
    simSetLines $ \c -> c {simCtrlDeLoadHazard = Just pc}

-- | Execute stage.
simExecute :: SimM ()
simExecute = do
  input <- ask
  irt <- gets simStateExInstrType

  simSetLines $ \c -> c {simCtrlExInstrType = Just irt}

  case irt of
    DecodedInstr ->
      case input of
        Just ir -> do
          case ir of
            LArith ->
              modify $ \s -> s {simStateMeAccess = SimNoDataAccess}
            LJump addr -> do
              modify $ \s -> s {simStateMeAccess = SimNoDataAccess}
              simSetLines $ \c -> c {simCtrlExJumpAddr = Just addr}
            LLoad size addr haz -> do
              modify $ \s -> s {simStateMeAccess = SimLoad size addr}
              simSetLines $ \c -> c {simCtrlExLoadHazard = Just haz}
            LStore size addr -> do
              modify $ \s -> s {simStateMeAccess = SimStore size addr}
              simSetLines $ \c -> c {simCtrlExStoreAddrSize = Just (addr, size)}
            LEnv -> do
              modify $ \s -> s {simStateMeAccess = SimNoDataAccess}
              simSetLines $ \c -> c {simCtrlExEnvInstr = True}
        Nothing -> modify $ \s -> s {simStateMeAccess = SimNoDataAccess}
    _ -> modify $ \s -> s {simStateMeAccess = SimNoDataAccess}

-- | Memory stage.
simMemory :: SimM ()
simMemory = do
  ir <- gets simStateMeAccess

  case ir of
    SimNoDataAccess -> pure ()
    SimLoad size addr -> do
      simSetLines $ \c -> c {simCtrlMeMemInstr = True}
      simReadRAM addr size
    SimStore size addr -> do
      simSetLines $ \c ->
        c {simCtrlMeMemInstr = True, simCtrlMeStoreAddrSize = Just (addr, size)}
      simWriteRAM addr size

-- | Writeback stage.
simWriteback :: SimM ()
simWriteback = pure ()

simReadPC :: Address -> SimM ()
simReadPC addr =
  tell $ Fetch addr

simReadRAM :: Address -> Size -> SimM ()
simReadRAM addr size =
  tell $ DataRead addr size

simWriteRAM :: Address -> Size -> SimM ()
simWriteRAM addr size =
  tell $ DataWrite addr size
