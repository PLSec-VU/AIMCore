-- | The leakage simulator: a pipeline with the same stage structure and timing
-- as "Core", which sees only the leakage -- never the program, the register
-- file or memory contents.
--
-- Each cycle it may receive the 'Leak' of the instruction entering its execute
-- stage ('SimInput'; 'Nothing' when there is none) and emits the bus
-- observation the core would make that cycle ('SimOutput'). The leakage proof
-- shows that, fed the ISA's leakage, it produces the same observations as the
-- core, so everything an attacker on the bus sees is a function of the leakage
-- alone.
module Proof.Leakage.Simulator
  ( SimInput,
    SimOutput,
    SimExInstrType (..),
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
import Instruction (storeHazard)
import Memory.Types
import Proof.Leakage.Model
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The input of the simulator.
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
  | -- | No instruction read because of memory bus overload.
    StallMemoryBusBusy
  | -- | First cycle.
    FirstCycle
  | -- | The core has halted.
    Halted
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
    -- | Instruction memory stage.
    simStateMeInstr :: Leak,
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
    -- | Stores the type of instruction in the `execute` stage (if it's a decoded
    -- instruction versus various stalls).
    simCtrlExInstrType :: Maybe SimExInstrType,
    -- | `True` when the leaked instruction is an environment instruction. To
    -- maintain consistency with the core, the execute stage is the only one
    -- that examines the input, which is why this control line is needed.
    simCtrlExEnvInstr :: Bool,
    -- | Stores the jump address if the instruction in the `execute` stage
    --   results in a jump.
    simCtrlExJumpAddr :: Maybe Address,
    -- | Stores the write address and size if the instruction in the `execute` stage
    --   is a store.
    simCtrlExStoreAddrSize :: Maybe (Address, Size),
    -- | Stores the load hazard bit if the instruction in the `execute` stage
    --   is a load.
    simCtrlExLoadHazard :: Maybe Bool,
    -- | `True` when the instruction in the `memory` stage is a store or a load.
    simCtrlMeMemInstr :: Bool,
    -- | Stores the write address and size if the instruction in the `memory` stage
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

simInit :: SimState
simInit =
  SimState
    { simStateFePc = initPc,
      simStateDePc = 0,
      simStateDeExpInstr = False,
      simStateExInstrType = FirstCycle,
      simStateMeInstr = LArith,
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

  let irt = if expInstr then DecodedInstr else StallMemoryBusBusy

  let halted = simCtrlExInstrType ctrl == Just Halted
  let env_instr_current_cycle = simCtrlExEnvInstr ctrl
  
  let jump_previous_cycle = simCtrlExInstrType ctrl == Just StallJumpFirstCycle
  let store_hazard_previous_cycle = simCtrlExInstrType ctrl == Just StallStoreHazardFirstCycle
  let load_hazard_previous_cycle = simCtrlExInstrType ctrl == Just StallLoadHazardFirstCycle

  let jump_current_cycle = isJust (simCtrlExJumpAddr ctrl)

  let store_hazard_current_cycle =
        maybe False (storeHazard pc) (simCtrlExStoreAddrSize ctrl) ||
        maybe False (storeHazard pc) (simCtrlMeStoreAddrSize ctrl)

  let load_hazard_current_cycle = expInstr && fromMaybe False (simCtrlExLoadHazard ctrl)

  let irt'
        -- Halt if the core is not running anymore.
        | halted = Halted
        -- Halt if there is an environment instruction in this cycle.
        | env_instr_current_cycle = Halted
        -- Stall if there was a jump in the previous cycle.
        | jump_previous_cycle = StallJumpSecondCycle
        -- Stall if there was a store hazard in the previous cycle.
        | store_hazard_previous_cycle = StallStoreHazardSecondCycle
        -- Stall if there was a load hazard in the previous cycle.
        | load_hazard_previous_cycle = StallLoadHazardSecondCycle
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
          modify $ \s -> s {simStateMeInstr = ir}

          case ir of
            LArith -> pure ()
            LJump addr ->
              simSetLines $ \c -> c {simCtrlExJumpAddr = Just addr}
            LLoad _ _ haz ->
              simSetLines $ \c -> c {simCtrlExLoadHazard = Just haz}
            LStore size addr ->
              simSetLines $ \c -> c {simCtrlExStoreAddrSize = Just (addr, size)}
            LEnv ->
              simSetLines $ \c -> c {simCtrlExEnvInstr = True}
        Nothing -> modify $ \s -> s {simStateMeInstr = LArith}
    _ -> modify $ \s -> s {simStateMeInstr = LArith}

-- | Memory stage.
simMemory :: SimM ()
simMemory = do
  ir <- gets simStateMeInstr

  case ir of
    LArith -> pure ()
    LJump _ -> pure ()
    LLoad size addr _ -> do
      simSetLines $ \c -> c {simCtrlMeMemInstr = True}
      simReadRAM addr size
    LStore size addr -> do
      simSetLines $ \c ->
        c {simCtrlMeMemInstr = True, simCtrlMeStoreAddrSize = Just (addr, size)}
      simWriteRAM addr size
    LEnv -> pure ()

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
