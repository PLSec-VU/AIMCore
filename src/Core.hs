{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{- HLINT ignore "Functor law" -}

module Core
  ( initInput,
    init,
    initCtrl,
    withCtrlReset,
    circuit,
    Input (..),
    Output (..),
    State (..),
    HaltState (..),
    fetch,
    decode,
    execute,
    memory,
    writeback,
    CPUM,
    MemAccess (..),
    Control (..),
    alu,
    branch,
    topEntity,
  )
where

import Access
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Instruction hiding (decode)
import Memory.Types
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))

topEntity ::
  (Access f, Generic (f Word), NFDataX (f Word)) =>
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Input f) ->
  Signal System (Output f)
topEntity = exposeClockResetEnable $ mealy circuit init

-- | The input to the CPU.
data Input f = Input
  { -- | Is this an instruction read?
    inputIsInstr :: Bool,
    -- | Reads from memory.
    inputMem :: f Word
  }

deriving instance (Show (f Word)) => Show (Input f)

deriving instance Generic (Input f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (Input f)

-- | A memory access
data MemAccess f = MemAccess
  { -- | Is this an instruction read?
    memIsInstr :: Bool,
    memAddress :: Address,
    memSize :: Size,
    -- | The word to be written, if there is one. If set to `Nothing`, then the
    -- `MemAccess` is a read. Otherwise, it's a write.
    memVal :: Maybe (f Word)
  }

deriving instance (Show (f Word)) => Show (MemAccess f)

deriving instance Generic (MemAccess f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (MemAccess f)

-- | The output of the CPU.
newtype Output f = Output
  { -- | A memory access.
    outMem :: First (MemAccess f)
  }

deriving instance (Show (f Word)) => Show (Output f)

deriving instance Generic (Output f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (Output f)

instance Semigroup (Output f) where
  Output mem <> Output mem' =
    Output (mem <> mem')

instance Monoid (Output f) where
  mempty = Output mempty

-- | CPU halt state
data HaltState = EBreak Address | Syscall Address | SecurityViolation
  deriving (Show, Eq, Generic)

instance NFDataX HaltState

-- | The internal state of the CPU; essentially the pipeline registers.
data State f = State
  { -- | Program counter fetch stage.
    stateFePc :: Address,
    -- | Program counter decode stage.
    stateDePc :: Address,
    -- | Program counter execute stage.
    stateExPc :: Address,
    -- | Instruction execute stage.
    stateExInstr :: Instruction,
    -- | Instruction memory stage.
    stateMeInstr :: Instruction,
    -- | Computation result memory stage.
    stateMeRes :: f Word,
    -- | Address for load and store, memory stage.
    stateMeAddr :: Address,
    -- | Instruction writeback stage.
    stateWbInstr :: Instruction,
    -- | Computation result writeback stage.
    stateWbRes :: f Word,
    -- | Register file.
    stateRegFile :: RegFile f,
    -- | Control/forwarding lines.
    stateCtrl :: Control f,
    -- | CPU halt state.
    stateHalt :: Maybe HaltState,
    -- | Pending halt state (propagating through pipeline to ensure flush).
    stateHaltPending :: Maybe HaltState
  }

deriving instance (Show (f Word)) => Show (State f)

deriving instance (Eq (f Word)) => Eq (State f)

deriving instance Generic (State f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (State f)

-- | Control lines.
data Control f = Control
  { -- | Stores `stateDePc` when the instruction in the `decode` stage has
    --   a load hazard with the instruction in the `execute` stage.
    ctrlDeLoadHazard :: Maybe Address,
    -- | Stores `stateDePc` when the instruction in the `decode` stage has
    --   a store hazard with the instruction in the `execute` stage or the
    --   instruction in the `memory` stage.
    ctrlDeStoreHazard :: Maybe Address,
    -- | Stores the instruction in the `execute` stage.
    ctrlExInstr :: Maybe Instruction,
    -- | Stores the jump address if the instruction in the `execute` stage
    --   results in a jump.
    ctrlExJumpAddr :: Maybe Address,
    -- | Stores the write address if the instruction in the `execute` stage
    --   is a store.
    ctrlExStoreAddr :: Maybe Address,
    -- | `True` when the instruction in the `memory` stage is a store or a load.
    ctrlMeMemInstr :: Bool,
    -- | Stores the write address if the instruction in the `memory` stage
    --   is a store.
    ctrlMeStoreAddr :: Maybe Address,
    -- | Forwards the `rd` register from the `memory` stage to the `execute`
    -- stage.
    ctrlMeRegFwd :: Maybe (RegIdx, f Word),
    -- | Forwards the `rd` register from the `writeback` stage to the `execute`
    -- stage.
    ctrlWbRegFwd :: Maybe (RegIdx, f Word)
  }

deriving instance (Show (f Word)) => Show (Control f)

deriving instance (Eq (f Word)) => Eq (Control f)

deriving instance Generic (Control f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (Control f)

type CPUM f = RWS (Input f) (Output f) (State f)

setLines :: (MonadState (State f) m) => (Control f -> Control f) -> m ()
setLines f = modify $ \s -> s {stateCtrl = f (stateCtrl s)}

-- | No secrets here, buddy: unwrap a word. If it's public, we gucci. If it's
-- private, die.
noSecrets' :: (Access f) => f a -> b -> (a -> CPUM f b) -> CPUM f b
noSecrets' w a = noSecrets w (setSecurityViolation >> pure a)

-- | Run the CPU for one step.
circuit :: (Access f) => State f -> Input f -> (State f, Output f)
circuit = flip $ execRWS pipe

-- | The CPU, composed of each stage.
pipe :: (Access f) => CPUM f ()
pipe = void $ withCtrlReset $ do
  writeback
  memory
  execute
  decode
  fetch

initInput :: (Access f) => Input f
initInput =
  Input
    { inputIsInstr = False,
      inputMem = pure 0
    }

init :: (Access f) => State f
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = Nop FirstCycle,
      stateMeInstr = Nop FirstCycle,
      stateMeRes = pure 0,
      stateMeAddr = 0,
      stateWbInstr = Nop FirstCycle,
      stateWbRes = pure 0,
      stateRegFile = initRF,
      stateCtrl = initCtrl,
      stateHalt = Nothing,
      stateHaltPending = Nothing
    }

-- | Initial control lines.
initCtrl :: Control f
initCtrl =
  Control
    { ctrlDeLoadHazard = Nothing,
      ctrlDeStoreHazard = Nothing,
      ctrlExInstr = Nothing,
      ctrlExJumpAddr = Nothing,
      ctrlExStoreAddr = Nothing,
      ctrlMeMemInstr = False,
      ctrlMeStoreAddr = Nothing,
      ctrlMeRegFwd = Nothing,
      ctrlWbRegFwd = Nothing
    }

-- | The control lines need to be reset every tick.
withCtrlReset :: CPUM f () -> CPUM f (Control f)
withCtrlReset m = do
  modify $ \s -> s {stateCtrl = initCtrl}
  m
  gets stateCtrl

-- | Set security violation flag.
setSecurityViolation :: CPUM f ()
setSecurityViolation =
  modify $ \s -> s {stateHalt = Just SecurityViolation}

-- | The fetch stage.
fetch :: CPUM f ()
fetch = do
  pc <- gets stateFePc
  ctrl <- gets stateCtrl

  -- Always try to read unless the instruction in the `memory` stage is a load or a store.
  unless (ctrlMeMemInstr ctrl) $
    readPC pc
  
  -- We stall if the instruction in the `memory` stage is a load or a store.
  let stall = ctrlMeMemInstr ctrl

  let next_pc =
        fromMaybe
          (fromMaybe
             (fromMaybe
                (if stall then pc else pc + 4)
                (ctrlDeLoadHazard ctrl))
             (ctrlDeStoreHazard ctrl))
          (ctrlExJumpAddr ctrl)

  modify $ \s ->
    s { -- Increment program counter for next fetch.
        stateFePc = next_pc,
        -- Propagate program counter to next stage.
        stateDePc = pc
      }

-- | Decode stage.
decode :: (Access f) => CPUM f ()
decode = do
  input <- ask
  pc <- gets stateDePc
  ctrl <- gets stateCtrl
  
  ir <-
    if inputIsInstr input
      then noSecrets' (inputMem input) (Nop Halted) (pure . decode')
      else pure $ Nop MemoryBusBusy

  let branch_current_cycle = isJust (ctrlExJumpAddr ctrl)
  let branch_previous_cycle = maybe False isNopJumpFirstCycle (ctrlExInstr ctrl)

  let call_current_cycle = maybe False isCall (ctrlExInstr ctrl)
  let break_current_cycle = maybe False isBreak (ctrlExInstr ctrl)
  let halted = maybe False isNopHalted (ctrlExInstr ctrl)
  
  let store_hazard_current_cycle =
        (ctrlMeStoreAddr ctrl == Just pc) ||
        (ctrlExStoreAddr ctrl == Just pc)
  
  let store_hazard_previous_cycle = maybe False isNopStoreHazardFirstCycle (ctrlExInstr ctrl)
  let load_hazard_current_cycle = maybe False (loadHazard ir) (ctrlExInstr ctrl)
  let load_hazard_previous_cycle = maybe False isNopLoadHazardFirstCycle (ctrlExInstr ctrl)

  let ir'
        -- Stall if there is a jump in this cycle.
        | branch_current_cycle = Nop JumpFirstCycle
        -- Stall if there was a jump in the previous cycle.
        | branch_previous_cycle = Nop JumpSecondCycle
        -- Halt if a syscall is executed in this cycle.
        | call_current_cycle = Nop Halted
        -- Halt if a break is executed in this cycle.
        | break_current_cycle = Nop Halted
        -- Halt if the core is not running anymore.
        | halted = Nop Halted
        -- Stall if there is a store hazard in this cycle.
        | store_hazard_current_cycle = Nop StoreHazardFirstCycle
        -- Stall if there was a store hazard in the previous cycle.
        | store_hazard_previous_cycle = Nop StoreHazardSecondCycle
        -- Stall if there is a load hazard in this cycle.
        | load_hazard_current_cycle = Nop LoadHazardFirstCycle
        -- Stall if there was a load hazard in the previous cycle.
        | load_hazard_previous_cycle = Nop LoadHazardSecondCycle
        -- Otherwise we process the decoded instruction.
        | otherwise = ir

  modify $ \s -> s {stateExInstr = ir', stateExPc = pc}

  when (ir' == Nop StoreHazardFirstCycle) $ do
    setLines $ \c -> c {ctrlDeStoreHazard = Just pc}

  when (ir' == Nop LoadHazardFirstCycle) $ do
    setLines $ \c -> c {ctrlDeLoadHazard = Just pc}

-- | Execute stage.
execute :: forall f. (Access f) => CPUM f ()
execute = do
  ir <- gets stateExInstr

  -- Default values.
  modify $ \s ->
    s { stateMeInstr = ir,
        stateMeRes = pure 0,
        stateMeAddr = 0
      }

  setLines $ \c -> c {ctrlExInstr = Just ir}

  case ir of
    Instruction.RType op _ rs1 rs2 -> do
      r1 <- getFirstArg rs1
      r2 <- getSecondArg rs2
      let res = alu op r1 r2
      modify $ \s -> s {stateMeRes = res}
    Instruction.IType (Arith op) _ rs1 imm -> do
      r1 <- getFirstArg rs1
      let imm' = signExtend imm
      let res = alu op r1 (pure imm')
      modify $ \s -> s {stateMeRes = res}
    Instruction.IType (Load _ _) _ rs1 imm -> do
      r1 <- getFirstArg rs1
      let imm' = signExtend imm
      let res = alu ADD r1 (pure imm')
      noSecrets' res () $ \res' -> do
        modify $ \s -> s {stateMeAddr = unpack res'}
    Instruction.SType _ imm rs1 rs2 -> do
      r1 <- getFirstArg rs1
      r2 <- getSecondArg rs2
      let imm' = signExtend imm
      let res = alu ADD r1 (pure imm')
      modify $ \s -> s {stateMeRes = r2}
      noSecrets' res () $ \res' -> do
        modify $ \s -> s {stateMeAddr = unpack res'}
        setLines $ \c -> c {ctrlExStoreAddr = Just $ unpack res'}
    Instruction.BType cmp imm rs1 rs2 -> do
      r1 <- getFirstArg rs1
      r2 <- getSecondArg rs2
      pc <- gets $ pack . stateExPc
      let doBranch = branch cmp r1 r2
      noSecrets' doBranch () $ \doBranch' ->
        when doBranch' $ do
          let imm' = signExtend imm
          let branchAddr = alu ADD (pure pc) (pure imm') :: f Word
          setLines $ \c -> c {ctrlExJumpAddr = fromPublic $ unpack <$> branchAddr}
    Instruction.JType _ imm -> do
      pc <- gets $ pack . stateExPc
      let res = alu ADD (pure pc) (pure 4)
      modify $ \s -> s {stateMeRes = res}
      let imm' = signExtend imm
      let jumpAddr = alu ADD (pure pc) (pure imm') :: f Word
      setLines $ \c -> c {ctrlExJumpAddr = fromPublic $ unpack <$> jumpAddr}
    Instruction.IType Jump _ rs1 imm -> do
      r1 <- getFirstArg rs1
      pc <- gets $ pack . stateExPc
      let res = alu ADD (pure pc) (pure 4)
      modify $ \s -> s {stateMeRes = res}
      noSecrets' r1 () $ \r1' -> do
        let imm' = signExtend imm
        let jumpAddr = alu ADD (pure r1') (pure imm') :: f Word
        setLines $ \c -> c {ctrlExJumpAddr = fromPublic $ unpack <$> jumpAddr}
    Instruction.UType base _ imm -> do
      base' <-
        case base of
          Zero -> pure 0
          PC -> gets $ pack . stateExPc
      let imm' = imm ++# (0 :: BitVector 12)
      let res = alu ADD (pure base') (pure imm')
      modify $ \s -> s {stateMeRes = res}
    Instruction.IType (Env Call) _ _ _ -> do
      pc <- gets stateExPc
      pendingHalt (Syscall (pc + 4))
    Instruction.IType (Env Break) _ _ _ -> do
      pc <- gets stateExPc
      pendingHalt (EBreak (pc + 4))
    Instruction.Nop _ -> pure ()
  where
    getFirstArg :: RegIdx -> CPUM f (f Word)
    getFirstArg idx = do
      rf <- gets stateRegFile
      regWithFwd idx (lookupRF idx rf)

    getSecondArg :: RegIdx -> CPUM f (f Word)
    getSecondArg idx = do
      rf <- gets stateRegFile
      regWithFwd idx (lookupRF idx rf)

    regWithFwd ::  RegIdx -> f Word -> CPUM f (f Word)
    regWithFwd idx def = do
      let checkForFwd line = do
            (fwdIdx, fwdVal) <- MaybeT $ gets $ line . stateCtrl
            guard $ fwdIdx == idx && idx /= 0
            pure fwdVal
      fmap (fromMaybe def) $
        runMaybeT $
          checkForFwd ctrlMeRegFwd <|> checkForFwd ctrlWbRegFwd

    pendingHalt :: HaltState -> CPUM f ()
    pendingHalt hState = do
      modify $ \s -> s {stateHaltPending = Just hState}

alu :: (Access f) => Arith -> f Word -> f Word -> f Word
alu op lhs rhs = case op of
  ADD -> (+) <$> lhs <*> rhs
  SUB -> (-) <$> lhs <*> rhs
  XOR -> (.^.) <$> lhs <*> rhs
  OR -> (.|.) <$> lhs <*> rhs
  AND -> (.&.) <$> lhs <*> rhs
  SLL -> shiftL <$> lhs <*> (shiftBits <$> rhs)
  SRL -> shiftR <$> lhs <*> (shiftBits <$> rhs)
  SRA -> pack <$> (shiftR <$> (sign <$> lhs) <*> (shiftBits <$> rhs))
  SLT -> set <$> ((<) <$> (sign <$> lhs) <*> (sign <$> rhs))
  SLTU -> set <$> ((<) <$> lhs <*> rhs)
  where
    shiftBits s = fromIntegral $ slice d4 d0 s
    sign = unpack @(Signed 32)
    set b = if b then 1 else 0

branch :: (Access f) => Comparison -> f Word -> f Word -> f Bool
branch op lhs rhs = case op of
  EQ -> (==) <$> lhs <*> rhs
  NE -> (/=) <$> lhs <*> rhs
  LT -> (<) <$> (sign <$> lhs) <*> (sign <$> rhs)
  GE -> (>=) <$> (sign <$> lhs) <*> (sign <$> rhs)
  LTU -> (<) <$> lhs <*> rhs
  GEU -> (>=) <$> lhs <*> rhs
  where
    sign = unpack @(Signed 32)

memory :: (Access f) => CPUM f ()
memory = do
  ir <- gets stateMeInstr
  res <- gets stateMeRes
  addr <- gets stateMeAddr
  pending <- gets stateHaltPending

  case pending of
    Just hlt ->
      modify $ \s -> s {stateHalt = Just hlt, stateHaltPending = Nothing}
    Nothing -> pure ()

  modify $ \s -> s { stateWbInstr = ir, stateWbRes = res }

  -- Default register forwarding.
  setLines $ \c -> c {ctrlMeRegFwd = Nothing}

  case ir of
    Instruction.RType _ rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.IType (Arith _) rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.IType (Load size _) _ _ _ -> do
      setLines $ \c -> c {ctrlMeMemInstr = True}
      readRAM addr size
    Instruction.SType size _ _ _ -> do
      setLines $ \c -> c {ctrlMeMemInstr = True, ctrlMeStoreAddr = Just addr}
      writeRAM addr size res
    Instruction.JType rd _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.IType Jump rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.UType _ rd _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    _ -> pure ()

-- | Commit computations to the register file.
writeback :: forall f. (Access f) => CPUM f ()
writeback = do
  input <- asks inputMem
  ir <- gets stateWbInstr
  res <- gets stateWbRes

  case ir of
    Instruction.RType _ rd _ _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Instruction.IType (Arith _) rd _ _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Instruction.IType (Load size sign) rd _ _ -> do
      let val = loadExtend size sign <$> input
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, val)}
      writeRF rd val
    Instruction.JType rd _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Instruction.IType Jump rd _ _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Instruction.UType _ rd _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Nothing}
  where
    writeRF :: RegIdx -> f Word -> CPUM f ()
    writeRF idx val =
      modify $ \s -> s {stateRegFile = modifyRF idx val (stateRegFile s)}

readPC :: (MonadWriter (Output f) m) => Address -> m ()
readPC addr =
  tell $
    (mempty :: Output f)
      { outMem =
          pure $
            MemAccess
              { memIsInstr = True,
                memAddress = addr,
                memSize = Word,
                memVal = Nothing
              }
      }

readRAM :: (MonadWriter (Output f) m) => Address -> Size -> m ()
readRAM addr size =
  tell $
    (mempty :: Output f)
      { outMem =
          pure $
            MemAccess
              { memIsInstr = False,
                memAddress = addr,
                memSize = size,
                memVal = Nothing
              }
      }

writeRAM :: (MonadWriter (Output f) m) => Address -> Size -> f Word -> m ()
writeRAM addr size val =
  tell $
    (mempty :: Output f)
      { outMem =
          pure $
            MemAccess
              { memIsInstr = False,
                memAddress = addr,
                memSize = size,
                memVal = Just val
              }
      }
