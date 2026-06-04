{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
data Output f = Output
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
data HaltState = Running | EBreak | Syscall | SecurityViolation
  deriving (Show, Eq, Generic)

instance NFDataX HaltState

-- | The internal state of the CPU; essentially the pipeline registers.
data State f = State
  { -- | Program counter fetch stage
    stateFePc :: Address,
    -- | Program counter decode stage
    stateDePc :: Address,
    -- | Program counter execute stage
    stateExPc :: Address,
    -- | Instruction register execute stage
    stateExInstr :: Instruction,
    -- | Instruction register memory stage
    stateMeInstr :: Instruction,
    -- | ALU result register memory stage
    stateMeAluRes :: f Word,
    -- | Memory value to write for stores (`stateMeAluRes` only contains the address).
    stateMeStoreRes :: f Word,
    -- | Instruction register writeback stage
    stateWbInstr :: Instruction,
    -- | ALU result register writeback stage
    stateWbAluRes :: f Word,
    -- | Register file
    stateRegFile :: RegFile f,
    -- | Control/forwarding lines.
    stateCtrl :: Control f,
    -- | CPU halt state
    stateHalt :: HaltState
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
    -- | Stores the instruction in the `execute` stage.
    ctrlExInstr :: Maybe Instruction,
    -- | Stores the new PC if the instruction in the `execute` stage results in a jump.
    ctrlExAddress :: Maybe Address,
    -- | `True` when the instruction in the `memory` stage is a store or a load.
    ctrlMeMemInstr :: Bool,
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
noSecrets' w a m = noSecrets w (setSecurityViolation >> pure a) m

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
      stateMeAluRes = pure 0,
      stateMeStoreRes = pure 0,
      stateWbInstr = Nop FirstCycle,
      stateWbAluRes = pure 0,
      stateRegFile = initRF,
      stateCtrl = initCtrl,
      stateHalt = Running
    }

-- | Initial control lines.
initCtrl :: Control f
initCtrl =
  Control
    { ctrlDeLoadHazard = Nothing,
      ctrlExInstr = Nothing,
      ctrlExAddress = Nothing,
      ctrlMeMemInstr = False,
      ctrlMeRegFwd = Nothing,
      ctrlWbRegFwd = Nothing
    }

-- | The control lines need to be reset every tick.
withCtrlReset :: CPUM f () -> CPUM f (Control f)
withCtrlReset m = do
  modify $ \s -> s {stateCtrl = initCtrl}
  m
  ctrl <- gets stateCtrl
  pure ctrl

-- | Stop the CPU due to ebreak.
halt :: CPUM f ()
halt =
  modify $ \s -> s {stateHalt = EBreak}

-- | Stop the CPU due to syscall.
setSyscall :: CPUM f ()
setSyscall =
  modify $ \s -> s {stateHalt = Syscall}

-- | Set security violation flag.
setSecurityViolation :: CPUM f ()
setSecurityViolation =
  modify $ \s -> s {stateHalt = SecurityViolation}

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
          ( fromMaybe
              (if stall then pc else pc + 4)
              $ ctrlDeLoadHazard ctrl
          )
          $ ctrlExAddress ctrl

  modify $ \s ->
    s -- Increment program counter for next fetch.
      { stateFePc = next_pc,
        -- Propagate program counter to next stage.
        stateDePc = pc
      }

-- | Decode stage.
decode :: (Access f) => CPUM f ()
decode = do
  input <- ask
  ctrl <- gets stateCtrl  

  ir <-
    if (inputIsInstr input)
      then noSecrets' (inputMem input) (Nop Halted) (pure . decode')
      else pure $ Nop MemoryBusBusy

  let branch_first_cycle = maybe False isNopBranchFirstCycle (ctrlExInstr ctrl)
  let load_hazard_current_cycle = maybe False (loadHazard ir) (ctrlExInstr ctrl)
  let load_hazard_first_cycle = maybe False isNopLoadHazardFirstCycle (ctrlExInstr ctrl)
  let call_current_cycle = maybe False isCall (ctrlExInstr ctrl)
  let break_current_cycle = maybe False isBreak (ctrlExInstr ctrl)
  let halted = maybe False isNopHalted (ctrlExInstr ctrl)

  let ir'
        -- If a branch was taken in this cycle, we stall.
        | isJust (ctrlExAddress ctrl) = Nop BranchFirstCycle
        -- If a branch was taken in the previous cycle, we stall.
        | branch_first_cycle = Nop BranchSecondCycle
        -- If there is a load hazard with the instruction executed in this cycle, we stall.
        | load_hazard_current_cycle = Nop LoadHazardFirstCycle
        -- If there was a load hazard in the previous cycle, we stall.
        | load_hazard_first_cycle = Nop LoadHazardSecondCycle
        -- If a syscall is executed in this cycle, we halt.
        | call_current_cycle = Nop Halted
        -- If a break is executed in this cycle, we halt.
        | break_current_cycle = Nop Halted
        -- If the core is not running anymore, we halt.
        | halted = Nop Halted
        -- Otherwise we process the decoded instruction.
        | otherwise = ir

  modify $ \s ->
    s
      { stateExInstr = ir',
        stateExPc = stateDePc s
      }

  when load_hazard_current_cycle $ do
    pc <- gets stateDePc
    setLines $
      \c -> c {ctrlDeLoadHazard = Just pc}

-- | Execute stage.
execute :: forall f. (Access f) => CPUM f ()
execute = do
  ir <- gets stateExInstr
  modify $ \s -> s {stateMeInstr = ir, stateMeStoreRes = pure 0}
  setLines $ \c -> c {ctrlExInstr = Just ir}

  -- Fetch alu operands
  aluInputs <- runMaybeT $ fetchALUOperands ir

  modify $ \s ->
    let aluNOP = (ADD, pure 0, pure 0)
        (op, lhs, rhs) = fromMaybe aluNOP aluInputs
        res = alu op lhs rhs
     in s {stateMeAluRes = res}
  where
    fetchALUOperands :: Instruction -> MaybeT (CPUM f) (Arith, f Word, f Word)
    fetchALUOperands ir =
      case ir of
        Instruction.RType op _ _ _ -> do
          r1 <- rs1
          r2 <- rs2
          pure (op, r1, r2)
        Instruction.IType (Arith op) _ _ imm -> do
          r1 <- rs1
          let imm' = signExtend imm
          pure (op, r1, pure imm')
        Instruction.IType (Load _ _) _ _ imm -> do
          r1 <- rs1
          let imm' = signExtend imm
          pure (ADD, r1, pure imm')
        Instruction.SType _ imm _ _ -> do
          r1 <- rs1
          r2 <- rs2
          let imm' = signExtend imm
          modify $ \s -> s {stateMeStoreRes = r2}
          pure (ADD, r1, pure imm')
        Instruction.BType cmp imm _ _ -> do
          r1 <- rs1
          r2 <- rs2
          pc <- gets $ pack . stateExPc
          let doBranch = branch cmp r1 r2
          lift $ noSecrets' doBranch () $ \doBranch' ->
            when doBranch' $ do
              let branchAddr :: f Address
                  branchAddr = unpack <$> alu ADD (pure pc) (pure $ signExtend imm)
              setLines $
                \c -> c {ctrlExAddress = fromPublic branchAddr}
          empty
        Instruction.JType _ imm -> do
          pc <- gets $ pack . stateExPc
          let jumpAddr :: f Address
              jumpAddr = unpack <$> alu ADD (pure pc) (pure $ signExtend imm)
          setLines $
            \c -> c {ctrlExAddress = fromPublic jumpAddr}
          pure (ADD, pure pc, pure 4)
        Instruction.IType Jump _ _ imm -> do
          r1 <- rs1
          pc <- gets $ pack . stateExPc
          lift $ noSecrets' r1 () $ \r1' -> do
            let jumpAddr :: f Address
                jumpAddr = unpack <$> alu ADD (pure r1') (pure $ signExtend imm)
            setLines $
              \c -> c {ctrlExAddress = fromPublic jumpAddr}
          pure (ADD, pure pc, pure 4)
        Instruction.UType base _ imm -> do
          base' <- case base of
            Zero -> pure 0
            PC -> gets $ pack . stateExPc
          let imm' = imm ++# (0 :: BitVector 12)
          pure (ADD, pure base', pure imm')
        Instruction.IType (Env Call) _ _ _ -> empty
        Instruction.IType (Env Break) _ _ _ -> empty
        Instruction.Nop _ -> empty

    rs1 :: MaybeT (CPUM f) (f Word)
    rs1 = do
      ir <- gets stateExInstr
      let idx = fromMaybe 0 $ getRs1 ir
      rf <- gets stateRegFile
      lift $ regWithFwd getRs1 (lookupRF idx rf)

    rs2 :: MaybeT (CPUM f) (f Word)
    rs2 = do
      ir <- gets stateExInstr
      let idx = fromMaybe 0 $ getRs2 ir
      rf <- gets stateRegFile
      lift $ regWithFwd getRs2 (lookupRF idx rf)

    regWithFwd :: (Instruction -> Maybe RegIdx) -> f Word -> CPUM f (f Word)
    regWithFwd getR def = do
      ir <- gets stateExInstr
      let checkForFwd line = do
            (fwdIdx, fwdVal) <- MaybeT $ gets $ line . stateCtrl
            guard (hazardRW getR ir fwdIdx)
            pure fwdVal
      fmap (fromMaybe def) $
        runMaybeT $
          checkForFwd ctrlMeRegFwd <|> checkForFwd ctrlWbRegFwd

    hazardRW :: (Instruction -> Maybe RegIdx) -> Instruction -> RegIdx -> Bool
    hazardRW getR ir rd = isJust $ do
      rs <- getR ir
      guard $ rd /= 0 && rs == rd

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
  res <- gets stateMeAluRes
  val <- gets stateMeStoreRes

  modify $ \s ->
    s {stateWbInstr = ir, stateWbAluRes = res}

  -- Default register forwarding.
  setLines $ \c -> c {ctrlMeRegFwd = Nothing}

  case ir of
    Instruction.RType _ rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.IType (Arith _) rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.IType (Load size _) _ _ _ ->
      noSecrets' res () $ \res' -> do
        setLines $ \c ->
          c {ctrlMeMemInstr = True}
        readRAM (unpack res') size
    Instruction.SType size _ _ _ ->
      noSecrets' res () $ \res' -> do
        setLines $ \c ->
          c {ctrlMeMemInstr = True}
        writeRAM (unpack res') size val
    Instruction.JType rd _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.IType Jump rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.UType _ rd _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Instruction.IType (Env Call) _ _ _ ->
      setSyscall
    Instruction.IType (Env Break) _ _ _ ->
      halt
    _ -> pure ()

-- | Commit computations to the register file.
writeback :: forall f. (Access f) => CPUM f ()
writeback = do
  input <- asks inputMem
  ir <- gets stateWbInstr
  res <- gets stateWbAluRes

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
      writeRF 0 (pure 0 :: f Word)
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
