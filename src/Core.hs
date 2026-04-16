{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Core (
  initInput,
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
  CPUM (..),
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
import Types
import qualified Types
import Util
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
  { inputIsInstr :: Bool
  -- ^ Is this an instruction read?
  , inputMem :: f Word
  -- ^ Reads from memory.
  , inputRs1 :: f Word
  -- ^ Read from the register file (corresponding to the index requested in
  -- the `Output`'s `outRs1` field).
  , inputRs2 :: f Word
  -- ^ Read from the register file (corresponding to the index requested in
  -- the `Output`'s `outRs2` field).
  }

deriving instance (Show (f Word)) => Show (Input f)

deriving instance (Generic (f Word)) => Generic (Input f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (Input f)

-- | A memory access
data MemAccess f = MemAccess
  { memIsInstr :: Bool
  -- ^ Is this an instruction read?
  , memAddress :: Address
  , memSize :: Size
  , memVal :: Maybe (f Word)
  -- ^ The word to be written, if there is one. If set to `Nothing`, then the
  -- `MemAccess` is a read. Otherwise, it's a write.
  }

deriving instance (Show (f Word)) => Show (MemAccess f)

deriving instance (Generic (f Word)) => Generic (MemAccess f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (MemAccess f)

-- | The output of the CPU.
data Output f = Output
  { outMem :: First (MemAccess f)
  -- ^ A memory access.
  , outRs1 :: First RegIdx
  -- ^ A read request from the register file; stores it in the `inputRs1`
  -- field of `Input` on the next tick.
  , outRs2 :: First RegIdx
  -- ^ A read request from the register file; stores it in the `inputRs2`
  -- field of `Input` on the next tick.
  , outRd :: First (RegIdx, f Word)
  -- ^ A write to the register file.
  , outSyscall :: First Bool
  -- ^ A syscall request.
  , outHalt :: First Bool
  -- ^ Are we done?
  }

deriving instance (Show (f Word)) => Show (Output f)

deriving instance (Generic (f Word)) => Generic (Output f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (Output f)

instance Semigroup (Output f) where
  Output mem rs1 rs2 rd syscall hlt <> Output mem' rs1' rs2' rd' syscall' hlt' =
    Output (mem <> mem') (rs1 <> rs1') (rs2 <> rs2') (rd <> rd') (syscall <> syscall') (hlt <> hlt')

instance Monoid (Output f) where
  mempty = Output mempty mempty mempty mempty mempty mempty

-- | CPU halt state
data HaltState = Running | EBreak | SecurityViolation
  deriving (Show, Eq, Generic)

instance NFDataX HaltState

-- | The internal state of the CPU; essentially the pipeline registers.
data State f = State
  { stateFePc :: Address
  -- ^ Program counter fetch stage
  , stateDePc :: Address
  -- ^ Program counter decode stage
  , stateExPc :: Address
  -- ^ Program counter execute stage
  , stateExInstr :: Instruction
  -- ^ Instruction register execute stage
  , stateMemInstr :: Instruction
  -- ^ Instruction register memory stage
  , stateMemRes :: f Word
  -- ^ ALU result register memory stage
  , stateMemVal :: f Word
  -- ^ Memory value to write for stores (`stateMemRes` only contains the address).
  , stateWbInstr :: Instruction
  -- ^ Instruction register writeback stage
  , stateWbRes :: f Word
  -- ^ ALU result register writeback stage
  , stateCtrl :: Control f
  -- ^ Control/forwarding lines.
  , stateHalt :: HaltState
  -- ^ CPU halt state
  }

deriving instance (Show (f Word)) => Show (State f)

deriving instance (Eq (f Word)) => Eq (State f)

deriving instance (Generic (f Word)) => Generic (State f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (State f)

-- | Control lines.
data Control f = Control
  { ctrlFirstCycle :: Bool
  -- ^ `True` during the first step of execution.
  , ctrlDecodeLoad :: Bool
  -- ^ `True` when the instruction in the `decode` stage is a load.
  , ctrlMeOutputActive :: Bool
  -- ^ `True` when the instruction in the `memory` stage results in a read/write.
  , ctrlMeRegFwd :: Maybe (RegIdx, f Word)
  -- ^ Forwards the rd register from the `memory` stage to the `execute`
  -- stage.  The `RegIdx` payload is necessary to know what the destination
  -- register is for the instruction in the `memory` stage: it's too late to
  -- check this in the `execute` stage because it will already have been
  -- overwritten with the instruction for the next cycle.
  , ctrlWbRegFwd :: Maybe (RegIdx, f Word)
  -- ^ Forwards the `rd` register from the `writeback` stage to the `execute`
  -- stage.
  , ctrlExBranch :: Maybe Address
  -- ^ The result of a branch computation. Set in the `execute` stage and
  -- contains the new PC.
  , ctrlMeBranch :: Bool
  -- ^ `True` when the instruction in the `memory` stage is a branch.
  , ctrlExLoad :: Bool
  -- ^ `True` when the instruction in the `execute` stage is a load or call.
  , ctrlWbMemInstr :: Bool
  -- ^ `True` when the instruction in the `writeback` stage is a load or call.
  }

-- \| Need propagate whether a branch instruction is in the `memory` stage
-- so we can stall the decode stall another cycle.

deriving instance (Show (f Word)) => Show (Control f)

deriving instance (Eq (f Word)) => Eq (Control f)

deriving instance (Generic (f Word)) => Generic (Control f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (Control f)

type CPUM f = RWS (Input f) (Output f) (State f)

-- | Run the CPU for one step.
circuit :: (Access f) => State f -> Input f -> (State f, Output f)
circuit = flip $ execRWS pipe

{- | The CPU, composed of each stage. Note that in Haskell-land, this pipeline
is sequential. That is, it works like so:

   writeback──state──> memory ──state──> execute ──state──> decode ──state──> fetch

The stages appear in reverse-order because they only send data forward
through the pipeline registers. This means---when simulated in pure
Haskell---that each stage only modifies the state of stages that have already
executed because the stages only have forward dependencies. This enables each
stage to update the state without affecting the execution of stages that have
yet to execute during the current tick.

That is, any writes to the `State` state appear as if they're semantically atomic,
even if they operationally aren't in the pure simulation.

The control lines are an exception here because they send data backwards
through the pipeline and *will* affect the current tick. Another exception is
the fields of `Output`, which are all packaged inside of a `First` type.
This means that the *first* value written to the output is the actual output
at the end of a cycle. Since the stages are written in reverse-order, this
correspond with the last write of the in-order pipeline having precedence.
E.g., outputs from the `writeback` stage take precedence over the other stages.

When synthesized by Clash, the registers will be allocated to the state and
they will be hooked up to each stage like so:

   state
    ├── writeback
    ├── memory
    ├── execute
    ├── decode
    └── fetch

This configuration may make it seem like the precise order of the stages is
immaterial, but recall that they definitely do matter at least for `Output`
due to the use of `First`.

Interstage reads/writes from the pipeline state create new dependencies
between stages. For example, writing to `stateWbRes` in `memory` and then reading
from `stateWbRes` in the `writeback` stage results in:

   state
    ├── writeback
    |    /|\
    |     |
    |    stateWbRes
    |     |
    ├── memory
    ├── execute
    ├── decode
    └── etch

i.e., `stateWbRes` becomes part of the pipeline registers between the `memory` and
`writeback` stages. These dependencies implicitly define the ordering of the
pipeline via data dependencies.
-}

-- | The CPU.
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
    { inputIsInstr = False
    , inputMem = pure 0
    , inputRs1 = pure 0
    , inputRs2 = pure 0
    }

init :: (Access f) => State f
init =
  State
    { stateFePc = initPc
    , stateDePc = 0
    , stateExPc = 0
    , stateExInstr = nop
    , stateMemInstr = nop
    , stateMemRes = pure 0
    , stateMemVal = pure 0
    , stateWbInstr = nop
    , stateWbRes = pure 0
    , stateCtrl = initCtrl
    , stateHalt = Running
    }

-- | Initial control lines.
initCtrl :: (Access f) => Control f
initCtrl =
  Control
    { ctrlFirstCycle = True
    , ctrlDecodeLoad = False
    , ctrlMeOutputActive = False
    , ctrlMeRegFwd = Nothing
    , ctrlWbRegFwd = Nothing
    , ctrlExBranch = Nothing
    , ctrlMeBranch = False
    , ctrlExLoad = False
    , ctrlWbMemInstr = False
    }

-- | The control lines need to be reset every tick.
withCtrlReset :: (Access f) => CPUM f () -> CPUM f (Control f)
withCtrlReset m = do
  firstCycle <- gets $ ctrlFirstCycle . stateCtrl
  modify $ \s -> s{stateCtrl = initCtrl{ctrlFirstCycle = firstCycle}}
  m
  ctrl <- gets stateCtrl
  modify $ \s -> s{stateCtrl = (stateCtrl s){ctrlFirstCycle = False}}
  pure ctrl

-- | Stop the CPU.
halt :: (Access f) => CPUM f ()
halt =
  modify $ \s -> s{stateHalt = EBreak}

-- | Set security violation flag.
setSecurityViolation :: (Access f) => CPUM f ()
setSecurityViolation =
  modify $ \s -> s{stateHalt = SecurityViolation}

-- | The fetch stage.
fetch :: (Access f) => CPUM f ()
fetch = do
  ctrl <- gets stateCtrl
  pc <- gets stateFePc
  mBranchAddr <- gets $ ctrlExBranch . stateCtrl

  let stall =
        -- Have to always stall incrementing the program counter on any load
        -- instruction because we cannot tell early enough if there's actually a
        -- load hazard since that occurs in the `decode` stage.
        ctrlDecodeLoad ctrl
          ||
          -- We stall on `ctrlMeOutputActive` because that means next cycle
          -- the memory will be unavailable to read an instruction from, so we
          -- shouldn't increment the program counter.
          ctrlMeOutputActive ctrl
          || isJust mBranchAddr

  if stall
    then modify $ \s -> s{stateFePc = fromMaybe pc mBranchAddr}
    else do
      -- Fetch the next instruction from memory.  Will only actually happen if no
      -- other reads/writes occur in subsequent stages.
      readPC pc
      modify $ \s ->
        s
          { -- Increment program counter for next fetch.
            stateFePc = fromMaybe (pc + 4) mBranchAddr
          , -- Propagate program counter to next stage.
            stateDePc = pc
          }

-- | Decode stage.
decode :: (Access f) => CPUM f ()
decode = do
  input <- ask
  ir <-
    if (inputIsInstr input)
      then noSecrets (inputMem input) nop (pure . Instruction.decode')
      else pure Instruction.nop

  when ((isLoad ir) || (isCall ir)) $
    setLines $
      \c -> c{ctrlDecodeLoad = True}

  ctrl <- gets stateCtrl

  let stall =
        -- First cycle = gibberish from memory, so we stall.
        ctrlFirstCycle ctrl
          -- This means that the branch was taken, so we have to stall and
          -- wait until the next cycle to get the correct instruction.
          || isJust (ctrlExBranch ctrl)
          -- This means that fetch stalled in the previous cycle, so we need to
          -- stall in this cycle since the instruction is garbage.
          || ctrlMeBranch ctrl
          -- Instruction in execute is a load or call; we stall the `fetch`
          -- stage when `decode` has a load or call (`ctrlDecodeLoad`), so
          -- `decode` has to stall one cycle later.
          || ctrlExLoad ctrl
          -- `fetch` stalls on `ctrlMeOutputActive` so we have to stall on `ctrlWbMemInstr`
          || ctrlWbMemInstr ctrl

  modify $ \s ->
    if stall
      then
        s
          { stateExInstr = nop
          , stateExPc = stateDePc s
          }
      else
        s
          { stateExInstr = ir
          , stateExPc = stateDePc s
          }

  unless stall $
    readRF ir
 where
  readRF ir =
    tell $
      mempty
        { outRs1 = pure $ fromMaybe 0 $ getRs1 ir
        , outRs2 = pure $ fromMaybe 0 $ getRs2 ir
        }

-- | Execute stage.
execute :: forall f. (Access f) => CPUM f ()
execute = do
  ir <- gets stateExInstr
  modify $ \s -> s{stateMemInstr = ir}

  -- TODO: Probably integrate this into the pattern match below
  when ((isLoad ir) || (isCall ir)) $
    setLines $
      \c -> c {ctrlExLoad = True}

  -- Fetch alu operands
  aluInputs <- runMaybeT $ fetchALUOperands ir

  modify $ \s ->
    let aluNOP = (ADD, pure 0, pure 0)
        (op, lhs, rhs) = fromMaybe aluNOP aluInputs
        res = alu op lhs rhs
     in s{stateMemRes = res}
 where
  fetchALUOperands :: Instruction -> MaybeT (CPUM f) (Arith, f Word, f Word)
  fetchALUOperands ir =
    case ir of
      Instruction.IType Env{} _ _ _ -> empty
      Instruction.RType op _ _ _ -> do
        r1 <- rs1
        r2 <- rs2
        pure (op, r1, r2)
      Instruction.IType Jump _ _ imm -> do
        pc <- gets $ pure . pack . stateExPc
        r1 <- rs1
        lift $ noSecrets r1 () $ \_ -> do
          let branchAddr = unpack <$> alu ADD r1 (pure $ signExtend imm)
          setLines $
            \c -> c{ctrlExBranch = fromPublic branchAddr}
        pure
          (ADD, pc, pure 4)
      Instruction.IType op _ _ imm -> do
        -- Do addition for non arithmetic operations.
        let op' = case op of
              Arith arith -> arith
              _ -> ADD
        r1 <- rs1
        let imm' = signExtend imm
        pure (op', r1, pure imm')
      Instruction.SType _ imm _ _ -> do
        r1 <- rs1
        r2 <- rs2
        let imm' = signExtend imm
        modify $ \s -> s{stateMemVal = r2}
        pure (ADD, r1, pure imm')
      Instruction.BType cmp imm _ _ -> do
        r1 <- rs1
        r2 <- rs2
        pc <- gets $ pack . stateExPc
        let doBranch = branch cmp r1 r2
            branchAddr :: f Address
            branchAddr = unpack <$> alu ADD (pure pc) (pure $ signExtend imm)
        lift $ noSecrets doBranch () $ \doBranch' ->
          if doBranch'
            then setLines $
              \c -> c{ctrlExBranch = fromPublic branchAddr}
            else modify $ \s -> s{stateMemInstr = nop}
        empty
      Instruction.UType base _ imm -> do
        base' <- case base of
          Zero -> pure 0
          PC -> gets $ pack . stateExPc
        let imm' = imm ++# (0 :: BitVector 12)
        pure (ADD, pure base', pure imm')
      Instruction.JType _ imm -> do
        pc <- gets $ pack . stateExPc
        let branchAddr :: f Address
            branchAddr = unpack <$> alu ADD (pure pc) (pure $ signExtend imm)
        setLines $
          \c -> c{ctrlExBranch = fromPublic branchAddr}
        pure (ADD, pure pc, pure 4)

  isIType :: Instruction -> Bool
  isIType (Instruction.IType{}) = True
  isIType _ = False

  rs1 :: MaybeT (CPUM f) (f Word)
  rs1 = lift $ regWithFwd getRs1 =<< asks inputRs1

  rs2 :: MaybeT (CPUM f) (f Word)
  rs2 = lift $ regWithFwd getRs2 =<< asks inputRs2

  regWithFwd :: (Instruction -> Maybe RegIdx) -> f Word -> CPUM f (f Word)
  regWithFwd getR def = do
    ir <- gets stateExInstr
    let checkForFwd line = do
          (fwdIdx, fwdVal) <- MaybeT $ gets $ line . stateCtrl
          guard (hazardRW getR ir fwdIdx)
          pure fwdVal
    fmap
      (fromMaybe def)
      $ runMaybeT
      $ checkForFwd ctrlMeRegFwd <|> checkForFwd ctrlWbRegFwd

  hazardRW :: (Instruction -> Maybe RegIdx) -> Instruction -> RegIdx -> Bool
  hazardRW getR src rd = isJust $ do
    rs <- getR src
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
  res <- gets stateMemRes
  instr <- gets stateMemInstr

  -- Store Forwarding
  try $ do
    rd <- getRd instr
    me_res <- lift $ gets stateMemRes
    lift $ setLines $ \c -> c{ctrlMeRegFwd = pure (rd, me_res)}

  case instr of
    Instruction.SType size _ _ _ ->
      noSecrets res () $ \res' -> do
        r2 <- gets stateMemVal
        writeRAM (unpack res') size r2
        setLines $ \c ->
          c{ctrlMeOutputActive = True}
    Instruction.IType (Load size _) _ _ _ ->
      noSecrets res () $ \res' -> do
        setLines $ \c ->
          c
            { -- Don't forward loads that haven't happened yet
              ctrlMeRegFwd = Nothing
            , ctrlMeOutputActive = True
            }
        readRAM (unpack res') size
    Instruction.IType (Env Call) _ _ _ -> do
      setLines $ \c ->
        c
          { -- Don't forward syscalls that haven't happened yet
            ctrlMeRegFwd = Nothing
          , ctrlMeOutputActive = True
          }
      readSyscall
    Instruction.IType Jump _ _ _ ->
      setLines $ \c ->
        c {ctrlMeBranch = True}
    Instruction.BType _ _ _ _ ->
      setLines $ \c ->
        c {ctrlMeBranch = True}
    Instruction.JType _ _ ->
      setLines $ \c ->
        c {ctrlMeBranch = True}
    _ -> pure ()

  modify $ \s ->
    s
      { stateWbInstr = stateMemInstr s
      , stateWbRes = stateMemRes s
      }

-- | Commit computations to the register file.
writeback :: (Access f) => CPUM f ()
writeback = do
  input <- asks inputMem
  ir <- gets stateWbInstr
  res <- gets stateWbRes

  haltState <- gets stateHalt

  when (haltState /= Running) $ do
    tell $
      mempty{outHalt = pure True}
    readRAM 0 Types.Word

  when (isBreak ir) $ do
    -- Flush the pipeline
    modify $ \s ->
      s
        { stateMemInstr = nop
        , stateExInstr = nop
        }
    readRAM 0 Types.Word
    halt

  try $ do
    rd <- getRd ir
    lift $ setLines $ \c ->
      c{ctrlWbRegFwd = pure (rd, res)}

  case ir of
    Instruction.RType _ rd _ _ -> writeRF rd res
    Instruction.IType (Arith _) rd _ _ -> writeRF rd res
    Instruction.UType _ rd _ -> writeRF rd res
    Instruction.IType (Load size sign) rd _ _ -> do
      let val = loadExtend size sign <$> input
      setLines $ \c ->
        c
          { ctrlWbRegFwd = pure (rd, val)
          , ctrlWbMemInstr = True
          }
      writeRF rd val
    Instruction.SType _ _ _ _ ->
      setLines $ \c ->
        c
          { ctrlWbMemInstr = True
          }
    Instruction.IType Jump rd _ _ ->
      writeRF rd res
    Instruction.JType rd _ ->
      writeRF rd res
    Instruction.IType (Env Call) _ _ _ -> do
      let val = input
      let rd = 10 -- a0
      setLines $ \c ->
        c
          { ctrlWbRegFwd = pure (rd, val)
          , ctrlWbMemInstr = True
          }
    _ -> pure ()
 where
  writeRF idx val =
    tell $ mempty{outRd = pure (idx, val)}

readPC :: (Access f, MonadWriter (Output f) m) => Address -> m ()
readPC addr =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsInstr = True
              , memAddress = addr
              , memSize = Types.Word
              , memVal = Nothing
              }
      }

readRAM :: (Access f, MonadWriter (Output f) m) => Address -> Size -> m ()
readRAM addr size =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsInstr = False
              , memAddress = addr
              , memSize = size
              , memVal = Nothing
              }
      }

writeRAM :: (Access f, MonadWriter (Output f) m) => Address -> Size -> f Word -> m ()
writeRAM addr size val =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsInstr = False
              , memAddress = addr
              , memSize = size
              , memVal = Just val
              }
      }

readSyscall :: (Access f, MonadWriter (Output f) m) => m ()
readSyscall =
  tell $
    mempty{outSyscall = pure True}

setLines :: (Access f, MonadState (State f) m) => (Control f -> Control f) -> m ()
setLines f = modify $ \s -> s{stateCtrl = f $ stateCtrl s}

{- | No secrets here, buddy: unwrap a word. If it's public, we gucci. If it's
private, die.
-}
noSecrets :: (Access f) => f a -> b -> (a -> CPUM f b) -> CPUM f b
noSecrets w a m = case fromPublic w of
  Just v -> m v
  Nothing -> setSecurityViolation >> pure a
