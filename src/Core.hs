module Core
  ( initInput,
    init,
    initCtrl,
    resetCtrl,
    circuit,
    Input (..),
    Output (..),
    State (..),
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

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Instruction hiding (decode)
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Input ->
  Signal System Output
topEntity = exposeClockResetEnable $ mealy circuit init

-- | The input to the CPU.
data Input = Input
  { -- | Is this an instruction read?
    inputIsInstr :: Bool,
    -- | Reads from memory.
    inputMem :: Word,
    -- | Read from the register file (corresponding to the index requested in
    -- the `Output`'s `outRs1` field).
    inputRs1 :: Word,
    -- | Read from the register file (corresponding to the index requested in
    -- the `Output`'s `outRs2` field).
    inputRs2 :: Word
  }
  deriving (Show, Generic, NFDataX)

-- | A memory access
data MemAccess = MemAccess
  { -- | Is this an instruction read?
    memIsInstr :: Bool,
    memAddress :: Address,
    memSize :: Size,
    -- | The word to be written, if there is one. If set to `Nothing`, then the
    -- `MemAccess` is a read. Otherwise, it's a write.
    memVal :: Maybe Word
  }
  deriving (Show, Generic, NFDataX)

-- | The output of the CPU.
data Output = Output
  { -- | A memory access.
    outMem :: First MemAccess,
    -- | A read request from the register file; stores it in the `inputRs1`
    -- field of `Input` on the next tick.
    outRs1 :: First RegIdx,
    -- | A read request from the register file; stores it in the `inputRs2`
    -- field of `Input` on the next tick.
    outRs2 :: First RegIdx,
    -- | A write to the register file.
    outRd :: First (RegIdx, Word),
    -- | Are we done?
    outHalt :: First Bool
  }
  deriving (Show, Generic, NFDataX)

instance Semigroup Output where
  Output mem rs1 rs2 rd hlt <> Output mem' rs1' rs2' rd' hlt' =
    Output (mem <> mem') (rs1 <> rs1') (rs2 <> rs2') (rd <> rd') (hlt <> hlt')

instance Monoid Output where
  mempty = Output mempty mempty mempty mempty mempty

-- | The internal state of the CPU; essentially the pipeline registers.
data State = State
  { -- | Program counter fetch stage
    stateFePc :: Address,
    -- | Program counter decode stage
    stateDePc :: Address,
    -- | Program counter execute stage
    stateExPc :: Address,
    -- | Instruction register execute stage
    stateExInstr :: Instruction,
    -- | Regster 1
    stateExRs1 :: Word,
    -- | Register 2
    stateExRs2 :: Word,
    -- | Instruction register memory stage
    stateMemInstr :: Instruction,
    -- | ALU result register memory stage
    stateMemRes :: Word,
    -- | Memory value to write for stores (`stateMemRes` only contains the address).
    stateMemVal :: Word,
    -- | Did we branch in the `execute` stage on this instruction?
    stateMemBranch :: Bool,
    -- | Instruction register writeback stage
    stateWbInstr :: Instruction,
    -- | ALU result register writeback stage
    stateWbRes :: Word,
    -- | Control/forwarding lines.
    stateCtrl :: Control,
    -- | Are we done?
    stateHalt :: Bool
  }
  deriving (Show, Generic, NFDataX)

-- | Control lines.
data Control = Control
  { -- | `True` during the first step of execution.
    ctrlFirstCycle :: Bool,
    -- | `True` when the instruction in the `decode` stage is a load.
    ctrlDecodeLoad :: Bool,
    -- | `True` when the instruction in the `execute` stage is a load.
    ctrlMemOutputActive :: Bool,
    -- | `True` when the memory input is a read from memory request.
    ctrlMemInputActive :: Bool,
    -- | Forwards the rd register from the `memory` stage to the `execute`
    -- stage.  The `RegIdx` payload is necessary to know what the destination
    -- register is for the instruction in the `memory` stage: it's too late to
    -- check this in the `execute` stage because it will already have been
    -- overwritten with the instruction for the next cycle.
    ctrlMeRegFwd :: Maybe (RegIdx, Word),
    -- | Forwards the `rd` register from the `writeback` stage to the `execute`
    -- stage.
    ctrlWbRegFwd :: Maybe (RegIdx, Word),
    -- | The result of a branch computation. Set in the `execute` stage and
    -- contains the new PC.
    ctrlExBranch :: Maybe Address,
    -- | The result of a branch computation. Set in the `execute` stage and
    -- contains the new PC.
    ctrlMemBranch :: Bool
  }
  -- \| Need propagate whether a branch instruction is in the `memory` stage
  -- so we can stall the decode stall another cycle.

  deriving (Show, Eq, Generic, NFDataX)

-- | The CPU monad.
type CPUM = RWS Input Output State

-- | Run the CPU for one step.
circuit :: State -> Input -> (State, Output)
circuit = flip $ execRWS pipe

-- | The CPU, composed of each stage. Note that in Haskell-land, this pipeline
-- is sequential. That is, it works like so:
--
--    writeback──state──> memory ──state──> execute ──state──> decode ──state──> fetch
--
-- The stages appear in reverse-order because they only send data forward
-- through the pipeline registers. This means---when simulated in pure
-- Haskell---that each stage only modifies the state of stages that have already
-- executed because the stages only have forward dependencies. This enables each
-- stage to update the state without affecting the execution of stages that have
-- yet to execute during the current tick.
--
-- That is, any writes to the `State` state appear as if they're semantically atomic,
-- even if they operationally aren't in the pure simulation.
--
-- The control lines are an exception here because they send data backwards
-- through the pipeline and *will* affect the current tick. Another exception is
-- the fields of `Output`, which are all packaged inside of a `First` type.
-- This means that the *first* value written to the output is the actual output
-- at the end of a cycle. Since the stages are written in reverse-order, this
-- correspond with the last write of the in-order pipeline having precedence.
-- E.g., outputs from the `writeback` stage take precedence over the other stages.
--
-- When synthesized by Clash, the registers will be allocated to the state and
-- they will be hooked up to each stage like so:
--
--    state
--     ├── writeback
--     ├── memory
--     ├── execute
--     ├── decode
--     └── fetch
--
-- This configuration may make it seem like the precise order of the stages is
-- immaterial, but recall that they definitely do matter at least for `Output`
-- due to the use of `First`.
--
-- Interstage reads/writes from the pipeline state create new dependencies
-- between stages. For example, writing to `stateWbRes` in `memory` and then reading
-- from `stateWbRes` in the `writeback` stage results in:
--
--    state
--     ├── writeback
--     |    /|\
--     |     |
--     |    stateWbRes
--     |     |
--     ├── memory
--     ├── execute
--     ├── decode
--     └── etch
--
-- i.e., `stateWbRes` becomes part of the pipeline registers between the `memory` and
-- `writeback` stages. These dependencies implicitly define the ordering of the
-- pipeline via data dependencies.

-- | The CPU.
pipe :: CPUM ()
pipe = do
  resetCtrl
  writeback
  memory
  execute
  decode
  fetch

initInput :: Input
initInput =
  Input
    { inputIsInstr = False,
      inputMem = 0,
      inputRs1 = 0,
      inputRs2 = 0
    }

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = nop,
      stateExRs1 = 0,
      stateExRs2 = 0,
      stateMemInstr = nop,
      stateMemRes = 0,
      stateMemVal = 0,
      stateMemBranch = False,
      stateWbInstr = nop,
      stateWbRes = 0,
      stateCtrl = initCtrl,
      stateHalt = False
    }

-- | Initial control lines.
initCtrl :: Control
initCtrl =
  Control
    { ctrlFirstCycle = True,
      ctrlDecodeLoad = False,
      ctrlMemOutputActive = False,
      ctrlMemInputActive = False,
      ctrlMeRegFwd = Nothing,
      ctrlWbRegFwd = Nothing,
      ctrlExBranch = Nothing,
      ctrlMemBranch = False
    }

-- | The control lines need to be reset every tick.
resetCtrl :: CPUM ()
resetCtrl =
  modify $ \s -> s {stateCtrl = initCtrl {ctrlFirstCycle = False}}

-- | Stop the CPU.
halt :: CPUM ()
halt =
  modify $ \s -> s {stateHalt = True}

-- | The fetch stage.
fetch :: CPUM ()
fetch = do
  ctrl <- gets stateCtrl
  let stall =
        ctrlDecodeLoad ctrl
          -- \^ Have to always stall incrementing the program counter on any load
          -- instruction because we cannot tell early enough if there's actually a
          -- load hazard since that occurs in the `decode` stage.
          || ctrlMemOutputActive ctrl
  -- \^ We stall on `ctrlMemOutputActive` because that means next cycle
  -- the memory will be unavailable to read an instruction from, so we
  -- shouldn't increment the program counter.

  pc <- gets stateFePc
  -- Fetch the next instruction from memory.  Will only actually happen if no
  -- other reads/writes occur in subsequent stages.
  readPC pc

  mBranchAddr <- gets $ ctrlExBranch . stateCtrl
  modify $ \s ->
    if stall
      then
        s
          { stateFePc = fromMaybe pc mBranchAddr
          }
      else
        s
          { -- Increment program counter for next fetch.
            stateFePc = fromMaybe (pc + 4) mBranchAddr,
            -- Propagate program counter to next stage.
            stateDePc = pc
          }

-- | Decode stage.
decode :: CPUM ()
decode = do
  input <- ask
  let ir
        | inputIsInstr input =
            Instruction.decode' $ inputMem input
        | otherwise = Instruction.nop
  ex_ir <- gets stateExInstr
  readRf ir

  when (isLoad ir) $
    setLines $
      \c -> c {ctrlDecodeLoad = True}

  ctrl <- gets stateCtrl

  let stall =
        loadHazard ir ex_ir
          || ctrlFirstCycle ctrl
          -- \^ First cycle = gibberish from memory, so we stall.
          || isJust (ctrlExBranch ctrl)
          -- \^ This means that the branch was taken, so we have to stall and
          -- wait until the next cycle to get the correct instruction.
          || ctrlMemInputActive ctrl
          -- \^ If the memory input is active (i.e., there's a load down the
          -- pipe), stall.
          || ctrlMemBranch ctrl
  -- \^ Is there a branch instruction in the memory stage for which
  -- we take the branch? Then the current instruction in the `decode`
  -- stage is stale and we have to stall.

  modify $ \s ->
    if stall
      then s {stateExInstr = nop}
      else
        s
          { stateExInstr = ir,
            stateExPc = stateDePc s
          }
  where
    readRf ir =
      tell $
        mempty
          { outRs1 = pure $ fromMaybe 0 $ getRs1 ir,
            outRs2 = pure $ fromMaybe 0 $ getRs2 ir
          }

-- | Execute stage.
execute :: CPUM ()
execute = do
  ir <- gets stateExInstr
  modify $ \s -> s {stateMemBranch = False}

  -- Fetch alu operands
  aluInputs <- runMaybeT $
    case ir of
      Instruction.RType op _ _ _ -> do
        r1 <- rs1
        r2 <- rs2
        pure (op, r1, r2)
      Instruction.IType Jump _ _ imm -> do
        pc <- gets $ pack . stateExPc
        r1 <- rs1
        setLines $
          \c -> c {ctrlExBranch = Just $ bitCoerce $ alu ADD r1 (signExtend imm)}
        pure (ADD, pc, 4)
      Instruction.IType op _ _ imm -> do
        -- Do addition for non arithmetic operations.
        let op' = case op of
              Arith arith -> arith
              _ -> ADD
        r1 <- rs1
        let imm' = signExtend imm
        pure (op', r1, imm')
      Instruction.SType _ imm _ _ -> do
        r1 <- rs1
        r2 <- rs2
        let imm' = signExtend imm
        modify $ \s -> s {stateMemVal = r2}
        pure (ADD, r1, imm')
      Instruction.BType cmp imm _ _ -> do
        r1 <- rs1
        r2 <- rs2
        pc <- gets $ pack . stateExPc
        let doBranch = branch cmp r1 r2
        when doBranch $ do
          modify $ \s -> s {stateMemBranch = True}
          setLines $
            \c -> c {ctrlExBranch = Just $ bitCoerce $ alu ADD pc (signExtend imm)}
        empty
      Instruction.UType base _ imm -> do
        base' <- case base of
          Zero -> pure 0
          PC -> gets $ pack . stateExPc
        let imm' = imm ++# 0 `shiftL` 12
        pure (ADD, base', imm')
      Instruction.JType _ imm -> do
        pc <- gets $ pack . stateExPc
        setLines $
          \c -> c {ctrlExBranch = Just $ bitCoerce $ alu ADD pc (signExtend imm)}
        pure (ADD, pc, 4)

  modify $ \s ->
    let aluNOP = (ADD, 0, 0)
        (op, lhs, rhs) = fromMaybe aluNOP aluInputs
        res = alu op lhs rhs
     in s
          { stateMemRes = res,
            stateMemInstr = ir
          }
  where
    rs1 :: MaybeT CPUM Word
    rs1 = lift $ regWithFwd getRs1 =<< asks inputRs1

    rs2 :: MaybeT CPUM Word
    rs2 = lift $ regWithFwd getRs2 =<< asks inputRs2

    regWithFwd :: (Instruction -> Maybe RegIdx) -> Word -> CPUM Word
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

alu :: Arith -> Word -> Word -> Word
alu op lhs rhs = case op of
  ADD -> lhs + rhs
  SUB -> lhs - rhs
  XOR -> lhs .^. rhs
  OR -> lhs .|. rhs
  AND -> lhs .&. rhs
  SLL -> lhs `shiftL` shiftBits rhs
  SRL -> lhs `shiftR` shiftBits rhs
  SRA -> pack $ sign lhs `shiftR` shiftBits rhs
  SLT -> set $ sign lhs > sign rhs
  SLTU -> set $ lhs > rhs
  where
    shiftBits s = fromIntegral $ slice d4 d0 s
    sign = unpack @(Signed 32)
    set b = if b then 1 else 0

branch :: Comparison -> Word -> Word -> Bool
branch op lhs rhs = case op of
  EQ -> lhs == rhs
  NE -> lhs /= rhs
  LT -> sign lhs < sign rhs
  GE -> sign lhs >= sign rhs
  LTU -> lhs < rhs
  GEU -> lhs >= rhs
  where
    sign = unpack @(Signed 32)

memory :: CPUM ()
memory = do
  res <- gets stateMemRes
  instr <- gets stateMemInstr

  -- Store Forwarding
  try $ do
    rd <- getRd instr
    me_res <- lift $ gets stateMemRes
    lift $ setLines $ \c -> c {ctrlMeRegFwd = pure (rd, me_res)}

  case instr of
    Instruction.SType size _ _ _ -> do
      r2 <- gets stateMemVal
      writeRAM (unpack res) size (unpack r2)
      setLines $ \c ->
        c {ctrlMemOutputActive = True}
    Instruction.IType (Load size _) _ _ _ -> do
      setLines $ \c ->
        c
          { -- Don't forward loads that haven't happened yet
            ctrlMeRegFwd = Nothing,
            ctrlMemOutputActive = True
          }
      readRAM (unpack res) size
    Instruction.BType {} -> do
      branched <- gets stateMemBranch
      when branched $
        setLines $ \c ->
          c {ctrlMemBranch = True}
    Instruction.IType Jump _ _ _ ->
      setLines $ \c ->
        c {ctrlMemBranch = True}
    Instruction.JType {} ->
      setLines $ \c ->
        c {ctrlMemBranch = True}
    _ -> pure ()

  modify $ \s ->
    s
      { stateWbInstr = stateMemInstr s,
        stateWbRes = stateMemRes s
      }

-- | Commit computations to the register file.
writeback :: CPUM ()
writeback = do
  input <- asks inputMem
  ir <- gets stateWbInstr
  res <- gets stateWbRes

  halted <- gets stateHalt

  when halted $ do
    tell $
      mempty {outHalt = pure True}
    readRAM 0 Word

  when (isBreak ir) $ do
    -- Flush the pipeline
    modify $ \s ->
      s
        { stateMemInstr = nop,
          stateExInstr = nop
        }
    readRAM 0 Word
    halt

  try $ do
    rd <- getRd ir
    lift $ setLines $ \c ->
      c {ctrlWbRegFwd = pure (rd, res)}

  case ir of
    Instruction.RType _ rd _ _ -> writeRF rd res
    Instruction.IType (Arith _) rd _ _ -> writeRF rd res
    Instruction.UType _ rd _ -> writeRF rd res
    Instruction.IType (Load size sign) rd _ _ -> do
      let val = loadExtend size sign input
      setLines $ \c ->
        c
          { ctrlWbRegFwd = pure (rd, val),
            ctrlMemInputActive = True
          }
      writeRF rd val
    Instruction.SType {} ->
      setLines $ \c ->
        c {ctrlMemInputActive = True}
    Instruction.IType Jump rd _ _ ->
      writeRF rd res
    Instruction.JType rd _ ->
      writeRF rd res
    _ -> pure ()
  where
    writeRF idx val =
      tell $ mempty {outRd = pure (idx, val)}

readPC :: (MonadWriter Output m) => Address -> m ()
readPC addr =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsInstr = True,
                memAddress = addr,
                memSize = Word,
                memVal = Nothing
              }
      }

readRAM :: (MonadWriter Output m) => Address -> Size -> m ()
readRAM addr size =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsInstr = False,
                memAddress = addr,
                memSize = size,
                memVal = Nothing
              }
      }

writeRAM :: (MonadWriter Output m) => Address -> Size -> Word -> m ()
writeRAM addr size val =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsInstr = False,
                memAddress = addr,
                memSize = size,
                memVal = Just val
              }
      }

-- checkLines :: (MonadState State m) => [Control -> Bool] -> m Bool
-- checkLines ls = do
--   ctrl <- gets stateCtrl
--   pure $ or [test ctrl | test <- ls]

setLines :: (MonadState State m) => (Control -> Control) -> m ()
setLines f = modify $ \s -> s {stateCtrl = f $ stateCtrl s}
