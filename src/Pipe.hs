{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module Pipe
  ( initInput,
    initPipe,
    initCtrl,
    resetCtrl,
    pipe,
    Input (..),
    Output (..),
    Pipe (..),
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
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Instruction hiding (decode, halt)
import qualified Instruction
import Types
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))

-- | The input to the CPU.
data Input = Input
  { -- | Reads from memory.
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
    memIsPc :: Bool,
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
data Pipe = Pipe
  { -- | Program counter fetch stage
    fePc :: Address,
    -- | Program counter decode stage
    dePc :: Address,
    -- | Program counter execute stage
    exPc :: Address,
    -- | Instruction register execute stage
    exIr :: Instruction,
    -- | Regster 1
    exRs1 :: Word,
    -- | Register 2
    exRs2 :: Word,
    -- | Instruction register memory stage
    meIr :: Instruction,
    -- | ALU result register memory stage
    meRe :: Word,
    -- | Memory value to write for stores (`meRe` only contains the address).
    meVal :: Word,
    -- | Did we branch in the `execute` stage on this instruction?
    meBranch :: Bool,
    -- | Instruction register writeback stage
    wbIr :: Instruction,
    -- | ALU result register writeback stage
    wbRe :: Word,
    -- | Control/forwarding lines.
    pipeCtrl :: Control,
    -- | Are we done?
    pipeHalt :: Bool
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
type CPUM = RWS Input Output Pipe

-- | Run the CPU for one step.
pipe :: Pipe -> Input -> (Pipe, Output)
pipe = flip $ execRWS pipeM

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
-- That is, any writes to the `Pipe` state appear as if they're semantically atomic,
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
-- between stages. For example, writing to `wbRe` in `memory` and then reading
-- from `wbRe` in the `writeback` stage results in:
--
--    state
--     ├── writeback
--     |    /|\
--     |     |
--     |    wbRe
--     |     |
--     ├── memory
--     ├── execute
--     ├── decode
--     └── etch
--
-- i.e., `wbRe` becomes part of the pipeline registers between the `memory` and
-- `writeback` stages. These dependencies implicitly define the ordering of the
-- pipeline via data dependencies.

-- | The CPU.
pipeM :: CPUM ()
pipeM = do
  writeback
  memory
  execute
  decode
  fetch
  resetCtrl

initInput :: Input
initInput =
  Input
    { inputMem = 0,
      inputRs1 = 0,
      inputRs2 = 0
    }

initPipe :: Pipe
initPipe =
  Pipe
    { fePc = 4 * 50,
      dePc = 0,
      exPc = 0,
      exIr = nop,
      exRs1 = 0,
      exRs2 = 0,
      meIr = nop,
      meRe = 0,
      meVal = 0,
      meBranch = False,
      wbIr = nop,
      wbRe = 0,
      pipeCtrl = initCtrl,
      pipeHalt = False
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
  modify $ \s -> s {pipeCtrl = initCtrl {ctrlFirstCycle = False}}

-- | Stop the CPU.
halt :: CPUM ()
halt =
  modify $ \s -> s {pipeHalt = True}

-- | The fetch stage.
fetch :: CPUM ()
fetch = do
  pc <- gets fePc
  -- Fetch the next instruction from memory.  Will only actually happen if no
  -- other reads/writes occur in subsequent stages.
  readPC pc

  stall <-
    checkLines
      [ -- Have to always stall incrementing the program counter on any load
        -- instruction because we cannot tell early enough if there's actually a load
        -- hazard since that occurs in the `decode` stage.
        ctrlDecodeLoad,
        -- We stall on `ctrlMemOutputActive` because that means next cycle the
        -- memory will be unavailable to read an instruction from, so we shouldn't
        -- increment the program counter.
        ctrlMemOutputActive
      ]

  mBranchAddr <- gets $ ctrlExBranch . pipeCtrl
  modify $ \s ->
    if stall
      then
        s
          { fePc = fromMaybe pc mBranchAddr
          }
      else
        s
          { -- Increment program counter for next fetch.
            fePc = fromMaybe (pc + 4) mBranchAddr,
            -- Propagate program counter to next stage.
            dePc = pc
          }

-- | Decode stage.
decode :: CPUM ()
decode = do
  ir <- Instruction.decode <$> asks inputMem
  ex_ir <- gets exIr
  readRf ir

  case ir of
    IType Load {} _ _ _ ->
      setLines $ \c -> c {ctrlDecodeLoad = True}
    _ -> pure ()

  stall <-
    (loadHazard ir ex_ir ||)
      <$> checkLines
        [ -- First cycle = gibberish from memory, so we stall.
          ctrlFirstCycle,
          -- This means that the branch was taken, so we have to stall and wait
          -- until the next cycle to get the correct instruction.
          isJust . ctrlExBranch,
          -- If the memory input is active (i.e., there's a load down the pipe),
          -- stall.
          ctrlMemInputActive,
          -- Is there a branch instruction in the memory stage for which we take
          -- the branch? Then the current instruction in the `decode` stage is
          -- stale and we have to stall.
          ctrlMemBranch
        ]

  modify $ \s ->
    if stall
      then s {exIr = nop}
      else
        s
          { exIr = ir,
            exPc = dePc s
          }
  where
    readRf ir =
      tell $
        mempty
          { outRs1 = pure $ fromMaybe 0 $ getRs1 ir,
            outRs2 = pure $ fromMaybe 0 $ getRs2 ir
          }

    loadHazard :: Instruction -> Instruction -> Bool
    loadHazard de_ir ex_ir@(IType Load {} _ _ _) = isJust $ do
      let mr1 = getRs1 de_ir
          mr2 = getRs2 de_ir
          mrd = getRd ex_ir
      (guard =<< (==) <$> mrd <*> mr1)
        <|> (guard =<< (==) <$> mrd <*> mr2)
    loadHazard _ _ = False

-- | Execute stage.
execute :: CPUM ()
execute = do
  ir <- gets exIr
  modify $ \s -> s {meBranch = False}

  -- Fetch alu operands
  aluInputs <- runMaybeT $
    case ir of
      EBREAK -> empty
      -- Unknown instruction
      Invalid -> empty
      Instruction.RType op _ _ _ -> do
        r1 <- rs1
        r2 <- rs2
        pure (op, r1, r2)
      Instruction.IType Jump _ _ imm -> do
        pc <- gets $ pack . exPc
        r1 <- rs1
        modify $ \s -> s {meBranch = True}
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
        modify $ \s -> s {meVal = r2}
        pure (ADD, r1, imm')
      Instruction.BType cmp imm _ _ -> do
        r1 <- rs1
        r2 <- rs2
        pc <- gets $ pack . exPc
        let doBranch = branch cmp r1 r2
        when doBranch $ do
          modify $ \s -> s {meBranch = True}
          setLines $
            \c -> c {ctrlExBranch = Just $ bitCoerce $ alu ADD pc (signExtend imm)}
        empty
      Instruction.UType base _ imm -> do
        base' <- case base of
          Zero -> pure 0
          PC -> gets $ pack . exPc
        let imm' = imm ++# 0 `shiftL` 12
        pure (ADD, base', imm')
      Instruction.JType _ imm -> do
        pc <- gets $ pack . exPc
        modify $ \s -> s {meBranch = True}
        setLines $
          \c -> c {ctrlExBranch = Just $ bitCoerce $ alu ADD pc (signExtend imm)}
        pure (ADD, pc, 4)

  modify $ \s ->
    let aluNOP = (ADD, 0, 0)
        (op, lhs, rhs) = fromMaybe aluNOP aluInputs
        result = alu op lhs rhs
     in s
          { meRe = result,
            meIr = ir
          }
  where
    rs1 :: MaybeT CPUM Word
    rs1 = lift $ regWithFwd getRs1 =<< asks inputRs1

    rs2 :: MaybeT CPUM Word
    rs2 = lift $ regWithFwd getRs2 =<< asks inputRs2

    regWithFwd :: (Instruction -> Maybe RegIdx) -> Word -> CPUM Word
    regWithFwd getR def = do
      ir <- gets exIr
      let checkForFwd line = do
            (fwdIdx, fwdVal) <- MaybeT $ gets $ line . pipeCtrl
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
  result <- gets meRe
  instr <- gets meIr

  -- Store Forwarding
  try $ do
    rd <- getRd instr
    res <- lift $ gets meRe
    lift $ setLines $ \c -> c {ctrlMeRegFwd = pure (rd, res)}

  case instr of
    Instruction.SType size _ _ _ -> do
      r2 <- gets meVal
      writeRAM (unpack result) size (unpack r2)
      setLines $ \c ->
        c {ctrlMemOutputActive = True}
    Instruction.IType Load {} _ _ _ -> do
      setLines $ \c ->
        c
          { -- Don't forward loads that haven't happened yet
            ctrlMeRegFwd = Nothing,
            ctrlMemOutputActive = True
          }
      readRAM $ unpack result
    Instruction.BType {} -> do
      branched <- gets meBranch
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
      { wbIr = meIr s,
        wbRe = meRe s
      }

-- | Commit computations to the register file.
writeback :: CPUM ()
writeback = do
  input <- asks inputMem
  ir <- gets wbIr
  result <- gets wbRe

  halted <- gets pipeHalt

  when halted $ do
    tell $
      mempty {outHalt = pure True}
    readRAM 0

  when (ir == Instruction.halt) $ do
    -- Flush the pipeline
    modify $ \s ->
      s
        { meIr = nop,
          exIr = nop
        }
    readRAM 0
    halt

  try $ do
    rd <- getRd ir
    lift $ setLines $ \c ->
      c {ctrlWbRegFwd = pure (rd, result)}

  -- We could do a lot better here. It's just annoying to deal with the type
  -- naturals.
  let loadExtend = \case
        (Byte, Signed) -> signExtend $ slice d7 d0 input
        (Byte, Unsigned) -> zeroExtend $ slice d7 d0 input
        (Half, Signed) -> signExtend $ slice d15 d0 input
        (Half, Unsigned) -> signExtend $ slice d15 d0 input
        (Word, _) -> signExtend $ slice d31 d0 input

  case ir of
    Instruction.RType _ rd _ _ -> writeRF rd result
    Instruction.IType (Arith _) rd _ _ -> writeRF rd result
    Instruction.UType _ rd _ -> writeRF rd result
    Instruction.IType (Load size sign) rd _ _ -> do
      let val = loadExtend (size, sign)
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
      writeRF rd result
    Instruction.JType rd _ ->
      writeRF rd result
    _ -> pure ()
  where
    writeRF idx val =
      tell $ mempty {outRd = pure (idx, val)}

try :: (Monad m) => MaybeT m () -> m ()
try m = runMaybeT m >>= maybe (pure ()) pure

readPC :: (MonadWriter Output m) => Address -> m ()
readPC addr =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsPc = True,
                memAddress = addr,
                memSize = Word,
                memVal = Nothing
              }
      }

readRAM :: (MonadWriter Output m) => Address -> m ()
readRAM addr =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsPc = False,
                memAddress = addr,
                memSize = Word,
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
              { memIsPc = False,
                memAddress = addr,
                memSize = size,
                memVal = Just val
              }
      }

checkLines :: (MonadState Pipe m) => [Control -> Bool] -> m Bool
checkLines ls = do
  ctrl <- gets pipeCtrl
  pure $ or [test ctrl | test <- ls]

setLines :: (MonadState Pipe m) => (Control -> Control) -> m ()
setLines f = modify $ \s -> s {pipeCtrl = f $ pipeCtrl s}
