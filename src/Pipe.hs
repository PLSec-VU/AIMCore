{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module Pipe
  ( init,
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
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Instruction hiding (decode)
import qualified Instruction
import Regfile
import Types
import Prelude hiding (Ordering (..), Word, init, not, undefined, (&&), (||))

-- Fix
init = undefined

-- | The input to the CPU.
data Input = Input
  { inputMem :: Word,
    inputRs1 :: Word,
    inputRs2 :: Word
  }
  deriving (Show, Generic, NFDataX)

-- | A memory access
data MemAccess = MemAccess
  { memAddress :: Address,
    memVal :: Maybe Word
  }
  deriving (Show, Generic, NFDataX)

-- | The output of the CPU.
data Output = Output
  { -- Choice of `Last` vs. `First` (from `Data.Monoid`)
    -- will affect the correct order of stages in `pipeM`.
    outMem :: Last MemAccess,
    outRs1 :: Last RegIdx,
    outRs2 :: Last RegIdx,
    outRd :: Last (RegIdx, Word)
  }
  deriving (Show, Generic, NFDataX)

instance Semigroup Output where
  Output mem rs1 rs2 rd <> Output mem' rs1' rs2' rd' =
    Output (mem <> mem') (rs1 <> rs1') (rs2 <> rs2') (rd <> rd')

instance Monoid Output where
  mempty = Output mempty mempty mempty mempty

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
    -- | Memory value to write
    meVal :: Word,
    -- | Instruction register writeback stage
    wbIr :: Instruction,
    -- | ALU result register writeback stage
    wbRe :: Word
  }
  deriving (Show, Generic, NFDataX)

-- | The CPU monad.
type CPUM = RWS Input Output Pipe

-- | Run the CPU for one step.
pipe :: Pipe -> Input -> (Pipe, Output)
pipe = flip $ execRWS pipeM

-- | The CPU, composed of each stage. Note that in Haskell-land, this pipeline
-- is sequential. That is, it works like so:
--
--    fetch ──state──> decode ──state──> execute ──state──> memory ──state──> writeback
--
-- When synthesized by Clash, the registers will be allocated to the state and
-- they will be hooked up to each stage like so:
--
--    state
--     ├── fetch
--     ├── decode
--     ├── execute
--     ├── memory
--     └── writeback
--
-- This configuration may make it seem like the precise order of the stages is
-- immaterial.  However, because the fields of `Output` use `Last`, this means
-- that the *last* value written to the output is the actual output at the end
-- of a cycle. So, since fetch comes first (and it always results in an output),
-- any output from later stages (i.e., memory) will be the actual output (and to
-- compensate the fetch still will simply not increment the pc).
--
-- Interstage reads/writes from the pipeline state create new dependencies between
-- stages. For example, writing to `dePc` in `decode` and then reading from `dePc`
-- results in:
--
--    state
--     ├── fetch
--     |     |
--     |    dePc
--     |     |
--     |     v
--     ├── decode
--     ├── execute
--     ├── memory
--     └── writeback
--
-- i.e., `dePc` becomes part of the pipeline registers between the fetch and decode stages.
-- This means that each register in the `Pipe` state should be written to by exactly
-- one stage and be read by exactly one stage---namely, the subsequent one.
pipeM :: CPUM ()
pipeM = do
  fetch
  decode
  execute
  memory
  writeback

-- | The fetch stage.
fetch :: CPUM ()
fetch = do
  -- Get program counter.
  pc <- gets fePc

  try $ do
    let noHazard getIr = do
          ir <- gets getIr
          guard . not $ hazardPC ir

    -- Ensure there is no branch in the pipeline.
    noHazard exIr
    noHazard meIr
    noHazard wbIr

    modify $ \s ->
      s
        { -- Propagate program counter to next stage.
          fePc = pc + 1,
          -- Increment program counter for next fetch.
          dePc = pc
        }

  -- Fetch fromm memory.
  readRAM pc
  where
    -- Returns `True` if the given instruction may modify the program counter.
    hazardPC :: Instruction -> Bool
    hazardPC = \case
      Instruction.JType {} -> True
      Instruction.IType Jump _ _ _ -> True
      Instruction.BType {} -> True
      _ -> False

-- | Decode stage.
decode :: CPUM ()
decode = do
  ir <- Instruction.decode <$> asks inputMem
  rs1 <- asks inputRs1
  rs2 <- asks inputRs2
  modify $ \s ->
    s
      { exIr = ir,
        exPc = dePc s,
        exRs1 = rs1,
        exRs2 = rs2
      }

-- | Execute stage.
execute :: CPUM ()
execute = do
  -- Fetch alu operands
  val <- runMaybeT $ do
    ir <- gets exIr

    -- Make sure there is no hazard.
    ir' <- gets meIr
    guard $ hazardRW ir ir'

    case ir of
      -- Unknown instruction
      Invalid -> empty
      Instruction.RType op _ rs1 rs2 -> do
        r1 <- lift $ gets exRs1
        r2 <- lift $ gets exRs2
        pure (op, r1, r2)
      Instruction.IType op _ rs1 imm -> do
        -- Do addition for non arithmetic operations.
        let op' = case op of
              Arith arith -> arith
              _ -> ADD

        r1 <- lift $ gets exRs1
        let imm' = signExtend imm
        pure (op', r1, imm')
      Instruction.SType _ imm rs1 _ -> do
        r1 <- lift $ gets exRs1
        r2 <- lift $ gets exRs2
        let imm' = signExtend imm
        modify $ \s -> s {meVal = r2}
        pure (ADD, r1, imm')
      Instruction.BType cmp imm rs1 rs2 -> do
        r1 <- lift $ gets exRs1
        r2 <- lift $ gets exRs2
        let imm' = if branch cmp r1 r2 then signExtend imm else 4
        pc <- gets $ pack . exPc
        pure (ADD, pc, imm')
      Instruction.UType base _ imm -> do
        base' <- case base of
          Zero -> pure 0
          PC -> gets $ pack . exPc
        let imm' = imm ++# 0
        pure (ADD, base', imm')
      Instruction.JType _ imm -> do
        pc <- gets $ pack . exPc
        let imm' = signExtend imm
        pure (ADD, pc, imm')

  ((op, lhs, rhs), ir) <- case val of
    Just (op, lhs, rhs) -> do
      ir <- gets exIr
      pure ((op, lhs, rhs), ir)
    _ -> pure ((ADD, 0, 0), nop)

  let result = alu op lhs rhs

  --  -- Return whether we used the input for a load.
  --  case ir of
  --    Instruction.IType Load {} _ _ _ -> pure True
  --    _ -> pure False

  modify $ \s ->
    s
      { meRe = result,
        meIr = ir
      }
  where
    -- Read-Write hazard check.
    --
    -- Returns true if there is a read-write hazard between the source registers
    -- of the first instruction and the destination of the second instruction.
    hazardRW :: Instruction -> Instruction -> Bool
    hazardRW src dst = isJust $ do
      rd <- getRd dst
      unless (rd == 0) $
        let chk getRs = do
              rs <- getRs src
              guard $ rs == rd
         in chk getRs1 <|> chk getRs2

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

-- | Memory stage; interact with memory for load or store operations.
memory :: CPUM ()
memory = do
  result <- gets meRe

  instr <- gets meIr

  case instr of
    Instruction.SType size _ _ rs2 -> do
      r2 <- gets meVal
      writeRAM (unpack result) (unpack r2)
    Instruction.IType Load {} _ _ _ ->
      readRAM $ unpack result
    _ -> pure ()

  -- Propagate instruction register to the next stage
  modify $ \s ->
    s
      { wbIr = meIr s,
        wbRe = meRe s
      }

-- | Commit computations to the register file.
--
-- Returns whether we used the memory access for the writeback. This useful to
-- determine whether this word should be decoded as an instruction.
writeback :: CPUM ()
writeback = do
  input <- asks inputMem
  ir <- gets wbIr
  result <- gets wbRe

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
      writeRF rd $ loadExtend (size, sign)
    Instruction.BType {} -> do
      modify $ \s -> s {fePc = unpack result}
    Instruction.JType rd _ -> do
      modify $ \s -> s {fePc = unpack result}
      pc <- gets fePc
      writeRF rd $ pack pc
    Instruction.IType Jump rd _ _ -> do
      modify $ \s -> s {fePc = unpack result}
      pc <- gets fePc
      writeRF rd $ pack pc
    _ -> pure ()

writeRF :: (MonadWriter Output m) => RegIdx -> Word -> m ()
writeRF idx val =
  tell $ mempty {outRd = pure (idx, val)}

try :: (Monad m) => MaybeT m () -> m ()
try m = runMaybeT m >>= maybe (pure ()) pure

readRAM :: (MonadWriter Output m) => Address -> m ()
readRAM addr =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memAddress = addr,
                memVal = Nothing
              }
      }

writeRAM :: (MonadWriter Output m) => Address -> Word -> m ()
writeRAM addr val =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memAddress = addr,
                memVal = Just val
              }
      }
