{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Pipe
  ( pipe
  , init
  ) where

import Prelude hiding (Word, undefined, Ordering (..), init, (&&), (||), not)
import Clash.Prelude hiding (def, Word, Ordering (..), init)

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Maybe (fromMaybe, isJust)

import Types
import Regfile
import Instruction hiding (decode)
import qualified Instruction

data Pipe = Pipe
  { rf :: Regfile
  -- ^ Register file

  , fePc :: Address
  -- ^ Program counter fetch stage

  , dePc :: Address
  -- ^ Program counter decode stage

  , exPc :: Address
  -- ^ Program counter execute stage
  , exIr :: Instruction
  -- ^ Instruction register execute stage


  , meIr :: Instruction
  -- ^ Instruction register memory stage
  , meRe :: Word
  -- ^ ALU result register memory stage

  , wbIr :: Instruction
  -- ^ Instruction register writeback stage
  , wbRe :: Word
  -- ^ ALU result register writeback stage
  }
  deriving Show

-- try :: Monad m => m a -> MaybeT m a -> m a
-- try def may = runMaybeT may >>= maybe def pure

try :: Monad m => MaybeT m () -> m ()
try m = runMaybeT m >>= maybe (pure ()) pure

-- | Read-Write hazard check.
--
-- Returns true if there is a read-write hazard between the source registers
-- of the first instruction and the destination of the second instruction.
hazardRW :: Instruction -> Instruction -> Bool
hazardRW src dst = isJust $ do
  rd <- getRd dst
  guard $ rd /= 0
  let chk getRs = do
        rs <- getRs src
        guard $ rs == rd
  chk getRs1  <|> chk getRs2

-- | Branch hazard check.
--
-- Returns true if the given instruction may modify the program counter.
hazardPC :: Instruction -> Bool
hazardPC = \case
  Instruction.JType {} -> True
  Instruction.IType Jump _ _ _ -> True
  Instruction.BType {} -> True
  _ -> False

-- | Get the destination register of an instruction, if any.
getRd :: Alternative f => Instruction -> f RegIdx
getRd = \case
  Instruction.RType _ rd _ _ -> pure rd
  Instruction.IType _ rd _ _ -> pure rd
  Instruction.UType _ rd _ -> pure rd
  Instruction.JType rd _ -> pure rd
  _ -> empty

-- | Get the first source register of an instruction, if any.
getRs1 :: Alternative f => Instruction -> f RegIdx
getRs1 = \case
  Instruction.RType _ _ rs1 _ -> pure rs1
  Instruction.IType _ _ rs1 _ -> pure rs1
  Instruction.SType _ _ rs1 _ -> pure rs1
  Instruction.BType _ _ rs1 _ -> pure rs1
  _ -> empty

-- | Get the second source register of an instruction, if any.
getRs2 :: Alternative f => Instruction -> f RegIdx
getRs2 = \case
  Instruction.RType _ _ _ rs2 -> pure rs2
  Instruction.SType _ _ _ rs2 -> pure rs2
  Instruction.BType _ _ _ rs2 -> pure rs2
  _ -> empty


readRF :: MonadState Pipe m => RegIdx -> m Word
readRF idx = do
  rf' <- gets rf
  pure $ lookupRF idx rf'

writeRF :: MonadState Pipe m => RegIdx -> Word -> m ()
writeRF idx val = do
  rf' <- gets rf
  modify $ \s -> s { rf = modifyRF idx val rf' }

type In = Word

data Out = Out
  { outAddress :: Address
  , outWrite :: Maybe (Size, Word)
  }
  deriving Show

init :: Pipe
init = Pipe
  { rf = initRF
  , fePc = 0
  , dePc = 0
  , exPc = 0
  , exIr = nop
  , meIr = nop
  , meRe = 0
  , wbIr = nop
  , wbRe = 0
  }

pipe :: Pipe -> In -> (Pipe, Out)
pipe = runCircuit pipeM

runCircuit :: (i -> State s o) -> s -> i -> (s, o)
runCircuit circuit s i = let (o, s') = runState (circuit i) s in (s', o)

pipeM :: MonadState Pipe m => In -> m Out
pipeM input = do
  rd <- writeback input
  me <- memory
  execute
  decode $ if rd then pure input else empty
  fe <- fetch
  pure $ fromMaybe fe me

-- | Commit computations to the register file.
--
-- Returns whether we used the memory access for the writeback. This useful to
-- determine whether this word should be decoded as an instruction.
writeback :: MonadState Pipe m => In -> m Bool
writeback input = do
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
      modify $ \s -> s { fePc = unpack result }

    Instruction.JType rd _ -> do
      modify $ \s -> s { fePc = unpack result }
      pc <- gets fePc
      writeRF rd $ pack pc

    Instruction.IType Jump rd _ _ -> do
      modify $ \s -> s { fePc = unpack result }
      pc <- gets fePc
      writeRF rd $ pack pc

    _ -> pure ()

  -- Return whether we used the input for a load.
  case ir of
    Instruction.IType Load {} _ _ _ -> pure True
    _ -> pure False

-- | Interact with memory for load or store operations.
memory :: MonadState Pipe m => m (Maybe Out)
memory = do
  result <- gets meRe

  out <- runMaybeT $ gets meIr >>= \case
    Instruction.SType size _ _ rs2 -> do
      r2 <- readRF rs2
      let out = Out
            { outAddress = unpack r2
            , outWrite = pure (size, unpack result)
            }
      pure out

    Instruction.IType Load {} _ _ _ -> do
      let out = Out
            { outAddress = unpack result
            , outWrite = empty
            }
      pure out

    _ -> empty

  -- Propagate instruction register to the next stage
  modify $ \s -> s { wbIr = meIr s, wbRe = meRe s }
  pure out

-- | Execute arithmetic operations.
execute :: MonadState Pipe m => m ()
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
        r1 <- readRF rs1
        r2 <- readRF rs2
        pure (op, r1, r2)

      Instruction.IType op _ rs1 imm -> do
        -- Do addition for non arithmetic operations.
        let op' = case op of
              Arith arith -> arith
              _ -> ADD

        r1 <- readRF rs1
        let imm' = signExtend imm
        pure (op', r1, imm')

      Instruction.SType _ imm rs1 _ -> do
        r1 <- readRF rs1
        let imm' = signExtend imm
        pure (ADD, r1, imm')

      Instruction.BType cmp imm rs1 rs2 -> do
        r1 <- readRF rs1
        r2 <- readRF rs2
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
  -- Compute alu operation and pass it to the next stage
  modify $ \s -> s { meRe = result }

  -- Propagate instruction register to the next stage
  modify $ \s -> s { meIr = ir }


-- | Decode the current instruction.
decode :: MonadState Pipe m => Maybe In -> m ()
decode input = do
  let ir = maybe nop Instruction.decode input
  modify $ \s -> s { exIr = ir }

  -- Propagate program counter to next stage.
  modify $ \s -> s { exPc = dePc s }

fetch :: MonadState Pipe m => m Out
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

    -- Propagate program counter to next stage.
    modify $ \s -> s { dePc = pc }

    -- Increment program counter for next fetch.
    modify $ \s -> s { fePc = pc + 4 }

  -- Fetch from memory
  pure $ Out
    { outAddress = pc
    , outWrite = empty
    }

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


