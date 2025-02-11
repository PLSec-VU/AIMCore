{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module Pipe
  ( init,
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
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Debug.Trace
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
    outMem :: First MemAccess,
    outRs1 :: First RegIdx,
    outRs2 :: First RegIdx,
    outRd :: First (RegIdx, Word)
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
    -- | Memory value to write for stores (`meRe` only contains the address).
    meVal :: Word,
    -- | Instruction register writeback stage
    wbIr :: Instruction,
    -- | ALU result register writeback stage
    wbRe :: Word,
    -- | Control lines
    pipeCtrl :: Control
  }
  deriving (Show, Generic, NFDataX)

-- | Control lines.
data Control = Control
  { -- | `True` during the first step of execution. Needed to prevent the
    -- `decode` stage from sending out garbage.
    ctrlFirstCycle :: Bool,
    -- | `True` when an instruction depends on a prior load instruction. Set in the `execute` stage
    -- and stalls the pipeline.
    ctrlExMem :: Bool,
    -- | `True` during a memory read/write, set in the `memory` stage.
    ctrlMemAccess :: Bool,
    -- | Forwards the `rd` register from the `memory` stage to the `execute` stage.
    -- The `RegIdx` payload is necessary to know what the destination register is for
    -- the instruction in the `memory` stage: it's too late to check this in the `execute` stage
    -- because it will already have been overwritten with the instruction for the next cycle.
    ctrlMeRegFwd :: Maybe (RegIdx, Word),
    -- | Forwards the `rd` register from the `writeback` stage to the `execute` stage.
    ctrlWbRegFwd :: Maybe (RegIdx, Word),
    -- | The result of a branch computatoin. Set in the `execute` stage and contains the new PC.
    ctrlExBranch :: Maybe Address
  }
  deriving (Show, Eq, Generic, NFDataX)

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
  writeback
  memory
  execute
  decode
  fetch
  resetCtrl

initCtrl :: Control
initCtrl =
  Control
    { ctrlFirstCycle = True,
      ctrlExMem = False,
      ctrlMemAccess = False,
      ctrlMeRegFwd = Nothing,
      ctrlWbRegFwd = Nothing,
      ctrlExBranch = Nothing
    }

resetCtrl :: CPUM ()
resetCtrl =
  modify $ \s -> s {pipeCtrl = initCtrl {ctrlFirstCycle = False}}

-- | The fetch stage.
fetch :: CPUM ()
fetch = do
  pc <- gets fePc
  stall <- checkLines [ctrlExMem, ctrlMemAccess]

  unless stall $ do
    branchAddr <- gets $ ctrlExBranch . pipeCtrl
    modify $ \s ->
      s
        { -- Increment program counter for next fetch.
          fePc = fromMaybe (pc + 1) branchAddr,
          -- Propagate program counter to next stage.
          dePc = pc
        }

  readRAM pc

-- | Decode stage.
decode :: CPUM ()
decode = do
  ir <- Instruction.decode <$> asks inputMem

  readRf ir

  stall <- gets $ isJust . ctrlExBranch . pipeCtrl
  stall2 <- gets $ ctrlExMem . pipeCtrl
  stall3 <- gets $ ctrlFirstCycle . pipeCtrl

  unless (stall || stall2 || stall3) $ do
    modify $ \s ->
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

-- | Execute stage.
execute :: CPUM ()
execute = do
  ir <- gets exIr
  me_ir <- gets meIr
  wb_ir <- gets wbIr

  one <- runMaybeT rs1
  two <- runMaybeT rs2

  traceM $
    unlines
      [ "ir",
        show ir,
        "me_ir",
        show me_ir,
        "wb_ir",
        show wb_ir,
        "one",
        show one,
        "two",
        show two
        -- "hazardRW True ir me_ir",
        -- show (hazardRW True ir me_ir),
        -- "hazardRW True ir wb_ir",
        -- show (hazardRW True ir wb_ir),
        -- "hazardRW False ir me_ir",
        -- show (hazardRW False ir me_ir),
        -- "hazardRW False ir wb_ir",
        -- show (hazardRW False ir wb_ir)
      ]

  -- Need to save the forwarded values on a stall! Maybe?
  let stall = hazardLoad ir me_ir
  if stall
    then do
      setLines $
        \c -> c {ctrlExMem = True}
      modify $ \s ->
        s
          { meRe = 0,
            meIr = nop
          }
    else do
      -- Fetch alu operands
      val <- runMaybeT $ do
        case ir of
          -- Unknown instruction
          Invalid -> empty
          Instruction.RType op _ _ _ -> do
            r1 <- rs1
            r2 <- rs2
            pure (op, r1, r2)
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
            when doBranch $
              setLines $
                \c -> c {ctrlExBranch = Just $ bitCoerce $ alu ADD pc (signExtend imm)}
            pure (ADD, 0, 0)
          Instruction.UType base _ imm -> do
            base' <- case base of
              Zero -> pure 0
              PC -> gets $ pack . exPc
            let imm' = imm ++# 0
            pure (ADD, base', imm')
          Instruction.JType _ imm -> do
            pc <- gets $ pack . exPc
            setLines $
              \c -> c {ctrlExBranch = Just $ bitCoerce $ alu ADD pc (signExtend imm)}
            pure (ADD, 0, 0)

      ((op, lhs, rhs), ir) <- case val of
        Just (op, lhs, rhs) -> do
          ir <- gets exIr
          pure ((op, lhs, rhs), ir)
        _ -> pure ((ADD, 0, 0), nop)

      let result = alu op lhs rhs

      modify $ \s ->
        s
          { meRe = result,
            meIr = ir
          }
  where
    rs1 :: MaybeT CPUM Word
    rs1 = lift $ regWithFwd True $ asks inputRs1

    rs2 :: MaybeT CPUM Word
    rs2 = lift $ regWithFwd False $ asks inputRs2

    regWithFwd :: Bool -> CPUM Word -> CPUM Word
    regWithFwd reg def = do
      ir <- gets exIr
      me_ir <- gets meIr
      wb_ir <- gets wbIr
      fmap
        fromJust
        $ runMaybeT
        $ do
          msum
            [ do
                (meFwdIdx, meFwdVal) <- MaybeT (gets $ ctrlMeRegFwd . pipeCtrl)
                guard (hazardRW reg ir meFwdIdx)
                pure meFwdVal,
              do
                (wbFwdIdx, wbFwdVal) <- MaybeT (gets $ ctrlWbRegFwd . pipeCtrl)
                guard (hazardRW reg ir wbFwdIdx)
                pure wbFwdVal,
              lift
                def
            ]

    hazardRW :: Bool -> Instruction -> RegIdx -> Bool
    hazardRW reg src rd = isJust $ do
      guard (rd /= 0)
      let chk getRs = do
            rs <- getRs src
            guard $ rs == rd
       in if reg
            then chk getRs1
            else chk getRs2

    hazardLoad :: Instruction -> Instruction -> Bool
    hazardLoad ex_ir mem_ir@(IType Load {} _ _ _) = isJust $ do
      rs1 <- getRs1 ex_ir
      rs2 <- getRs2 ex_ir
      rd <- getRd mem_ir
      guard $ (rd == rs1) || (rd == rs2)
    hazardLoad _ _ = False

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

  -- Forwarding
  try $ do
    rd <- getRd instr
    res <- lift $ gets meRe
    lift $ setLines $ \c -> c {ctrlMeRegFwd = pure (rd, res)}

  case instr of
    Instruction.SType size _ _ rs2 -> do
      setLines $ \c -> c {ctrlMemAccess = True}
      r2 <- gets meVal
      writeRAM (unpack result) (unpack r2)
    Instruction.IType Load {} _ _ _ -> do
      setLines $ \c -> c {ctrlMemAccess = True}
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

  runMaybeT $ do
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
        c {ctrlWbRegFwd = pure (rd, val)}
      writeRF rd val
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
  where
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

checkLines :: (MonadState Pipe m) => [Control -> Bool] -> m Bool
checkLines lines = do
  ctrl <- gets pipeCtrl
  pure $ or [test ctrl | test <- lines]

setLines :: (MonadState Pipe m) => (Control -> Control) -> m ()
setLines f = modify $ \s -> s {pipeCtrl = f $ pipeCtrl s}
