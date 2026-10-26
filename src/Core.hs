{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# LANGUAGE InstanceSigs #-}
{- HLINT ignore "Functor law" -}

module Core
  ( initInput,
    initAt,
    init,
    initCtrl,
    circuit,
    Input (..),
    Output (..),
    StateG (..),
    State,
    HaltState (..),
    fetch,
    decode,
    execute,
    memory,
    writeback,
    CPUM,
    MemAccess (..),
    Request,
    Control (..),
    alu,
    branch,
    sllWord,
    srlWord,
    sraWord,
    topEntity,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Data.Maybe (fromMaybe, isJust, isNothing)
import Core.Instruction hiding (decode)
import Memory.Types
import Core.RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Input ->
  Signal System Output
topEntity = exposeClockResetEnable $ mealy (circuit @RegFile) (init @RegFile)

-- | The input to the CPU: memory's answer to the previous request, or 'Nothing'
-- while it is not ready yet.
newtype Input = Input {inputMem :: Maybe Word}
  deriving (Show, Eq, Generic, NFDataX)

-- | A memory access.
data MemAccess
  = -- | An instruction fetch: always a whole word.
    MemFetch Address
  | -- | A data read of the given size.
    MemRead Address Size
  | -- | A data write of the given size, with the value to write.
    MemWrite Address Size Word
  deriving (Show, Generic, NFDataX)

-- | A request on the memory bus, if any, as the memory servers see it.
type Request = Maybe MemAccess

-- | The output of the CPU.
newtype Output = Output
  { -- | A memory access.
    outMem :: Request
  }
  deriving (Show, Generic, NFDataX)

instance Semigroup Output where
  (<>) :: Output -> Output -> Output
  -- When several stages access memory in one cycle, the first one wins.
  Output mem <> Output mem' =
    Output (mem <|> mem')

instance Monoid Output where
  mempty = Output Nothing

-- | CPU halt state
data HaltState = EBreak Address | Syscall Address
  deriving (Show, Eq, Generic)

instance NFDataX HaltState

-- | The internal state of the CPU; essentially the pipeline registers.
--
-- Parameterised over the register-file representation @r@ so that the same
-- pipeline can be run on the synthesisable 'RegFile' or, for symbolic
-- execution, on the function-backed 'RegFn'. See 'RegFileOps'.
data StateG r = State
  { -- | Program counter fetch stage.
    stateFePc :: Address,
    -- | Program counter decode stage.
    stateDePc :: Address,
    -- | Does the decode stage expect an instruction?
    stateDeExpInstr :: Bool,
    -- | Program counter execute stage.
    stateExPc :: Address,
    -- | Instruction execute stage.
    stateExInstr :: Instruction,
    -- | Instruction memory stage.
    stateMeInstr :: Instruction,
    -- | Computation result memory stage.
    stateMeRes :: Word,
    -- | Address for load and store, memory stage.
    stateMeAddr :: Address,
    -- | Instruction writeback stage.
    stateWbInstr :: Instruction,
    -- | Computation result writeback stage.
    stateWbRes :: Word,
    -- | Register file.
    stateRegFile :: r,
    -- | Control/forwarding lines.
    stateCtrl :: Control,
    -- | CPU halt state.
    stateHalt :: Maybe HaltState
  }

-- | The synthesisable state: the register file is a 'Vec'.
type State = StateG RegFile

deriving instance (Show r) => Show (StateG r)

deriving instance (Eq r) => Eq (StateG r)

deriving instance Generic (StateG r)

deriving anyclass instance (Generic r, NFDataX r) => NFDataX (StateG r)

-- | Control lines.
data Control = Control
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
    -- | Stores the write address and size if the instruction in the `execute` stage
    --   is a store.
    ctrlExStoreAddrSize :: Maybe (Address, Size),
    -- | `True` when the instruction in the `memory` stage is a store or a load.
    ctrlMeMemInstr :: Bool,
    -- | Stores the write address and size if the instruction in the `memory` stage
    --   is a store.
    ctrlMeStoreAddrSize :: Maybe (Address, Size),
    -- | Forwards the `rd` register from the `memory` stage to the `execute`
    -- stage.
    ctrlMeRegFwd :: Maybe (RegIdx, Word),
    -- | Forwards the `rd` register from the `writeback` stage to the `execute`
    -- stage.
    ctrlWbRegFwd :: Maybe (RegIdx, Word),
    -- | True when memory has not answered yet: the pipeline holds.
    ctrlMemNotReady :: Bool
  }
  deriving (Show, Eq, Generic, NFDataX)

type CPUM r = RWS Input Output (StateG r)

setLines :: (MonadState (StateG r) m) => (Control -> Control) -> m ()
setLines f = modify $ \s -> s {stateCtrl = f (stateCtrl s)}

-- | Run the CPU for one step.
circuit :: forall r. (RegFileOps r) => StateG r -> Input -> (StateG r, Output)
circuit = flip $ execRWS pipe

-- | The CPU, composed of each stage.
pipe :: (RegFileOps r) => CPUM r ()
pipe = do
  -- The control lines need to be reset every tick.
  modify $ \s -> s {stateCtrl = initCtrl}
  writeback
  memory
  execute
  decode
  fetch

initInput :: Input
initInput = Input (Just 0)

initAt :: Address -> r -> StateG r
initAt pc rf =
  State
    { stateFePc = pc,
      stateDePc = 0,
      stateDeExpInstr = False,
      stateExPc = 0,
      stateExInstr = Nop FirstCycle,
      stateMeInstr = Nop FirstCycle,
      stateMeRes = 0,
      stateMeAddr = 0,
      stateWbInstr = Nop FirstCycle,
      stateWbRes = 0,
      stateRegFile = rf,
      stateCtrl = initCtrl,
      stateHalt = Nothing
    }

init :: forall r. (RegFileOps r) => StateG r
init = initAt initPc initRFg

-- | Initial control lines.
initCtrl :: Control
initCtrl =
  Control
    { ctrlDeLoadHazard = Nothing,
      ctrlDeStoreHazard = Nothing,
      ctrlExInstr = Nothing,
      ctrlExJumpAddr = Nothing,
      ctrlExStoreAddrSize = Nothing,
      ctrlMeMemInstr = False,
      ctrlMeStoreAddrSize = Nothing,
      ctrlMeRegFwd = Nothing,
      ctrlWbRegFwd = Nothing,
      ctrlMemNotReady = False
    }

-- | Fetch stage. Holds while memory has not answered (see 'writeback'), like the
-- other stages before writeback: a stall then leaves every pipeline register
-- as it was.
fetch :: CPUM r ()
fetch = do
  notReady <- gets (ctrlMemNotReady . stateCtrl)
  unless notReady fetchFresh

-- | Fetch stage, in a cycle where the pipeline advances.
fetchFresh :: CPUM r ()
fetchFresh = do
  pc <- gets stateFePc
  ctrl <- gets stateCtrl

  -- We stall if the instruction in the `memory` stage is a load or a store.
  let stall = ctrlMeMemInstr ctrl

  -- Always try to read unless we stall.
  unless stall $
    readPC pc

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
        stateDePc = pc,
        -- Decode expects an instruction next cycle iff we fetch now.
        stateDeExpInstr = not stall
      }

-- | Decode stage. Holds while memory has not answered (see 'writeback').
decode :: CPUM r ()
decode = do
  notReady <- gets (ctrlMemNotReady . stateCtrl)
  unless notReady decodeFresh

-- | Decode stage, in a cycle where the pipeline advances.
decodeFresh :: CPUM r ()
decodeFresh = do
  input <- ask
  pc <- gets stateDePc
  expInstr <- gets stateDeExpInstr
  ctrl <- gets stateCtrl

  let ir =
        if expInstr
          then maybe (Nop NoInstruction) decode' (inputMem input)
          else Nop NoInstruction

  let halted = ctrlExInstr ctrl == Just (Nop Halted)
  let call_current_cycle = maybe False isCall (ctrlExInstr ctrl)
  let break_current_cycle = maybe False isBreak (ctrlExInstr ctrl)

  let jump_previous_cycle = ctrlExInstr ctrl == Just (Nop JumpFirstCycle)
  let store_hazard_previous_cycle = ctrlExInstr ctrl == Just (Nop StoreHazardFirstCycle)
  let load_hazard_previous_cycle = ctrlExInstr ctrl == Just (Nop LoadHazardFirstCycle)

  let jump_current_cycle = isJust (ctrlExJumpAddr ctrl)

  let store_hazard_current_cycle =
        maybe False (storeHazard pc) (ctrlExStoreAddrSize ctrl) ||
        maybe False (storeHazard pc) (ctrlMeStoreAddrSize ctrl)

  let load_hazard_current_cycle = maybe False (loadHazard ir) (ctrlExInstr ctrl)

  let ir'
        -- Halt if the core is not running anymore.
        | halted = Nop Halted
        -- Halt if there is a syscall in this cycle.
        | call_current_cycle = Nop Halted
        -- Halt if there is a break in this cycle.
        | break_current_cycle = Nop Halted
        -- Stall if there was a jump in the previous cycle.
        | jump_previous_cycle = Nop JumpSecondCycle
        -- Stall if there was a store hazard in the previous cycle.
        | store_hazard_previous_cycle = Nop StoreHazardSecondCycle
        -- Stall if there was a load hazard in the previous cycle.
        | load_hazard_previous_cycle = Nop LoadHazardSecondCycle
        -- Stall if there is a jump in this cycle.
        | jump_current_cycle = Nop JumpFirstCycle
        -- Stall if there is a store hazard in this cycle.
        | store_hazard_current_cycle = Nop StoreHazardFirstCycle
        -- Stall if there is a load hazard in this cycle.
        | load_hazard_current_cycle = Nop LoadHazardFirstCycle
        -- Otherwise we process the decoded instruction.
        | otherwise = ir

  modify $ \s -> s {stateExInstr = ir', stateExPc = pc}

  when (ir' == Nop StoreHazardFirstCycle) $ do
    setLines $ \c -> c {ctrlDeStoreHazard = Just pc}

  when (ir' == Nop LoadHazardFirstCycle) $ do
    setLines $ \c -> c {ctrlDeLoadHazard = Just pc}

-- | Execute stage. Holds while memory has not answered (see 'writeback').
execute :: forall r. (RegFileOps r) => CPUM r ()
execute = do
  notReady <- gets (ctrlMemNotReady . stateCtrl)
  unless notReady executeFresh

-- | Execute stage, in a cycle where the pipeline advances.
executeFresh :: forall r. (RegFileOps r) => CPUM r ()
executeFresh = do
  ir <- gets stateExInstr

  -- Default values.
  modify $ \s ->
    s { stateMeInstr = ir,
        stateMeRes = 0,
        stateMeAddr = 0
      }

  setLines $ \c -> c {ctrlExInstr = Just ir}

  case ir of
    Core.Instruction.RType op _ rs1 rs2 -> do
      r1 <- getFirstArg rs1
      r2 <- getSecondArg rs2
      let res = alu op r1 r2
      modify $ \s -> s {stateMeRes = res}
    Core.Instruction.IType (Arith op) _ rs1 imm -> do
      r1 <- getFirstArg rs1
      let imm' = signExtend imm
      let res = alu op r1 imm'
      modify $ \s -> s {stateMeRes = res}
    Core.Instruction.IType (Load _ _) _ rs1 imm -> do
      r1 <- getFirstArg rs1
      let imm' = signExtend imm
      let res = alu ADD r1 imm'
      modify $ \s -> s {stateMeAddr = unpack res}
    Core.Instruction.SType size imm rs1 rs2 -> do
      r1 <- getFirstArg rs1
      r2 <- getSecondArg rs2
      let imm' = signExtend imm
      let res = alu ADD r1 imm'
      modify $ \s -> s {stateMeRes = r2, stateMeAddr = unpack res}
      setLines $ \c -> c {ctrlExStoreAddrSize = Just (unpack res, size)}
    Core.Instruction.BType cmp imm rs1 rs2 -> do
      r1 <- getFirstArg rs1
      r2 <- getSecondArg rs2
      pc <- gets $ pack . stateExPc
      when (branch cmp r1 r2) $ do
        let imm' = signExtend imm
        let branchAddr = alu ADD pc imm'
        setLines $ \c -> c {ctrlExJumpAddr = Just (unpack branchAddr)}
    Core.Instruction.JType _ imm -> do
      pc <- gets $ pack . stateExPc
      let res = alu ADD pc 4
      modify $ \s -> s {stateMeRes = res}
      let imm' = signExtend imm
      let jumpAddr = alu ADD pc imm'
      setLines $ \c -> c {ctrlExJumpAddr = Just (unpack jumpAddr)}
    Core.Instruction.IType Jump _ rs1 imm -> do
      r1 <- getFirstArg rs1
      pc <- gets $ pack . stateExPc
      let res = alu ADD pc 4
      modify $ \s -> s {stateMeRes = res}
      let imm' = signExtend imm
      let jumpAddr = alu ADD r1 imm'
      setLines $ \c -> c {ctrlExJumpAddr = Just (unpack jumpAddr)}
    Core.Instruction.UType base _ imm -> do
      base' <-
        case base of
          Zero -> pure 0
          PC -> gets $ pack . stateExPc
      let imm' = imm ++# (0 :: BitVector 12)
      let res = alu ADD base' imm'
      modify $ \s -> s {stateMeRes = res}
    Core.Instruction.IType (Env Call) _ _ _ -> do
      pc <- gets stateExPc
      modify $ \s -> s {stateMeRes = pack $ pc + 4}
    Core.Instruction.IType (Env Break) _ _ _ -> do
      pc <- gets stateExPc
      modify $ \s -> s {stateMeRes = pack $ pc + 4}
    Core.Instruction.Nop _ -> pure ()
  where
    getFirstArg :: RegIdx -> CPUM r Word
    getFirstArg idx = do
      rf <- gets stateRegFile
      regWithFwd idx (lookupRFg idx rf)

    getSecondArg :: RegIdx -> CPUM r Word
    getSecondArg idx = do
      rf <- gets stateRegFile
      regWithFwd idx (lookupRFg idx rf)

    regWithFwd :: RegIdx -> Word -> CPUM r Word
    regWithFwd idx def = do
      ctrl <- gets stateCtrl
      let pick line = do
            (fwdIdx, fwdVal) <- line ctrl
            guard $ fwdIdx == idx && idx /= 0
            pure fwdVal
      pure $ fromMaybe def (pick ctrlMeRegFwd <|> pick ctrlWbRegFwd)

alu :: Arith -> Word -> Word -> Word
alu op lhs rhs = case op of
  ADD -> lhs + rhs
  SUB -> lhs - rhs
  XOR -> lhs .^. rhs
  OR -> lhs .|. rhs
  AND -> lhs .&. rhs
  SLL -> sllWord lhs (shiftBits rhs)
  SRL -> srlWord lhs (shiftBits rhs)
  SRA -> sraWord lhs (shiftBits rhs)
  SLT -> set (sign lhs < sign rhs)
  SLTU -> set (lhs < rhs)
  where
    shiftBits = slice d4 d0
    sign = unpack @(Signed 32)
    set b = if b then 1 else 0

-- | The three RISC-V shifts, with the shift amount kept as a bitvector.
--
-- RISC-V takes the amount from the low five bits of the second operand, so no
-- amount can reach the word width and these agree with the SMT shifts on the
-- nose.
--
-- They exist as named 'OPAQUE' functions, rather than 'shiftL' applied inline,
-- for the verifier's sake. 'Data.Bits.shiftL' takes an 'Int', so an inline
-- amount forces @fromIntegral@ on the five-bit slice, and that is modelled
-- through 'Integer': every shift site then emits an integer round trip
-- (@ubv_to_int@ to @int_to_bv@) wrapped in two overflow guards. Those few terms
-- pull integer arithmetic into a query that is otherwise pure bitvectors and
-- arrays, and Bitwuzla and Yices have no integer theory at all -- they reject
-- such a query outright rather than solve it slowly. Keeping the amount a
-- 'BitVector' behind a name lets "Axioms" map each shift to its SMT
-- counterpart; see 'ArrayRF.sllWordE'.
--
-- OPAQUE is essential, for the same reason as 'ArrayRF.loadRA': the axiom is
-- keyed on the name, so an inlined wrapper would leave nothing to rewrite.
{-# OPAQUE sllWord #-}
sllWord :: Word -> BitVector 5 -> Word
sllWord x n = shiftL x (fromIntegral n)

-- | Logical right shift; 'BitVector' is unsigned, so 'shiftR' is @bvlshr@.
{-# OPAQUE srlWord #-}
srlWord :: Word -> BitVector 5 -> Word
srlWord x n = shiftR x (fromIntegral n)

-- | Arithmetic right shift, via 'Signed' as the ISA prescribes.
{-# OPAQUE sraWord #-}
sraWord :: Word -> BitVector 5 -> Word
sraWord x n = pack (shiftR (unpack x :: Signed 32) (fromIntegral n))

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

-- | Memory stage. Holds while memory has not answered (see 'writeback').
memory :: CPUM r ()
memory = do
  notReady <- gets (ctrlMemNotReady . stateCtrl)
  unless notReady memoryFresh

-- | Memory stage, in a cycle where the pipeline advances.
memoryFresh :: CPUM r ()
memoryFresh = do
  ir <- gets stateMeInstr
  res <- gets stateMeRes
  addr <- gets stateMeAddr

  modify $ \s -> s {stateWbInstr = ir, stateWbRes = res}

  -- Default register forwarding.
  setLines $ \c -> c {ctrlMeRegFwd = Nothing}

  case ir of
    Core.Instruction.RType _ rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Core.Instruction.IType (Arith _) rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Core.Instruction.IType (Load size _) _ _ _ -> do
      setLines $ \c -> c {ctrlMeMemInstr = True}
      readRAM addr size
    Core.Instruction.SType size _ _ _ -> do
      setLines $ \c ->
        c {ctrlMeMemInstr = True, ctrlMeStoreAddrSize = Just (addr, size)}
      writeRAM addr size res
    Core.Instruction.JType rd _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Core.Instruction.IType Jump rd _ _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    Core.Instruction.UType _ rd _ ->
      setLines $ \c -> c {ctrlMeRegFwd = Just (rd, res)}
    _ -> pure ()

-- | Writeback stage. While memory has not answered ('inputMem' is 'Nothing'),
-- the instruction waits, and raises 'ctrlMemNotReady' so that the other stages
-- hold too: the whole pipeline keeps its state and makes no request, whatever
-- the request memory delays. Otherwise the instruction completes
-- ('writebackFresh').
writeback :: forall r. (RegFileOps r) => CPUM r ()
writeback = do
  input <- asks inputMem

  if isNothing input
    then setLines $ \c -> c {ctrlMemNotReady = True}
    else writebackFresh

-- | Writeback stage, in a cycle where the instruction in it completes.
writebackFresh :: forall r. (RegFileOps r) => CPUM r ()
writebackFresh = do
  ir <- gets stateWbInstr
  res <- gets stateWbRes

  case ir of
    Core.Instruction.RType _ rd _ _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Core.Instruction.IType (Arith _) rd _ _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Core.Instruction.IType (Load size sign) rd _ _ -> do
      input <- asks inputMem
      case input of
        -- this case should not arise
        Nothing -> pure ()
        Just w -> do
          let val = loadExtend size sign w
          setLines $ \c -> c {ctrlWbRegFwd = Just (rd, val)}
          writeRF rd val
    Core.Instruction.JType rd _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Core.Instruction.IType Jump rd _ _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Core.Instruction.UType _ rd _ -> do
      setLines $ \c -> c {ctrlWbRegFwd = Just (rd, res)}
      writeRF rd res
    Core.Instruction.IType (Env Call) _ _ _ -> do
      modify $ \s -> s {stateHalt = Just (Syscall $ unpack res)}
    Core.Instruction.IType (Env Break) _ _ _ -> do
      modify $ \s -> s {stateHalt = Just (EBreak $ unpack res)}
    _ -> setLines $ \c -> c {ctrlWbRegFwd = Nothing}
  where
    writeRF :: RegIdx -> Word -> CPUM r ()
    writeRF idx val =
      modify $ \s -> s {stateRegFile = modifyRFg idx val (stateRegFile s)}

readPC :: (MonadWriter Output m) => Address -> m ()
readPC addr =
  tell $
    (mempty :: Output)
      { outMem = pure $ MemFetch addr }

readRAM :: (MonadWriter Output m) => Address -> Size -> m ()
readRAM addr size =
  tell $
    (mempty :: Output)
      { outMem = pure $ MemRead addr size }

writeRAM :: (MonadWriter Output m) => Address -> Size -> Word -> m ()
writeRAM addr size val =
  tell $
    (mempty :: Output)
      { outMem = pure $ MemWrite addr size val }
