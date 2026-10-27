-- | The driver: how many cycles 'Proof.Core.Functional.Machine.stepSys' should advance the core
-- before its state is compared against the ISA again. Its stated invariant is
-- operational -- \"step until the instruction in the execute phase is no longer
-- a no-op\" -- so this module provides both:
--
--   * 'driver', a case table over the pipeline state, and
--   * 'driverRef', the operational reading, obtained by actually stepping.
--
-- Checking these two against each other is the first thing the test suite does.
--
-- 'takingJump' and 'storeHazard' are functions of the whole system state rather
-- than of a few fields: both depend on the execute stage's forwarded operand
-- values, which in turn depend on the register file, on the memory-stage and
-- writeback-stage forwarding lines, and (for a load in writeback) on
-- 'inputMem'.
module Proof.Core.Functional.Driver
  ( driver,
    driverCaseName,
    driverRef,
    takingJump,
    storeHazard,
    loadHazard,
    exArg,
    isBubble
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Maybe (fromMaybe)
import Core.Instruction hiding (loadHazard, storeHazard)
import qualified Core.Instruction
import Memory.Types (MemOps (..))
import Proof.Core.Functional.Machine
import Core.RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | @ctrlMeRegFwd@ as set by 'Core.memory'. Note that a load in the memory
-- stage forwards nothing (its value is not available yet).
meFwd :: SysG r m -> Maybe (RegIdx, Word)
meFwd (Sys core _ _) =
  case stateMeInstr core of
    RType _ rd _ _ -> Just (rd, res)
    IType (Arith _) rd _ _ -> Just (rd, res)
    JType rd _ -> Just (rd, res)
    IType Jump rd _ _ -> Just (rd, res)
    UType _ rd _ -> Just (rd, res)
    _ -> Nothing
  where
    res = stateMeRes core

-- | @ctrlWbRegFwd@ as set by 'Core.writeback'. A load forwards the value read
-- from memory on this cycle's 'inputMem', not 'stateWbRes'.
wbFwd :: SysG r m -> Maybe (RegIdx, Word)
wbFwd (Sys core inp _) =
  case stateWbInstr core of
    RType _ rd _ _ -> Just (rd, res)
    IType (Arith _) rd _ _ -> Just (rd, res)
    IType (Load size sign) rd _ _ -> Just (rd, loadExtend size sign (fromMaybe 0 (inputMem inp)))
    JType rd _ -> Just (rd, res)
    IType Jump rd _ _ -> Just (rd, res)
    UType _ rd _ -> Just (rd, res)
    _ -> Nothing
  where
    res = stateWbRes core

-- | The value 'Core.execute' reads for a source register, honouring the
-- memory-then-writeback forwarding priority of 'Core.execute'.
exArg :: (RegFileOps r) => SysG r m -> RegIdx -> Word
exArg sys@(Sys core _ _) idx =
  case pick (meFwd sys) <|> pick (wbFwd sys) of
    Just v -> v
    Nothing -> lookupRFg idx (stateRegFile core)
  where
    pick m = do
      (fwdIdx, fwdVal) <- m
      if fwdIdx == idx && idx /= 0 then Just fwdVal else Nothing

-- | Does the execute-stage instruction take a jump this cycle? This is
-- @ctrlExJumpAddr@ becoming 'Just'.
takingJump :: (RegFileOps r) => SysG r m -> Bool
takingJump sys@(Sys core _ _) =
  case stateExInstr core of
    BType cmp _ rs1 rs2 ->
      (branch cmp (exArg sys rs1) (exArg sys rs2))
    JType _ _ -> True
    IType Jump _ _ _ -> True
    _ -> False

-- | The store-hazard condition of 'Core.decode': the instruction word at the
-- decode-stage PC overlaps a store either in the execute stage or in the memory
-- stage.
--
-- The overlap test is 'Core.Instruction.storeHazard', the core's own, which is why
-- the store's size is carried alongside its address: a byte store and a word
-- store at the same address clash with different sets of PCs. 'Core.decode'
-- reads the pair off @ctrlExStoreAddrSize@ / @ctrlMeStoreAddrSize@; here the
-- execute-stage address is recomputed from the forwarded operand, as
-- 'Core.execute' would, and the memory-stage one is already in @stateMeAddr@.
storeHazard :: (RegFileOps r) => SysG r m -> Bool
storeHazard sys@(Sys core _ _) =
  maybe False (Core.Instruction.storeHazard dePc) exStore
    || maybe False (Core.Instruction.storeHazard dePc) meStore
  where
    dePc = stateDePc core
    exStore = case stateExInstr core of
      SType size imm rs1 _ ->
        Just
          ( unpack (alu ADD (exArg sys rs1) (signExtend imm)),
            size
          )
      _ -> Nothing
    meStore = case stateMeInstr core of
      SType size _ _ _ -> Just (stateMeAddr core, size)
      _ -> Nothing

-- | The load-hazard condition of 'Core.decode', between the instruction
-- arriving on 'inputMem' and the one in the execute stage.
loadHazard :: SysG r m -> Bool
loadHazard (Sys core inp _) =
  Core.Instruction.loadHazard deIr (stateExInstr core)
  where
    expInstr = stateDeExpInstr core
    deIr
      | expInstr = maybe (Nop NoInstruction) decode' (inputMem inp)
      | otherwise = Nop NoInstruction

-- | The driver's case table. The cases are checked in order. The driver is
-- only ever run from states that satisfy the invariant
-- ('Proof.Core.Functional.Invariant.invAt'); elsewhere it need not agree
-- with 'driverRef'.
driver :: (RegFileOps r) => SysG r m -> Int
driver sys@(Sys core _ _)
  | stateExInstr core == Nop FirstCycle = 1
  | stateExInstr core == Nop Halted = 0
  | isEnvInstr (stateExInstr core) = 2
  | takingJump sys = 2
  | storeHazard sys = if isMemInstr (stateExInstr core) then 3 else 2
  | loadHazard sys = 3
  | not (isMemInstr (stateWbInstr core)) = 0
  | not (isMemInstr (stateMeInstr core)) = 1
  | not (isMemInstr (stateExInstr core)) = 2
  | otherwise = 3

-- | Which case of the table 'driver' fired. Used to measure how much of the
-- table the tests actually reach.
driverCaseName :: (RegFileOps r) => SysG r m -> String
driverCaseName sys@(Sys core _ _)
  | stateExInstr core == Nop FirstCycle = "firstCycle"
  | stateExInstr core == Nop Halted = "halted"
  | isEnvInstr (stateExInstr core) = "env"
  | takingJump sys = "jump"
  | storeHazard sys = if isMemInstr (stateExInstr core) then "storeHazard/mem" else "storeHazard/nomem"
  | loadHazard sys = "loadHazard"
  | not (isMemInstr (stateWbInstr core)) = "steady/wb-nomem"
  | not (isMemInstr (stateMeInstr core)) = "steady/me-nomem"
  | not (isMemInstr (stateExInstr core)) = "steady/ex-nomem"
  | otherwise = "steady/all-mem"

-- | Is this a pipeline bubble, as opposed to a real instruction?
--
-- The driver's stated invariant is \"step until the execute stage is no longer
-- a no-op\", but that is slightly too coarse: @Nop DecodeFail@ is what an
-- undecodable memory word decodes to, so it /is/ the architectural instruction
-- at that PC and the ISA steps over it like any other. Only the stall reasons
-- are genuine bubbles.
isBubble :: Instruction -> Bool
isBubble (Nop DecodeFail) = False
isBubble (Nop _) = True
isBubble _ = False

-- | The operational reading of the driver's stated invariant: step until the
-- execute stage holds something that is not a no-op. Returns the number of
-- 'Proof.Core.Functional.Machine.stepSys' steps taken, or 'Nothing' if @fuel@ ran out (which is what
-- happens once the core has halted, since the execute stage then holds
-- @Nop Halted@ forever).
--
-- \"No longer a no-op\" is read as 'isBubble': @Nop DecodeFail@ counts
-- as a real instruction, since it is what an undecodable word in memory
-- decodes to.
driverRef :: (RegFileOps r, MemOps m) => Int -> SysG r m -> Maybe Int
driverRef fuel sys0 = go 1 (stepSys sys0)
  where
    go n s@(Sys core _ _)
      | not (isBubble (stateExInstr core)) = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (stepSys s)
