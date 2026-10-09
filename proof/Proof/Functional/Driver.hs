-- | The driver: how many cycles 'Proof.Machine.stepSys' should advance the core
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
module Proof.Functional.Driver
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
import Data.Functor.Identity
import Instruction hiding (loadHazard, storeHazard)
import qualified Instruction
import Memory.Types (MemOps (..))
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | @ctrlMeRegFwd@ as set by 'Core.memory'. Note that a load in the memory
-- stage forwards nothing (its value is not available yet).
meFwd :: SysG r m -> Maybe (RegIdx, Word)
meFwd (Sys st _ _) =
  case stateMeInstr st of
    RType _ rd _ _ -> Just (rd, res)
    IType (Arith _) rd _ _ -> Just (rd, res)
    JType rd _ -> Just (rd, res)
    IType Jump rd _ _ -> Just (rd, res)
    UType _ rd _ -> Just (rd, res)
    _ -> Nothing
  where
    res = runIdentity (stateMeRes st)

-- | @ctrlWbRegFwd@ as set by 'Core.writeback'. A load forwards the value read
-- from memory on this cycle's 'inputMem', not 'stateWbRes'.
wbFwd :: SysG r m -> Maybe (RegIdx, Word)
wbFwd (Sys st inp _) =
  case stateWbInstr st of
    RType _ rd _ _ -> Just (rd, res)
    IType (Arith _) rd _ _ -> Just (rd, res)
    IType (Load size sign) rd _ _ -> Just (rd, loadExtend size sign (runIdentity (inputMem inp)))
    JType rd _ -> Just (rd, res)
    IType Jump rd _ _ -> Just (rd, res)
    UType _ rd _ -> Just (rd, res)
    _ -> Nothing
  where
    res = runIdentity (stateWbRes st)

-- | The value 'Core.execute' reads for a source register, honouring the
-- memory-then-writeback forwarding priority of 'Core.execute'.
exArg :: (RegFileOps r) => SysG r m -> RegIdx -> Word
exArg sys@(Sys st _ _) idx =
  case pick (meFwd sys) <|> pick (wbFwd sys) of
    Just v -> v
    Nothing -> runIdentity (lookupRFg idx (stateRegFile st))
  where
    pick m = do
      (fwdIdx, fwdVal) <- m
      if fwdIdx == idx && idx /= 0 then Just fwdVal else Nothing

-- | Does the execute-stage instruction take a jump this cycle? This is
-- @ctrlExJumpAddr@ becoming 'Just'.
takingJump :: (RegFileOps r) => SysG r m -> Bool
takingJump sys@(Sys st _ _) =
  case stateExInstr st of
    BType cmp _ rs1 rs2 ->
      runIdentity (branch cmp (pure (exArg sys rs1)) (pure (exArg sys rs2)))
    JType _ _ -> True
    IType Jump _ _ _ -> True
    _ -> False

-- | The store-hazard condition of 'Core.decode': the instruction word at the
-- decode-stage PC overlaps a store either in the execute stage or in the memory
-- stage.
--
-- The overlap test is 'Instruction.storeHazard', the core's own, which is why
-- the store's size is carried alongside its address: a byte store and a word
-- store at the same address clash with different sets of PCs. 'Core.decode'
-- reads the pair off @ctrlExStoreAddrSize@ / @ctrlMeStoreAddrSize@; here the
-- execute-stage address is recomputed from the forwarded operand, as
-- 'Core.execute' would, and the memory-stage one is already in @stateMeAddr@.
storeHazard :: (RegFileOps r) => SysG r m -> Bool
storeHazard sys@(Sys st _ _) =
  maybe False (Instruction.storeHazard dePc) exStore
    || maybe False (Instruction.storeHazard dePc) meStore
  where
    dePc = stateDePc st
    exStore = case stateExInstr st of
      SType size imm rs1 _ ->
        Just
          ( unpack (runIdentity (alu ADD (pure (exArg sys rs1)) (pure (signExtend imm)))),
            size
          )
      _ -> Nothing
    meStore = case stateMeInstr st of
      SType size _ _ _ -> Just (stateMeAddr st, size)
      _ -> Nothing

-- | The load-hazard condition of 'Core.decode', between the instruction
-- arriving on 'inputMem' and the one in the execute stage.
loadHazard :: SysG r m -> Bool
loadHazard (Sys st inp _) =
  Instruction.loadHazard deIr (stateExInstr st)
  where
    expInstr = stateDeExpInstr st
    deIr
      | expInstr = decode' (runIdentity (inputMem inp))
      | otherwise = Nop MemoryBusBusy

-- | The driver's case table. The cases are checked in order.
driver :: (RegFileOps r) => SysG r m -> Int
driver sys@(Sys st _ _)
  | stateExInstr st == Nop FirstCycle = 1
  | stateExInstr st == Nop Halted = 0
  | isEnvInstr (stateExInstr st) = 2
  | takingJump sys = 2
  | storeHazard sys = if isMemInstr (stateExInstr st) then 3 else 2
  | loadHazard sys = 3
  | not (isMemInstr (stateWbInstr st)) = 0
  | not (isMemInstr (stateMeInstr st)) = 1
  | not (isMemInstr (stateExInstr st)) = 2
  | otherwise = 3

-- | Which case of the table 'driver' fired. Used to measure how much of the
-- table the tests actually reach.
driverCaseName :: (RegFileOps r) => SysG r m -> String
driverCaseName sys@(Sys st _ _)
  | stateExInstr st == Nop FirstCycle = "firstCycle"
  | stateExInstr st == Nop Halted = "halted"
  | isEnvInstr (stateExInstr st) = "env"
  | takingJump sys = "jump"
  | storeHazard sys = if isMemInstr (stateExInstr st) then "storeHazard/mem" else "storeHazard/nomem"
  | loadHazard sys = "loadHazard"
  | not (isMemInstr (stateWbInstr st)) = "steady/wb-nomem"
  | not (isMemInstr (stateMeInstr st)) = "steady/me-nomem"
  | not (isMemInstr (stateExInstr st)) = "steady/ex-nomem"
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
-- 'Proof.Machine.stepSys' steps taken, or 'Nothing' if @fuel@ ran out (which is what
-- happens once the core has halted, since the execute stage then holds
-- @Nop Halted@ forever).
--
-- \"No longer a no-op\" is read as 'isBubble': @Nop DecodeFail@ counts
-- as a real instruction, since it is what an undecodable word in memory
-- decodes to.
driverRef :: (RegFileOps r, MemOps m) => Int -> SysG r m -> Maybe Int
driverRef fuel sys0 = go 1 (stepSys sys0)
  where
    go n s@(Sys st _ _)
      | not (isBubble (stateExInstr st)) = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (stepSys s)
