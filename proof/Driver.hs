-- | The driver from @proof/driver.txt@, as code.
--
-- The driver says how many cycles 'Machine.stepSys' should advance the core
-- before its state is compared against the ISA again. Its stated invariant is
-- operational -- \"step until the instruction in the execute phase is no longer
-- a no-op\" -- so this module provides both:
--
--   * 'driver', the case table from @driver.txt@, and
--   * 'driverRef', the operational reading, obtained by actually stepping.
--
-- Checking these two against each other is the first thing the test suite does.
--
-- NOTE on signatures: @driver.txt@ writes @isJumpInstr@ and @storeHazard@ as
-- functions of a handful of state fields. They cannot in fact be computed from
-- those fields alone -- both depend on the execute stage's forwarded operand
-- values, which in turn depend on the register file, on the memory-stage and
-- writeback-stage forwarding lines, and (for a load in writeback) on
-- 'inputMem'. So they are given here as functions of the whole system state.
-- The stage ordering below mirrors 'Core.pipe': writeback, then memory, then
-- execute.
module Driver
  ( driver,
    driverCaseName,
    driverRef,
    isMemInstr,
    isEnvInstr,
    isJumpInstr,
    storeHazard,
    loadHazardD,
    exArg,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Instruction
import Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | From @invariant.txt@: loads and stores are the memory instructions.
isMemInstr :: Instruction -> Bool
isMemInstr ir = isLoad ir || isStore ir

-- | @ecall@ / @ebreak@.
isEnvInstr :: Instruction -> Bool
isEnvInstr ir = isCall ir || isBreak ir

-- | The register file as 'Core.execute' sees it, i.e. after 'Core.writeback'
-- has committed the writeback-stage instruction.
rfAfterWriteback :: (RegFileOps r) => SysG r m -> r Identity
rfAfterWriteback (Sys st inp _) =
  case stateWbInstr st of
    RType _ rd _ _ -> put rd res
    IType (Arith _) rd _ _ -> put rd res
    IType (Load size sign) rd _ _ -> put rd (pure (loadExtend size sign (runIdentity (inputMem inp))))
    JType rd _ -> put rd res
    IType Jump rd _ _ -> put rd res
    UType _ rd _ -> put rd res
    _ -> stateRegFile st
  where
    res = stateWbRes st
    put rd v = modifyRFg rd v (stateRegFile st)

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
exArg sys idx =
  case pick (meFwd sys) <|> pick (wbFwd sys) of
    Just v -> v
    Nothing -> runIdentity (lookupRFg idx (rfAfterWriteback sys))
  where
    pick m = do
      (fwdIdx, fwdVal) <- m
      if fwdIdx == idx && idx /= 0 then Just fwdVal else Nothing

-- | Does the execute-stage instruction take a jump this cycle? This is
-- @ctrlExJumpAddr@ becoming 'Just'.
isJumpInstr :: (RegFileOps r) => SysG r m -> Bool
isJumpInstr sys =
  case exInstr sys of
    BType cmp _ rs1 rs2 ->
      runIdentity (branch cmp (pure (exArg sys rs1)) (pure (exArg sys rs2)))
    JType _ _ -> True
    IType Jump _ _ _ -> True
    _ -> False

-- | The store-hazard condition of 'Core.decode': the decode-stage PC collides
-- with a store address either in the execute stage or in the memory stage.
storeHazard :: (RegFileOps r) => SysG r m -> Bool
storeHazard sys@(Sys st _ _) =
  exStore == Just dePc || meStore == Just dePc
  where
    dePc = stateDePc st
    exStore = case exInstr sys of
      SType _ imm rs1 _ ->
        Just (unpack (runIdentity (alu ADD (pure (exArg sys rs1)) (pure (signExtend imm)))))
      _ -> Nothing
    meStore = case stateMeInstr st of
      SType {} -> Just (stateMeAddr st)
      _ -> Nothing

-- | The load-hazard condition of 'Core.decode', between the instruction
-- arriving on 'inputMem' and the one in the execute stage.
loadHazardD :: SysG r m -> Bool
loadHazardD sys@(Sys _ inp _) =
  Instruction.loadHazard deIr (exInstr sys)
  where
    deIr
      | inputIsInstr inp = decode' (runIdentity (inputMem inp))
      | otherwise = Nop MemoryBusBusy

-- | The case table from @driver.txt@, in order.
driver :: (RegFileOps r) => SysG r m -> Int
driver sys@(Sys st _ _)
  | ex == Nop FirstCycle = 1
  -- @driver.txt@ has no case for a halted core; every other case is guarded by
  -- @stateHalt == Running@.
  | not (running sys) = 0
  | isEnvInstr ex = 2
  | isJumpInstr sys = 2
  | storeHazard sys = if isMemInstr ex then 3 else 2
  | loadHazardD sys = 3
  | not (isMemInstr (stateWbInstr st)) = 0
  | not (isMemInstr (stateMeInstr st)) = 1
  | not (isMemInstr ex) = 2
  | otherwise = 3
  where
    ex = exInstr sys

-- | Which case of the table 'driver' fired. Used to measure how much of the
-- table the tests actually reach.
driverCaseName :: (RegFileOps r) => SysG r m -> String
driverCaseName sys@(Sys st _ _)
  | ex == Nop FirstCycle = "firstCycle"
  | not (running sys) = "halted"
  | isEnvInstr ex = "env"
  | isJumpInstr sys = "jump"
  | storeHazard sys = if isMemInstr ex then "storeHazard/mem" else "storeHazard/nomem"
  | loadHazardD sys = "loadHazard"
  | not (isMemInstr (stateWbInstr st)) = "steady/wb-nomem"
  | not (isMemInstr (stateMeInstr st)) = "steady/me-nomem"
  | not (isMemInstr ex) = "steady/ex-nomem"
  | otherwise = "steady/all-mem"
  where
    ex = exInstr sys

-- | The operational reading of the driver's stated invariant: step until the
-- execute stage holds something that is not a no-op. Returns the number of
-- 'Machine.stepSys' steps taken, or 'Nothing' if @fuel@ ran out (which is what
-- happens once the core has halted, since the execute stage then holds
-- @Nop Halted@ forever).
--
-- \"No longer a no-op\" is read as 'Machine.isBubble': @Nop DecodeFail@ counts
-- as a real instruction, since it is what an undecodable word in memory
-- decodes to.
driverRef :: (RegFileOps r, MemOps m) => Int -> SysG r m -> Maybe Int
driverRef fuel sys0 = go 1 (stepSys sys0)
  where
    go n s
      | not (isBubble (exInstr s)) = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (stepSys s)
