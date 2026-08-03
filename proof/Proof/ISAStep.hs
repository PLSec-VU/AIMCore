{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An ISA-level (unpipelined) step function.
--
-- "ISA" itself only provides the decode side: 'ISA.interp'' turns an
-- 'Instruction' into an @'ISA.Instr' 'ISA.Func'@, and 'ISA.apply' evaluates one
-- of those 'ISA.Func's against the two source-register values and the PC. It
-- stops short of saying how the architectural state evolves, which is what the
-- invariant's @(isaPc, isaRegFile, isaMem)@ needs. This module supplies that
-- missing piece, staying as close to "ISA" as possible so the specification
-- side remains the one in "ISA" rather than a second implementation.
--
-- Parameterised over the register-file and memory representations for the same
-- reason as "Proof.Machine": the @Vec@-backed ones cannot be symbolically executed.
module Proof.ISAStep
  ( IsaStateG (..),
    IsaState,
    StepG (..),
    Step,
    isaStep,
    isaStepDecoded,
    isaRun,
    isaInstrAt,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Functor.Identity
import qualified ISA
import Instruction
import Proof.Machine (MemBytes, MemOps (..))
import RegFile
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Architectural state: the ISA-visible triple.
data IsaStateG r m = IsaState
  { isaPc :: ISA.PC,
    isaRegFile :: r Identity,
    isaMem :: m
  }

-- | The concrete architectural state the QuickCheck harness uses.
type IsaState = IsaStateG RegFile MemBytes

deriving instance (Show (r Identity), Show m) => Show (IsaStateG r m)

deriving instance (Eq (r Identity), Eq m) => Eq (IsaStateG r m)

-- | The result of one architectural step. 'IsaHalted' covers @ebreak@ and
-- @ecall@, which is where 'Core' parks in a 'Core.HaltState'.
data StepG r m
  = Next (IsaStateG r m)
  | IsaHalted

type Step = StepG RegFile MemBytes

deriving instance (Show (r Identity), Show m) => Show (StepG r m)

deriving instance (Eq (r Identity), Eq m) => Eq (StepG r m)

-- | The instruction the ISA would execute next.
isaInstrAt :: (MemOps m) => IsaStateG r m -> Instruction
isaInstrAt (IsaState pc _ mem) = decode' (memReadWord pc mem)

isaStep :: (RegFileOps r, MemOps m) => IsaStateG r m -> StepG r m
isaStep st = isaStepDecoded (isaInstrAt st) st

-- | Step the ISA using an instruction that has already been decoded.
--
-- This is definitionally the same transition as 'isaStep' when @ir@ is
-- @isaInstrAt st@.  Keeping the decoded instruction explicit is useful in the
-- refinement proof: the invariant already states that the core's execute-stage
-- instruction equals @isaInstrAt st@, so the transition can execute that
-- instruction directly instead of nesting one decoder inside another.
isaStepDecoded ::
  (RegFileOps r, MemOps m) =>
  Instruction ->
  IsaStateG r m ->
  StepG r m
isaStepDecoded ir st@(IsaState pc rf mem) =
  case instr of
    ISA.Reg rd f ->
      Next st {isaPc = pc + 4, isaRegFile = modifyRFg rd (pure (ap f)) rf}
    ISA.Load size sign rd f ->
      let val = loadExtend size sign (memReadWord (ap f) mem)
       in Next st {isaPc = pc + 4, isaRegFile = modifyRFg rd (pure val) rf}
    ISA.Jump rd link target ->
      Next
        st
          { isaPc = ap target,
            isaRegFile = modifyRFg rd (pure (bitCoerce (ap link))) rf
          }
    ISA.Store size addr rs2 ->
      Next st {isaPc = pc + 4, isaMem = memWriteWord size (ap addr) (reg (Just rs2)) mem}
    ISA.Branch cond target ->
      Next st {isaPc = if ap cond then ap target else pc + 4}
    ISA.Nop -> Next st {isaPc = pc + 4}
    ISA.Break -> IsaHalted
    ISA.Syscall -> IsaHalted
  where
    instr = ISA.interp' ir

    reg = maybe 0 (\idx -> runIdentity (lookupRFg idx rf))

    -- 'ISA.Func' is evaluated against the values of its two dependency
    -- registers and the PC, exactly as 'ISA.apply' prescribes.
    ap :: ISA.Func a -> a
    ap f = ISA.unDone (ISA.apply f (reg (ISA.getR1 instr)) (reg (ISA.getR2 instr)) pc)

-- | Run at most @n@ architectural steps, stopping early on halt. Returns the
-- states visited, starting with the initial one.
isaRun :: (RegFileOps r, MemOps m) => Int -> IsaStateG r m -> [IsaStateG r m]
isaRun n st
  | n <= 0 = [st]
  | otherwise = case isaStep st of
      IsaHalted -> [st]
      Next st' -> st : isaRun (n - 1) st'
