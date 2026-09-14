{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The ISA specification: what each instruction means, and how the
-- architectural state evolves.
--
-- Two halves, both semantics. 'interp'' turns an 'Instruction.Instruction' into
-- an @'Instr' 'Func'@ -- the effect it denotes -- and 'apply' evaluates one of
-- those 'Func's against the two source-register values and the PC. On top of
-- that, 'isaStep' says how the @(isaPc, isaRegFile, isaMem)@ triple moves.
--
-- The architectural state is parameterised over the register-file and memory
-- representations because the @Vec@-backed ones cannot be symbolically
-- executed; see 'RegFile.RegFileOps' and 'Memory.Types.MemOps'.
--
-- This is specification, not proof machinery. The refinement proof is stated
-- against it, so it must not depend on anything the proof defines.
module ISA
  ( Func,
    Done (..),
    apply,
    DepReg (..),
    Instr (..),
    PC,
    getR1,
    getR2,
    interp,
    interp',
    IsaStateG (..),
    IsaState,
    StepG (..),
    Step,
    isaStep,
    isaStepDecoded,
    isaRun,
    isaInstrAt,
  )
where

import Access
import Clash.Prelude hiding (Const, Log, Ordering (..), Word, def, init, lift, log)
import Core hiding (Syscall)
import Data.Functor.Identity
import Instruction (Instruction, Sign, decode', loadExtend)
import qualified Instruction
import Memory.Types (MemBytes, MemOps (..))
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type PC = Address

data Func a = Func
  { isaFunc :: Word -> Word -> PC -> a,
    isaDeps :: (Maybe RegIdx, Maybe RegIdx)
  }

instance Show (Func a) where
  show _ = "<Func>"

instance Functor Func where
  fmap g (Func f d) = Func (\r1 r2 pc -> g $ f r1 r2 pc) d

-- | The result of evaluating a 'Func'. 'isaStepDecoded' is the only consumer:
-- it evaluates each 'Func' against its two dependency registers and the PC to
-- get the architectural transition.
newtype Done a = Done {unDone :: a}
  deriving (Show, Eq)

apply :: Func a -> Word -> Word -> PC -> Done a
apply (Func f _) r1 r2 pc = Done $ f r1 r2 pc

data Instr f
  = Reg RegIdx (f Word)
  | Load Size Sign RegIdx (f Address)
  | Jump RegIdx (f Address) (f Address)
  | Store Size (f Address) RegIdx
  | Branch (f Bool) (f Address)
  | Nop
  | Break
  | Syscall

deriving instance
  ( Show (f Word),
    Show (f Address),
    Show (f Bool)
  ) =>
  Show (Instr f)

deriving instance
  ( Eq (f Word),
    Eq (f Address),
    Eq (f Bool)
  ) =>
  Eq (Instr f)

-- | The two source registers a decoded instruction depends on. Consumed by
-- 'isaStepDecoded' to supply the operands 'apply' evaluates a 'Func' against.
getR1 :: Instr Func -> Maybe RegIdx
getR1 = fst . deps

getR2 :: Instr Func -> Maybe RegIdx
getR2 = snd . deps

class DepReg a where
  deps :: a -> (Maybe RegIdx, Maybe RegIdx)

instance DepReg (Func a) where
  deps (Func _ d) = d

instance DepReg (Instr Func) where
  deps (Reg _ f) = deps f
  deps (Load _ _ _ f) = deps f
  deps (Jump _ _ f) = deps f
  deps (Store _ f r2) = (fst $ deps f, pure r2)
  deps (Branch f _) = deps f
  deps Break = (empty, empty)
  deps Nop = (empty, empty)
  deps Syscall = (pure 17, empty)

interp :: (Access f) => Input f -> Instr Func
interp input
  | not (inputIsInstr input) = Nop
  | otherwise = interp' $ Instruction.decode' $ unAccess $ inputMem input

interp' :: Instruction.Instruction -> Instr Func
interp' instr
  | Instruction.isBreak instr = Break
  | otherwise =
      case instr of
        Instruction.RType op rd r1 r2 ->
          Reg rd $ binaryF r1 r2 $ \w1 w2 -> unAccess $ alu op (Identity w1) (Identity w2)
        Instruction.IType iop rd r1 imm ->
          let op =
                case iop of
                  Instruction.Arith op' -> op'
                  _ -> Instruction.ADD
              alu_res = unaryF r1 $ \w -> unAccess $ alu op (Identity w) (Identity $ signExtend imm)
           in case iop of
                Instruction.Arith {} ->
                  Reg rd alu_res
                Instruction.Load size sign ->
                  Load size sign rd $ bitCoerce <$> alu_res
                Instruction.Jump ->
                  Jump rd (pcF (bitCoerce . (+ 4))) $ bitCoerce <$> alu_res
                Instruction.Env Instruction.Break ->
                  Break
                Instruction.Env Instruction.Call ->
                  Syscall
        Instruction.SType size imm r1 r2 -> do
          let addr_comp = unpack <$> unaryF r1 (+ signExtend imm)
           in Store size addr_comp r2
        Instruction.BType cmp imm r1 r2 ->
          let branched_comp = binaryF r1 r2 $ \w1 w2 -> unAccess $ branch cmp (Identity w1) (Identity w2)
              addr_comp = pcF (+ bitCoerce (signExtend imm))
           in Branch branched_comp addr_comp
        Instruction.UType Instruction.Zero rd imm ->
          let imm' = imm ++# (0 :: BitVector 12)
           in Reg rd $ constF imm'
        Instruction.UType Instruction.PC rd imm -> do
          let imm' = imm ++# (0 :: BitVector 12)
           in Reg rd $ pcF $ \pc -> bitCoerce pc + imm'
        Instruction.JType rd imm ->
          Jump rd (pcF (bitCoerce . (+ 4))) $ pcF (+ bitCoerce (signExtend imm))
        Instruction.Nop _ -> Nop
  where
    constF :: a -> Func a
    constF a =
      Func
        { isaFunc = const $ const $ const a,
          isaDeps = (empty, empty)
        }

    unaryF :: RegIdx -> (Word -> a) -> Func a
    unaryF rid f =
      Func
        { isaFunc = \r _ _ -> f r,
          isaDeps = (pure rid, empty)
        }

    binaryF :: RegIdx -> RegIdx -> (Word -> Word -> a) -> Func a
    binaryF rid1 rid2 f =
      Func
        { isaFunc = \r1 r2 _ -> f r1 r2,
          isaDeps = (pure rid1, pure rid2)
        }

    pcF :: (PC -> a) -> Func a
    pcF f =
      Func
        { isaFunc = const $ const f,
          isaDeps = (empty, empty)
        }

-- | Architectural state: the ISA-visible triple.
data IsaStateG r m = IsaState
  { isaPc :: PC,
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
    Reg rd f ->
      Next st {isaPc = pc + 4, isaRegFile = modifyRFg rd (pure (ap f)) rf}
    Load size sign rd f ->
      let val = loadExtend size sign (memReadWord (ap f) mem)
       in Next st {isaPc = pc + 4, isaRegFile = modifyRFg rd (pure val) rf}
    Jump rd link target ->
      Next
        st
          { isaPc = ap target,
            isaRegFile = modifyRFg rd (pure (bitCoerce (ap link))) rf
          }
    Store size addr rs2 ->
      Next st {isaPc = pc + 4, isaMem = memWriteWord size (ap addr) (reg (Just rs2)) mem}
    Branch cond target ->
      Next st {isaPc = if ap cond then ap target else pc + 4}
    Nop -> Next st {isaPc = pc + 4}
    Break -> IsaHalted
    Syscall -> IsaHalted
  where
    instr = interp' ir

    reg = maybe 0 (\idx -> runIdentity (lookupRFg idx rf))

    -- 'Func' is evaluated against the values of its two dependency
    -- registers and the PC, exactly as 'apply' prescribes.
    ap :: Func a -> a
    ap f = unDone (apply f (reg (getR1 instr)) (reg (getR2 instr)) pc)

-- | Run at most @n@ architectural steps, stopping early on halt. Returns the
-- states visited, starting with the initial one.
isaRun :: (RegFileOps r, MemOps m) => Int -> IsaStateG r m -> [IsaStateG r m]
isaRun n st
  | n <= 0 = [st]
  | otherwise = case isaStep st of
      IsaHalted -> [st]
      Next st' -> st : isaRun (n - 1) st'
