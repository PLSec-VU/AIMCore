-- | What the core leaks, what an attacker sees, and how to invert one into the
-- other.
--
-- The leakage proof ("Proof.Leakage.Obligation") establishes that an attacker
-- watching the memory bus learns nothing beyond 'L'. This module defines the
-- three functions that statement is about:
--
--   * 'obsOf' -- what the attacker sees in one cycle.
--   * 'leakOf' -- what one architectural instruction leaks. A function of the
--     ISA state alone; the pipeline does not appear in it.
--   * 'inv' -- a representative instruction with the same leakage as the real
--     one. The simulator runs the unmodified core on these, so anything 'inv'
--     cannot express is something the proof cannot cover.
module Proof.Leakage.Model
  ( -- * Observation
    Obs (..),
    HopObs (..),
    obsOf,

    -- * Leakage
    Class (..),
    L (..),
    leakOf,
    mkDeps,
    isaClass,
    coreClass,

    -- * Inversion
    inv,
    invWord,
    jumpSource,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Monoid (getFirst)
import Instruction
import qualified Instruction as I
import Proof.Driver (exArg)
import Proof.ISAStep
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Observation -----------------------------------------------------------------

-- | What the attacker sees on the memory bus in one cycle.
--
-- An instruction fetch shows its address -- this is the program-counter trace.
-- A data access shows only its kind and width: neither the address nor the
-- value.
--
-- Data addresses are excluded because the simulator could not reproduce them.
-- A load or store computes its address as @register + immediate@, and 'inv'
-- emits instructions that read a censored register file; a jump target escapes
-- that limit only because 'jumpSource' parks it in a register first, which
-- works for at most one instruction per hop.
data Obs
  = NoAccess
  | Fetch Address
  | DataRead Size
  | DataWrite Size
  deriving (Eq, Show, Generic, NFDataX)

-- | The observations of one driver hop, one slot per cycle.
--
-- A hop is one to four cycles. Four slots rather than a list because Pantomime
-- cannot execute folds; 'Nothing' means the hop was not that long, so hops of
-- different lengths compare unequal.
data HopObs = HopObs (Maybe Obs) (Maybe Obs) (Maybe Obs) (Maybe Obs)
  deriving (Eq, Show, Generic, NFDataX)

-- | The observation a cycle's 'Core.Output' produces.
obsOf :: Output Identity -> Obs
obsOf o = case getFirst (outMem o) of
  Nothing -> NoAccess
  Just (MemAccess True addr _ _) -> Fetch addr
  Just (MemAccess False _ size Nothing) -> DataRead size
  Just (MemAccess False _ size (Just _)) -> DataWrite size

-- Leakage ---------------------------------------------------------------------

-- | The class of one architectural instruction: what the attacker learns about
-- it beyond its own existence.
--
-- Each constructor carries what the simulator needs and no more:
--
--   * 'CBranchTaken' and 'CJal' carry the instruction's own immediate, a field
--     of the instruction word rather than a function of register contents.
--     'CBranchNotTaken' carries nothing, since an untaken branch's target is
--     not observable.
--   * 'CJalr' carries a computed address. This one is genuinely data
--     dependent -- it is the leak.
--   * 'CLoad' carries its destination register, because a load-use hazard
--     against the next instruction depends on it and that decides how many
--     cycles the next hop takes. It carries no address.
--   * Sizes are carried because 'Obs' shows the access width.
data Class
  = CPlain
  | CBranchTaken BImm
  | CBranchNotTaken
  | CJal JImm
  | CJalr Address
  | CLoad Size RegIdx
  | CStore Size
  | CCall
  | CBreak
  deriving (Eq, Show, Generic, NFDataX)

-- | An instruction's class together with the source registers it may depend on.
--
-- The dependencies are leaked for every instruction, not only those that use
-- them: 'Core.decode' compares them against the destination register of a load
-- in the execute stage, and that comparison decides the hop length.
data L = L
  { lClass :: Class,
    lDeps :: (Maybe RegIdx, Maybe RegIdx)
  }
  deriving (Eq, Show, Generic, NFDataX)

-- | An instruction's source registers, with @x0@ dropped -- reading @x0@ can
-- never be a hazard. Matches the filter in 'Instruction.loadHazard', which is
-- the consumer that matters.
--
-- Note this reads the registers through 'Instruction.getRs1' and
-- 'Instruction.getRs2', which do not always answer with the encoded fields:
-- @ecall@ reports @x17@ whatever is encoded, and a @Nop@ reports @x0@.
mkDeps :: Instruction -> (Maybe RegIdx, Maybe RegIdx)
mkDeps ir = (noZero (getRs1 ir), noZero (getRs2 ir))
  where
    noZero (Just 0) = Nothing
    noZero r = r

-- | The leakage of the instruction an architectural state is about to process.
leakOf :: (RegFileOps r, MemOps m) => IsaStateG r m -> L
leakOf isa = L (isaClass isa ir) (mkDeps ir)
  where
    ir = isaInstrAt isa

-- | Classify an instruction against the architectural state it runs in.
--
-- Three decisions are data dependent -- whether a branch is taken, and the two
-- computed jump targets -- and are resolved here against the architectural
-- register file. 'coreClass' resolves the same three against forwarded pipeline
-- values, and the proof shows they agree.
isaClass :: (RegFileOps r, MemOps m) => IsaStateG r m -> Instruction -> Class
isaClass (IsaState _ rf _) ir = case ir of
  BType cmp imm rs1 rs2 ->
    if runIdentity (branch cmp (pure (regv rs1)) (pure (regv rs2)))
      then CBranchTaken imm
      else CBranchNotTaken
  JType _ imm -> CJal imm
  IType Jump _ rs1 imm -> CJalr (unpack (regv rs1 + signExtend imm))
  IType (Load size _) rd _ _ -> CLoad size rd
  SType size _ _ _ -> CStore size
  IType (Env Call) _ _ _ -> CCall
  IType (Env Break) _ _ _ -> CBreak
  _ -> CPlain
  where
    regv i = runIdentity (lookupRFg i rf)

-- | Classify the execute-stage instruction against a core state.
--
-- The mirror of 'isaClass'. Operands are read through 'Proof.Driver.exArg',
-- which reproduces the forwarding priority 'Core.execute' uses.
coreClass :: (RegFileOps r) => SysG r m -> Instruction -> Class
coreClass sys ir = case ir of
  BType cmp imm rs1 rs2 ->
    if runIdentity (branch cmp (pure (exArg sys rs1)) (pure (exArg sys rs2)))
      then CBranchTaken imm
      else CBranchNotTaken
  JType _ imm -> CJal imm
  IType Jump _ rs1 imm -> CJalr (unpack (exArg sys rs1 + signExtend imm))
  IType (Load size _) rd _ _ -> CLoad size rd
  SType size _ _ _ -> CStore size
  IType (Env Call) _ _ _ -> CCall
  IType (Env Break) _ _ _ -> CBreak
  _ -> CPlain

-- Inversion -------------------------------------------------------------------

-- | A representative instruction with the same leakage as the real one.
--
-- The simulator's register file holds zero everywhere except the one slot
-- 'jumpSource' reserves, so every instruction here is chosen to behave
-- independently of register contents:
--
--   * @'CBranchTaken' imm@ becomes @beq d1, d2, imm@. Both operands read zero,
--     so it is always taken, and the original immediate takes it to the
--     original target. @'CBranchNotTaken'@ becomes @bne d1, d2, 0@, never taken
--     for the same reason.
--   * @'CJal'@ keeps its immediate, which is PC-relative and needs no register.
--   * @'CJalr'@ addresses @0(src)@ and reads its target out of @src@; see
--     'jumpSource'. When the real @jalr@ read @x0@ there is no register to use,
--     but then the target is @0 + signExtend imm@ and fits the immediate
--     exactly.
--   * @'CLoad'@ and @'CStore'@ address @0(rs1)@, i.e. address zero. The address
--     is not observable; the width and the fact that it is a memory instruction
--     are, and both survive.
--
-- Destination registers are @x0@ except for a load, whose destination is
-- leaked because it drives the next hop's hazard. A load into a real register
-- writes @loadExtend size sign 0 == 0@, so the register file stays zero.
--
-- Source registers are threaded through everywhere so that a load-use hazard
-- against this instruction fires in the simulator exactly when it fires in the
-- core.
inv :: L -> Instruction
inv (L cls (d1, d2)) = case cls of
  CPlain -> RType ADD 0 (r d1) (r d2)
  CBranchTaken imm -> BType EQ imm (r d1) (r d2)
  CBranchNotTaken -> BType NE 0 (r d1) (r d2)
  CJal imm -> JType 0 imm
  CJalr t -> case d1 of
    Just src | src /= 0 -> IType Jump 0 src 0
    _ -> IType Jump 0 0 (slice d11 d0 (pack t))
  CLoad size rd -> IType (Load size I.Signed) rd (r d1) 0
  CStore size -> SType size 0 (r d1) (r d2)
  CCall -> IType (Env Call) 0 0 0
  -- The immediate is the opcode rather than a value: 'Instruction.decode' reads
  -- @ecall@ off @immI == 0@ and @ebreak@ off @immI == 1@ and re-emits it, so a
  -- zero here would not survive 'invWord'.
  --
  -- Unlike @ecall@, an @ebreak@ reports its encoded @rs1@ from
  -- 'Instruction.getRs1', so that field is a real dependency and has to be
  -- threaded through like any other.
  CBreak -> IType (Env Break) 0 (r d1) 1
  where
    r = fromMaybe 0

-- | The word the simulator puts on the bus for 'Core.decode' to read.
--
-- @'Instruction.decode'' . 'invWord'@ is 'inv': every instruction 'inv' emits
-- survives the round trip.
invWord :: L -> Word
invWord l = case encode' (inv l) of
  Just w -> w
  Nothing -> 0

-- | The register a leaked @jalr@ target has to be parked in, and the target.
--
-- No RISC-V instruction word can hold a 32-bit jump target: every target the
-- core computes is @PC + immediate@ (13 bits for a branch, 21 for @jal@) or
-- @register + immediate@ (12 bits for @jalr@). The register file is therefore
-- the only place an arbitrary target fits, and @jalr@ is the one instruction
-- that reads it from there. Writing it costs nothing in leakage: the value is
-- the target, which 'L' already carries.
--
-- 'Nothing' for everything else, including a @jalr@ off @x0@, where the target
-- fits the immediate and nothing needs parking.
jumpSource :: L -> Maybe (RegIdx, Address)
jumpSource (L (CJalr t) (Just src, _)) | src /= 0 = Just (src, t)
jumpSource _ = Nothing
