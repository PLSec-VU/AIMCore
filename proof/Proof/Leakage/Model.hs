-- | What the core leaks, what an attacker sees, and how to invert one into the
-- other.
--
-- The leakage is the constant-time one: an instruction reveals its control flow
-- (whether a branch is taken, where a jump goes) and, if it accesses memory, the
-- address it accesses -- plus its class and source registers, which are
-- functions of the code and so public under the constant-time discipline. The
-- attacker sees every request the core puts on its memory interface, cycle by
-- cycle: the sequence of accesses a cache in front of the core would receive,
-- and their timing.
--
-- The leakage proof ("Proof.Leakage.Obligation") establishes that this attacker
-- learns nothing beyond 'L'. This module defines the three functions that
-- statement is about:
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
    obsReq,

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
    parkSource,
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
import ISA (IsaStateG (..), isaInstrAt)
import Memory.Types (MemOps (..))
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Observation -----------------------------------------------------------------

-- | What the attacker sees on the memory interface in one cycle: the request,
-- if any, without the data.
--
-- An instruction fetch shows its address -- this is the program-counter trace.
-- A data access shows its kind, address and width, but neither the value read
-- nor the value written.
data Obs
  = NoAccess
  | Fetch Address
  | DataRead Address Size
  | DataWrite Address Size
  deriving (Eq, Show, Generic, NFDataX)

-- | The observations of one driver hop, one slot per cycle.
--
-- A hop is one to four cycles. Four slots rather than a list because Pantomime
-- cannot execute folds; 'Nothing' means the hop was not that long, so hops of
-- different lengths compare unequal.
data HopObs = HopObs (Maybe Obs) (Maybe Obs) (Maybe Obs) (Maybe Obs)
  deriving (Eq, Show, Generic, NFDataX)

-- | The observation a cycle's 'Core.Output' produces: that of its request.
obsOf :: Output Identity -> Obs
obsOf o = obsReq (getFirst (outMem o))

-- | The observation of a request on the bus: the request without its data.
--
-- This is also what a cache in front of the core leaks about each request it
-- receives ("Proof.Cache.Server"), which is what lets the core's leakage proof
-- and the cache's compose. A request carrying a value is a write, whatever its
-- other fields say, as it is to memory ('Proof.Machine.idealServe').
obsReq :: Maybe (MemAccess Identity) -> Obs
obsReq req = case req of
  Nothing -> NoAccess
  Just (MemAccess _ addr size (Just _)) -> DataWrite addr size
  Just (MemAccess True addr _ Nothing) -> Fetch addr
  Just (MemAccess False addr size Nothing) -> DataRead addr size

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
--   * 'CJalr' carries its computed target, and 'CLoad' and 'CStore' their
--     computed data address. These are the data-dependent parts: the
--     constant-time discipline is that they must not depend on secrets.
--   * 'CLoad' also carries its destination register, because a load-use hazard
--     against the next instruction depends on it and that decides how many
--     cycles the next hop takes.
--   * Sizes are carried because 'Obs' shows the access width.
data Class
  = CPlain
  | CBranchTaken BImm
  | CBranchNotTaken
  | CJal JImm
  | CJalr Address
  | CLoad Size RegIdx Address
  | CStore Size Address
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
-- Four things are data dependent -- whether a branch is taken, the computed
-- @jalr@ target, and the load and store addresses -- and are resolved here
-- against the architectural register file. 'coreClass' resolves the same four
-- against forwarded pipeline values, and the proof shows they agree.
isaClass :: (RegFileOps r, MemOps m) => IsaStateG r m -> Instruction -> Class
isaClass (IsaState _ rf _) ir = case ir of
  BType cmp imm rs1 rs2 ->
    if runIdentity (branch cmp (pure (regv rs1)) (pure (regv rs2)))
      then CBranchTaken imm
      else CBranchNotTaken
  JType _ imm -> CJal imm
  IType Jump _ rs1 imm -> CJalr (unpack (regv rs1 + signExtend imm))
  IType (Load size _) rd rs1 imm -> CLoad size rd (unpack (regv rs1 + signExtend imm))
  SType size imm rs1 _ -> CStore size (unpack (regv rs1 + signExtend imm))
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
  IType (Load size _) rd rs1 imm -> CLoad size rd (unpack (exArg sys rs1 + signExtend imm))
  SType size imm rs1 _ -> CStore size (unpack (exArg sys rs1 + signExtend imm))
  IType (Env Call) _ _ _ -> CCall
  IType (Env Break) _ _ _ -> CBreak
  _ -> CPlain

-- Inversion -------------------------------------------------------------------

-- | A representative instruction with the same leakage as the real one.
--
-- The simulator's register file holds zero everywhere except the one slot
-- 'parkSource' reserves, so every instruction here is chosen to behave
-- independently of register contents:
--
--   * @'CBranchTaken' imm@ becomes @beq d1, d2, imm@. Both operands read zero,
--     so it is always taken, and the original immediate takes it to the
--     original target. @'CBranchNotTaken'@ becomes @bne d1, d2, 0@, never taken
--     for the same reason.
--   * @'CJal'@ keeps its immediate, which is PC-relative and needs no register.
--   * @'CJalr'@, @'CLoad'@ and @'CStore'@ address @0(src)@, where @src@ is the
--     real instruction's base register and holds the leaked address; see
--     'parkSource'. When the real instruction's base was @x0@ there is no
--     register to use, but then the address is @0 + signExtend imm@ and fits the
--     immediate exactly.
--
-- Destination registers are @x0@ except for a load, whose destination is
-- leaked because it drives the next hop's hazard. A load into a real register
-- writes @loadExtend size sign 0 == 0@, since the simulator answers every data
-- read with zero.
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
  CJalr t -> based t $ \base imm -> IType Jump 0 base imm
  CLoad size rd a -> based a $ \base imm -> IType (Load size I.Signed) rd base imm
  CStore size a -> based a $ \base imm -> SType size imm base (r d2)
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

    -- Address @a@ as @imm(base)@: from the parked source register, or, with no
    -- source register, from the immediate alone.
    based :: Address -> (RegIdx -> Imm -> Instruction) -> Instruction
    based a mk = case d1 of
      Just src | src /= 0 -> mk src 0
      _ -> mk 0 (slice d11 d0 (pack a))

-- | The word the simulator puts on the bus for 'Core.decode' to read.
--
-- @'Instruction.decode'' . 'invWord'@ is 'inv': every instruction 'inv' emits
-- survives the round trip.
invWord :: L -> Word
invWord l = case encode' (inv l) of
  Just w -> w
  Nothing -> 0

-- | The register a leaked address has to be parked in, and the address.
--
-- No RISC-V instruction word can hold a 32-bit address: the core computes a
-- @jalr@ target and a data address as @register + immediate@, with a 12-bit
-- immediate. The register file is therefore the only place an arbitrary address
-- fits, and the representative 'inv' builds reads it from the real
-- instruction's base register. Writing it costs nothing in leakage: the value is
-- the address, which 'L' already carries.
--
-- 'Nothing' for instructions without an address, and for one based on @x0@,
-- whose address fits the immediate and needs no parking.
parkSource :: L -> Maybe (RegIdx, Address)
parkSource (L cls (Just src, _))
  | src /= 0 = case cls of
      CJalr t -> Just (src, t)
      CLoad _ _ a -> Just (src, a)
      CStore _ a -> Just (src, a)
      _ -> Nothing
parkSource _ = Nothing
