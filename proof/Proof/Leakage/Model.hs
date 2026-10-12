-- | What an attacker sees, and what one architectural instruction leaks.
--
-- The leakage proof ("Proof.Leakage.Obligation") establishes that an attacker
-- watching the memory bus learns nothing beyond 'Leak'. Two definitions carry
-- that statement:
--
--   * 'obsOf' -- what the attacker sees in one cycle: the kind of access, its
--     address, and for a data access its width.
--   * 'leakOf' -- what the instruction an architectural state is about to
--     process leaks. A function of the ISA state alone; the pipeline does not
--     appear in it.
--
-- 'Leak' follows the observation rather than the instruction set. Control flow
-- shows up only as the next fetch address, so a taken branch and a jump are the
-- same thing ('LJump' with the target) and an untaken branch is
-- indistinguishable from arithmetic ('LArith'). A load or store shows its data
-- address; a load also carries whether the instruction at @pc + 4@ has a
-- load-use hazard against it, which is the one thing the fetch trace does not
-- already give away, since it decides how long the hop takes.
--
-- == What 'Leak' is, and is not
--
-- It is not a list of secrets. Every field is public in any run that does not
-- halt, because 'Core.execute' computes each one inside @noSecrets'@: the
-- branch condition, the @jalr@ target, and the load and store addresses. The
-- fetched instruction word goes through @noSecrets'@ in 'Core.decode' too, so
-- the program is public even under self-modifying code -- which matters, because
-- store /data/ is not forced public, so a store could otherwise plant a
-- secret-dependent instruction. A secret reaching any of these halts the core
-- with 'Core.SecurityViolation'. The hazard bit is not even data dependent:
-- 'Instruction.loadHazard' compares register /fields/, not their contents.
--
-- So 'Leak' is what the /simulator/ needs in order to replay the observation
-- from the architectural state alone, not what escapes. The security content
-- lives in the @Access@ / @noSecrets@ discipline.
module Proof.Leakage.Model
  ( -- * Observation
    Obs (..),
    obsOf,

    -- * Leakage
    Leak (..),
    leakOf,
    isJumpLeak,
    isMemLeak,
    isEnvLeak
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Instruction
import ISA (IsaStateG (..), isaInstrAt)
import Memory.Types (MemOps (..))
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Observation -----------------------------------------------------------------

-- | What the attacker sees on the memory bus in one cycle: the kind of access,
-- its address, and for a data access its width.
data Obs
  = NoAccess
  | Fetch Address
  | DataRead Address Size
  | DataWrite Address Size
  deriving (Eq, Show, Generic, NFDataX)

instance Semigroup Obs where
  NoAccess <> obs' = obs'
  obs <> _ = obs

instance Monoid Obs where
  mempty = NoAccess

-- | The observation a cycle's 'Core.Output' produces.
obsOf :: Output Identity -> Obs
obsOf o = case outMem o of
  Nothing -> NoAccess
  Just (MemAccess True addr _ _) -> Fetch addr
  Just (MemAccess False addr size Nothing) -> DataRead addr size
  Just (MemAccess False addr size (Just _)) -> DataWrite addr size

-- Leakage ---------------------------------------------------------------------

-- | What one architectural instruction leaks.
--
-- The constructors are named for what reaches the observation, not for the
-- instruction set:
--
--   * 'LArith' -- nothing beyond the fetch trace. No data access, and the next
--     fetch is @pc + 4@. Arithmetic, upper immediates, an untaken branch, and an
--     undecodable word all land here, and are indistinguishable.
--   * 'LJump' -- control goes somewhere other than @pc + 4@, and the target is
--     the next fetch address. A taken branch, @jal@ and @jalr@ are one case: the
--     attacker sees an address, not which instruction produced it.
--   * 'LLoad' -- a data read of the given width at the given address, plus
--     whether the instruction at @pc + 4@ has a load-use hazard against this
--     load. That bit is the one thing here not already in the fetch trace: it
--     decides how many cycles the hop takes, and the timing is observable.
--   * 'LStore' -- a data write of the given width at the given address. The
--     value written is not observable and is not carried.
--   * 'LEnv' -- @ecall@ or @ebreak@. The core halts, so no fetch follows.
data Leak
  = LArith
  | LJump Address
  | LLoad Size Address Bool
  | LStore Size Address
  | LEnv
  deriving (Eq, Show, Generic, NFDataX)

isJumpLeak :: Leak -> Bool
isJumpLeak (LJump _) = True
isJumpLeak _ = False

isMemLeak :: Leak -> Bool
isMemLeak (LLoad {}) = True
isMemLeak (LStore {}) = True
isMemLeak _ = False

isEnvLeak :: Leak -> Bool
isEnvLeak LEnv = True
isEnvLeak _ = False

-- | The leakage of the instruction an architectural state is about to process.
leakOf :: (RegFileOps r, MemOps m) => IsaStateG r m -> Leak
leakOf isa@(IsaState pc rf _) =
  case ir of
    RType {} -> LArith
    IType (Arith _) _ _ _ -> LArith
    UType {} -> LArith
    BType cmp imm rs1 rs2 ->
      let doBranch = runIdentity $ branch cmp (pure (regv rs1)) (pure (regv rs2))
      in if doBranch
           then LJump (pc + off imm)
           else LArith
    JType _ imm ->
      LJump (pc + off imm)
    IType Jump _ rs1 imm ->
      LJump (addr rs1 imm)
    IType (Load size _) _ rs1 imm ->
      let hazard = loadHazard (isaInstrAt isa {isaPc = pc + 4}) ir
      in LLoad size (addr rs1 imm) hazard
    SType size imm rs1 _ ->
      LStore size (addr rs1 imm)
    IType (Env _) _ _ _ -> LEnv
    -- An undecodable word decodes to @Nop DecodeFail@, which is a real
    -- architectural instruction and leaks like any other non-memory one.
    Nop _ -> LArith
  where
    ir = isaInstrAt isa
    regv i = runIdentity (lookupRFg i rf)
    off imm = unpack $ signExtend imm
    addr rs1 imm = unpack $ regv rs1 + signExtend imm
