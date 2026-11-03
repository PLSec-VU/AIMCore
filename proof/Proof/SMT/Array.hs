{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | Memory embedded as an SMT array, shared by every proof, and the
-- conversion of 'Unsigned' to 'Integer' that pantomime-clash leaves out. The
-- core's register file is embedded the same way in "Proof.Core.SMT.Array".
--
-- The embedding has to be /monomorphic/: an SMT array needs concrete index and
-- element sorts, so there is no way to map @Vec n a@ polymorphically. The
-- pattern is therefore:
--
--   * a monomorphic newtype ('MemArr') for the Haskell side,
--   * a matching newtype ('MemArrSMT') wrapping the SMT array,
--   * a type axiom relating the two,
--   * monomorphic wrappers ('loadM', 'storeM') that the verified code calls
--     instead of Clash's polymorphic @(!!)@ and @replace@, each with a term
--     axiom pointing at an embedding written with 'coerce'.
--
-- The polymorphic Clash operations must never appear in verified code -- they
-- are @OPAQUE@ and cannot be embedded. They appear here only inside the
-- Haskell implementations, which the axioms replace before symbolic execution
-- ever sees them.
module Proof.SMT.Array
  ( MemArr (..),
    MemArrSMT (..),
    loadM,
    storeM,
    loadME,
    storeME,
    eqM,
    eqME,
    unsignedToIntegerE,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Coerce (Coercible, coerce)
import qualified Pantomime.BuiltIn as P
import qualified Pantomime.Clash as Clash
import Memory.Types (MEM_SIZE_BYTES, MemOps (..))
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Memory as an SMT array ------------------------------------------------------

-- | The Haskell-side memory.
--
-- A 'Vec', mirroring 'Proof.Core.SMT.Array.RegArr'. The size is irrelevant to
-- verification: the type axiom maps this to an array over the whole 32-bit
-- address space, and the body is never symbolically executed, because 'loadM'
-- and 'storeM' are replaced by their embeddings first. A function would
-- arguably be the more honest Haskell representation, since memory spans 32
-- bits; whether one would also work here is untested.
newtype MemArr = MemArr (Vec MEM_SIZE_BYTES Byte)

-- | The SMT-side counterpart: byte-granular, so sub-word stores stay natural.
newtype MemArrSMT = MemArrSMT (P.Array (P.BitVec 32) (P.BitVec 8))

-- | Byte read. OPAQUE so the term axiom, which is keyed on this name, still
-- has a name to fire on after optimisation.
{-# OPAQUE loadM #-}
loadM :: MemArr -> Address -> Byte
loadM (MemArr v) a = v !! a

-- | Byte write. OPAQUE for the same reason.
{-# OPAQUE storeM #-}
storeM :: MemArr -> Address -> Byte -> MemArr
storeM (MemArr v) a b = MemArr (replace a b v)

loadME ::
  forall arr (bvA :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible MemArrSMT arr) =>
  (Coercible Clash.BitVec bvA) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvA 32 ->
  bvV 8
loadME = coerce go
  where
    go :: MemArrSMT -> Clash.BitVec 32 -> Clash.BitVec 8
    go (MemArrSMT arr) (Clash.BitVecP a) = Clash.BitVecP (P.aselect arr a)

storeME ::
  forall arr (bvA :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible MemArrSMT arr) =>
  (Coercible Clash.BitVec bvA) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvA 32 ->
  bvV 8 ->
  arr
storeME = coerce go
  where
    go :: MemArrSMT -> Clash.BitVec 32 -> Clash.BitVec 8 -> MemArrSMT
    go (MemArrSMT arr) (Clash.BitVecP a) (Clash.BitVecP v) =
      MemArrSMT (P.astore arr a v)

-- | Equality, so that whole memories can be compared. Replaced by 'eqME' under
-- the term axiom; OPAQUE for the same reason as 'loadM'.
{-# OPAQUE eqM #-}
eqM :: MemArr -> MemArr -> Bool
eqM (MemArr v) (MemArr w) = v == w

instance Eq MemArr where
  (==) = eqM

-- | Embedding of 'eqM' as array equality.
eqME :: forall arr. (Coercible MemArrSMT arr) => arr -> arr -> Bool
eqME = coerce go
  where
    go :: MemArrSMT -> MemArrSMT -> Bool
    go (MemArrSMT a) (MemArrSMT b) = a == b

instance MemOps MemArr where
  memReadByte a m = loadM m a

  memReadWord a m =
    loadM m (a + 3) ++# loadM m (a + 2) ++# loadM m (a + 1) ++# loadM m a

  memWrite a size w m =
    case size of
      Types.Byte -> put a b0 m
      Types.Half -> put (a + 1) b1 (put a b0 m)
      Types.Word ->
        put (a + 3) b3 (put (a + 2) b2 (put (a + 1) b1 (put a b0 m)))
    where
      b0 = slice d7 d0 w
      b1 = slice d15 d8 w
      b2 = slice d23 d16 w
      b3 = slice d31 d24 w
      put i v mm = storeM mm i v

-- Unsigned to Integer ---------------------------------------------------------
--
-- pantomime-clash embeds @toInteger@ on 'BitVector' but not on 'Unsigned', and
-- Clash's primitive is @OPAQUE@, so even a literal such as
-- 'Memory.Cache4KServer.penalty' converted with 'fromIntegral' stops symbolic
-- execution. Both types are embedded as 'Clash.BitVec', so the embedding is
-- the same. On a constant, the solver library folds the conversion away, so
-- the query stays in the bitvector fragment.

-- | @toInteger@ on an 'Unsigned' as @ubv_to_int@.
unsignedToIntegerE ::
  forall (bv :: Nat -> Type) (n :: Nat).
  (Coercible Clash.BitVec bv) =>
  bv n ->
  Integer
unsignedToIntegerE = coerce go
  where
    go :: Clash.BitVec n -> Integer
    go Clash.BitVecZ = 0
    go (Clash.BitVecP x) = P.toInteger (P.bvu2i x)
