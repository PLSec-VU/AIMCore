{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

-- | A register file embedded as an SMT array.
--
-- The embedding has to be /monomorphic/: an SMT array needs concrete index and
-- element sorts, so there is no way to map @Vec n a@ polymorphically. The
-- pattern is therefore:
--
--   * a monomorphic newtype ('RegArr') for the Haskell side,
--   * a matching newtype ('RegArrSMT') wrapping the SMT array,
--   * a type axiom relating the two,
--   * monomorphic wrappers ('loadRA', 'storeRA') that the verified code calls
--     instead of Clash's polymorphic @(!!)@ and @replace@, each with a term
--     axiom pointing at an embedding written with 'coerce'.
--
-- The polymorphic Clash operations must never appear in verified code -- they
-- are @OPAQUE@ and cannot be embedded. They appear here only inside the
-- Haskell implementations, which the axioms replace before symbolic execution
-- ever sees them.
module Proof.SMT.Array
  ( RegArr (..),
    RegArrF (..),
    MemArr (..),
    MemArrSMT (..),
    loadM,
    storeM,
    loadME,
    storeME,
    RegArrSMT (..),
    loadRA,
    storeRA,
    zeroRA,
    zeroRAE,
    loadRAE,
    storeRAE,
    sllWordE,
    srlWordE,
    sraWordE,
  )
where

import Access
import qualified Clash.Prelude
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import qualified Pantomime.BuiltIn as P
import qualified Pantomime.Clash as Clash
import Proof.Machine (MemOps (..))
import Memory.Types (MEM_SIZE_BYTES)
import RegFile (RegFileOps (..))
import Types
import qualified Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The Haskell-side register file: 32 entries, so the index is exactly a
-- 'RegIdx' with no offset arithmetic. Slot 0 is present but never used --
-- @x0@ is handled by the caller, outside the array operations.
newtype RegArr = RegArr (Vec 32 Word)

-- | The SMT-side counterpart. Same arity (zero), which is what lets the type
-- axiom relate them.
newtype RegArrSMT = RegArrSMT (P.Array (P.BitVec 5) (P.BitVec 32))

-- | Read. Replaced by 'loadRAE' under the term axiom.
--
-- OPAQUE is essential: the axiom is keyed on this /name/, so if GHC inlines
-- the wrapper then only the polymorphic @index_int@ survives into Core and the
-- axiom never fires.
{-# OPAQUE loadRA #-}
loadRA :: RegArr -> RegIdx -> Word
loadRA (RegArr v) i = v !! i

-- | Write. Replaced by 'storeRAE' under the term axiom. OPAQUE for the same
-- reason as 'loadRA'.
{-# OPAQUE storeRA #-}
storeRA :: RegArr -> RegIdx -> Word -> RegArr
storeRA (RegArr v) i x = RegArr (replace i x v)

-- | Embedding of 'loadRA' as an array select.
--
-- Two subtleties. First, the source types are type /variables/ with
-- constructor-level 'Coercible' givens: congruence lifts @Coercible f g@ to
-- @Coercible (f a) (g a)@ when the head is a variable, but not through a
-- concrete constructor with nominal parameter roles.
--
-- Second, the array is indexed by the /primitive/ @Pantomime.BuiltIn.BitVec@,
-- not by pantomime-clash's @BitVec@. The latter is a GADT admitting
-- zero-width vectors, so it is neither coercible to the primitive nor a
-- 'P.Primitive' itself, and cannot be an array index or element. So we unwrap
-- at the boundary with 'Clash.BitVecP'.
loadRAE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible RegArrSMT arr) =>
  (Coercible Clash.BitVec bvI) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvI 5 ->
  bvV 32
loadRAE = coerce go
  where
    go :: RegArrSMT -> Clash.BitVec 5 -> Clash.BitVec 32
    go (RegArrSMT arr) (Clash.BitVecP k) = Clash.BitVecP (P.aselect arr k)

-- | Embedding of 'storeRA' as an array store.
storeRAE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible RegArrSMT arr) =>
  (Coercible Clash.BitVec bvI) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvI 5 ->
  bvV 32 ->
  arr
storeRAE = coerce go
  where
    go :: RegArrSMT -> Clash.BitVec 5 -> Clash.BitVec 32 -> RegArrSMT
    go (RegArrSMT arr) (Clash.BitVecP k) (Clash.BitVecP v) =
      RegArrSMT (P.astore arr k v)

-- | The all-zero register file.
--
-- 'Proof.Leakage.Simulator.censor' installs this on every state it builds, so
-- unlike 'Core.init' it is on the symbolic path and has to be embeddable.
-- 'Clash.Prelude.repeat' gives the Haskell semantics; 'zeroRAE' replaces it
-- under the term axiom, which is keyed on this name -- hence OPAQUE, for the
-- same reason as 'loadRA'.
{-# OPAQUE zeroRA #-}
zeroRA :: RegArr
zeroRA = RegArr (Clash.Prelude.repeat 0)

-- | Embedding of 'zeroRA' as a constant array.
zeroRAE :: forall arr. (Coercible RegArrSMT arr) => arr
zeroRAE = coerce (RegArrSMT (P.aconst 0))

-- | The array-backed register file, wrapped so it fits 'RegFileOps'.
--
-- The @f@ parameter is phantom: the embedding must be monomorphic, so the
-- element type is fixed to 'Word'. This is a verification-only representation
-- and is used at @f ~ Identity@, where 'unAccess' is the identity.
newtype RegArrF (f :: Type -> Type) = RegArrF RegArr

instance RegFileOps RegArrF where
  lookupRFg idx (RegArrF a) = if idx == 0 then pure 0 else pure (loadRA a idx)
  modifyRFg idx v rf@(RegArrF a)
    | idx == 0 = rf
    | otherwise = RegArrF (storeRA a idx (unAccess v))

  -- On the symbolic path; see 'zeroRA'.
  initRFg = RegArrF zeroRA

-- Shifts as SMT bitvector shifts ----------------------------------------------
--
-- Embeddings for 'Core.sllWord', 'Core.srlWord' and 'Core.sraWord'. See the note
-- on those functions for why they exist: without them each shift site converts
-- its five-bit amount through 'Integer', and the resulting @ubv_to_int@ /
-- @int_to_bv@ pair makes every query unreadable to the bitvector-only solvers.
--
-- The amount is zero-extended to the word width because SMT's shifts require
-- both operands to have the same sort. That is exact rather than a choice: the
-- amount is five bits, so it is always less than 32 and no shift saturates.

-- | @shiftL@ on a 'Word' as @bvshl@.
sllWordE ::
  forall (bvX :: Nat -> Type) (bvN :: Nat -> Type).
  (Coercible Clash.BitVec bvX) =>
  (Coercible Clash.BitVec bvN) =>
  bvX 32 ->
  bvN 5 ->
  bvX 32
sllWordE = coerce go
  where
    go :: Clash.BitVec 32 -> Clash.BitVec 5 -> Clash.BitVec 32
    go (Clash.BitVecP x) (Clash.BitVecP n) =
      Clash.BitVecP (P.bvshl x (P.bvzext n))

-- | @shiftR@ on a 'Word' as @bvlshr@.
srlWordE ::
  forall (bvX :: Nat -> Type) (bvN :: Nat -> Type).
  (Coercible Clash.BitVec bvX) =>
  (Coercible Clash.BitVec bvN) =>
  bvX 32 ->
  bvN 5 ->
  bvX 32
srlWordE = coerce go
  where
    go :: Clash.BitVec 32 -> Clash.BitVec 5 -> Clash.BitVec 32
    go (Clash.BitVecP x) (Clash.BitVecP n) =
      Clash.BitVecP (P.bvlshr x (P.bvzext n))

-- | Arithmetic @shiftR@ on a 'Word' as @bvashr@.
sraWordE ::
  forall (bvX :: Nat -> Type) (bvN :: Nat -> Type).
  (Coercible Clash.BitVec bvX) =>
  (Coercible Clash.BitVec bvN) =>
  bvX 32 ->
  bvN 5 ->
  bvX 32
sraWordE = coerce go
  where
    go :: Clash.BitVec 32 -> Clash.BitVec 5 -> Clash.BitVec 32
    go (Clash.BitVecP x) (Clash.BitVecP n) =
      Clash.BitVecP (P.bvashr x (P.bvzext n))

-- Memory as an SMT array ------------------------------------------------------

-- | The Haskell-side memory.
--
-- A 'Vec', mirroring 'RegArr'. The size is irrelevant to verification: the type
-- axiom maps this to an array over the whole 32-bit address space, and the body
-- is never symbolically executed, because 'loadM' and 'storeM' are replaced by
-- their embeddings first. A function would arguably be the more honest Haskell
-- representation, since memory spans 32 bits; whether one would also work here
-- is untested.
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

instance MemOps MemArr where
  memReadByte a m = loadM m a

  memReadWord a m =
    loadM m (a + 3) ++# loadM m (a + 2) ++# loadM m (a + 1) ++# loadM m a

  memWriteWord size a w m =
    case size of
      Types.Byte -> put a b0 m
      Types.Half -> put (a + 1) b1 (put a b0 m)
      Types.Word -> put (a + 3) b3 (put (a + 2) b2 (put (a + 1) b1 (put a b0 m)))
    where
      b0 = slice d7 d0 w
      b1 = slice d15 d8 w
      b2 = slice d23 d16 w
      b3 = slice d31 d24 w
      put i v mm = storeM mm i v
