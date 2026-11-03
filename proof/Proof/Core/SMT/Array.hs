{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | The core's register file embedded as an SMT array, in the pattern
-- "Proof.SMT.Array" describes for memory, and the ALU's shifts
-- ('Core.Alu.sllWord' and friends) as SMT bitvector shifts.
module Proof.Core.SMT.Array
  ( RegArr (..),
    RegArrF (..),
    RegArrSMT (..),
    loadRA,
    storeRA,
    zeroRA,
    zeroRAE,
    loadRAE,
    storeRAE,
    eqRA,
    eqRAE,
    sllWordE,
    srlWordE,
    sraWordE,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Coerce (Coercible, coerce)
import qualified Pantomime.BuiltIn as P
import qualified Pantomime.Clash as Clash
import Core.RegFile (RegFileOps (..))
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The register file as an SMT array -------------------------------------------

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

-- | Equality, so that whole register files can be compared. Replaced by 'eqRAE'
-- under the term axiom; OPAQUE for the same reason as 'loadRA'.
{-# OPAQUE eqRA #-}
eqRA :: RegArr -> RegArr -> Bool
eqRA (RegArr v) (RegArr w) = v == w

instance Eq RegArr where
  (==) = eqRA

-- | Embedding of 'eqRA' as array equality.
eqRAE :: forall arr. (Coercible RegArrSMT arr) => arr -> arr -> Bool
eqRAE = coerce go
  where
    go :: RegArrSMT -> RegArrSMT -> Bool
    go (RegArrSMT a) (RegArrSMT b) = a == b

-- | The all-zero register file.
--
-- 'Proof.Core.Leakage.Simulator.censor' installs this on every state it builds,
-- so unlike 'Core.Pipeline.init' it is on the symbolic path and has to be
-- embeddable. 'Clash.Prelude.repeat' gives the Haskell semantics; 'zeroRAE'
-- replaces it under the term axiom, which is keyed on this name -- hence
-- OPAQUE, for the same reason as 'loadRA'.
{-# OPAQUE zeroRA #-}
zeroRA :: RegArr
zeroRA = RegArr (Clash.Prelude.repeat 0)

-- | Embedding of 'zeroRA' as a constant array.
zeroRAE :: forall arr. (Coercible RegArrSMT arr) => arr
zeroRAE = coerce (RegArrSMT (P.aconst 0))

-- | The array-backed register file, wrapped so it fits 'RegFileOps'. A
-- verification-only representation.
newtype RegArrF = RegArrF RegArr
  deriving (Eq)

instance RegFileOps RegArrF where
  lookupRFg idx (RegArrF a) = if idx == 0 then 0 else loadRA a idx
  modifyRFg idx v rf@(RegArrF a)
    | idx == 0 = rf
    | otherwise = RegArrF (storeRA a idx v)

  -- On the symbolic path; see 'zeroRA'.
  initRFg = RegArrF zeroRA

-- Shifts as SMT bitvector shifts ----------------------------------------------
--
-- Embeddings for 'Core.Alu.sllWord', 'Core.Alu.srlWord' and
-- 'Core.Alu.sraWord'. See the note on those functions for why they exist:
-- without them each shift site converts its five-bit amount through 'Integer',
-- and the resulting
-- @ubv_to_int@ / @int_to_bv@ pair makes every query unreadable to the
-- bitvector-only solvers.
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
