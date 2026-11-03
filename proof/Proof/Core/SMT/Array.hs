{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | The core's register file embedded as an SMT array, in the pattern
-- "Proof.SMT.Array" describes for memory. The ALU's shifts are embedded in
-- "Stepper.Shift", which the stepper shares.
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
    freshRA,
    freshRAE,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Coerce (coerce)
import qualified SymFC.BuiltIn as P
import SymFC.BuiltIn (Embeddable, Embedding (..), embedding)
import qualified SymFC.Clash as Clash
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
-- Second, the array is indexed by the /primitive/ @SymFC.BuiltIn.BitVec@,
-- not by symfc-clash's @BitVec@. The latter is a GADT admitting
-- zero-width vectors, so it is neither coercible to the primitive nor a
-- 'P.Primitive' itself, and cannot be an array index or element. So we unwrap
-- at the boundary with 'Clash.BitVecP'.
loadRAE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  Embeddable arr RegArrSMT =>
  Embeddable bvI Clash.BitVec =>
  Embeddable bvV Clash.BitVec =>
  arr ->
  bvI 5 ->
  bvV 32
loadRAE
  | Embedding <- embedding @arr
  , Embedding <- embedding @bvI
  , Embedding <- embedding @bvV = coerce go
  where
    go :: RegArrSMT -> Clash.BitVec 5 -> Clash.BitVec 32
    go (RegArrSMT arr) (Clash.BitVecP k) = Clash.BitVecP (P.aselect arr k)

-- | Embedding of 'storeRA' as an array store.
storeRAE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  Embeddable arr RegArrSMT =>
  Embeddable bvI Clash.BitVec =>
  Embeddable bvV Clash.BitVec =>
  arr ->
  bvI 5 ->
  bvV 32 ->
  arr
storeRAE
  | Embedding <- embedding @arr
  , Embedding <- embedding @bvI
  , Embedding <- embedding @bvV = coerce go
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
eqRAE :: forall arr. Embeddable arr RegArrSMT => arr -> arr -> Bool
eqRAE
  | Embedding <- embedding @arr = coerce go
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
zeroRAE :: forall arr. Embeddable arr RegArrSMT => arr
zeroRAE
  | Embedding <- embedding @arr = coerce (RegArrSMT (P.aconst 0))

-- | A fresh symbolic register file.
--
-- A 'Vec' has no symbolic counterpart, so this has no Haskell meaning: the body
-- halts, and 'freshRAE' replaces it under the term axiom. If the axiom ever
-- stops firing, the property halts and the build fails instead of silently
-- checking a concrete register file.
{-# OPAQUE freshRA #-}
freshRA :: P.Sym RegArr
freshRA = P.halt

-- | Embedding of 'freshRA' as a fresh SMT array.
freshRAE :: forall arr. Embeddable arr RegArrSMT => P.Sym arr
freshRAE | Embedding <- embedding @arr = coerce . RegArrSMT <$> P.fresh

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
