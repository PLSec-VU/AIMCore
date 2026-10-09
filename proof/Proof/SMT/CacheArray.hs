{-# LANGUAGE KindSignatures #-}

-- | The arrays of "Memory.Cache4K" embedded as SMT arrays, as "Proof.SMT.Array"
-- embeds the register file and memory: a monomorphic SMT newtype per array, and
-- an embedding per wrapper, which the term axioms in "Proof.SMT.CacheAxioms"
-- put in place of the wrapper.
module Proof.SMT.CacheArray
  ( TagArrSMT (..),
    tagReadE,
    tagWriteE,
    tagEmptyE,
    DataArrSMT (..),
    dataReadE,
    dataWriteE,
    dataZeroE,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Coerce (Coercible, coerce)
import qualified Pantomime.BuiltIn as P
import qualified Pantomime.Clash as Clash
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | 'Memory.Cache4K.TagArr' on the SMT side.
newtype TagArrSMT = TagArrSMT (P.Array (P.BitVec 8) (P.BitVec 21))

tagReadE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible TagArrSMT arr) =>
  (Coercible Clash.BitVec bvI) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvI 8 ->
  bvV 21
tagReadE = coerce go
  where
    go :: TagArrSMT -> Clash.BitVec 8 -> Clash.BitVec 21
    go (TagArrSMT arr) (Clash.BitVecP k) = Clash.BitVecP (P.aselect arr k)

tagWriteE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible TagArrSMT arr) =>
  (Coercible Clash.BitVec bvI) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvI 8 ->
  bvV 21 ->
  arr
tagWriteE = coerce go
  where
    go :: TagArrSMT -> Clash.BitVec 8 -> Clash.BitVec 21 -> TagArrSMT
    go (TagArrSMT arr) (Clash.BitVecP k) (Clash.BitVecP v) = TagArrSMT (P.astore arr k v)

tagEmptyE :: forall arr. (Coercible TagArrSMT arr) => arr
tagEmptyE = coerce (TagArrSMT (P.aconst 0))

-- | 'Memory.Cache4K.DataArr' on the SMT side.
newtype DataArrSMT = DataArrSMT (P.Array (P.BitVec 10) (P.BitVec 32))

dataReadE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible DataArrSMT arr) =>
  (Coercible Clash.BitVec bvI) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvI 10 ->
  bvV 32
dataReadE = coerce go
  where
    go :: DataArrSMT -> Clash.BitVec 10 -> Clash.BitVec 32
    go (DataArrSMT arr) (Clash.BitVecP k) = Clash.BitVecP (P.aselect arr k)

dataWriteE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  (Coercible DataArrSMT arr) =>
  (Coercible Clash.BitVec bvI) =>
  (Coercible Clash.BitVec bvV) =>
  arr ->
  bvI 10 ->
  bvV 32 ->
  arr
dataWriteE = coerce go
  where
    go :: DataArrSMT -> Clash.BitVec 10 -> Clash.BitVec 32 -> DataArrSMT
    go (DataArrSMT arr) (Clash.BitVecP k) (Clash.BitVecP v) = DataArrSMT (P.astore arr k v)

dataZeroE :: forall arr. (Coercible DataArrSMT arr) => arr
dataZeroE = coerce (DataArrSMT (P.aconst 0))
