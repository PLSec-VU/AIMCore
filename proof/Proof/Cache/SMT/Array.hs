{-# LANGUAGE KindSignatures #-}

-- | The arrays of "Memory.Cache4K" embedded as SMT arrays, as "Proof.SMT.Array"
-- embeds the register file and memory: a monomorphic SMT newtype per array, and
-- an embedding per wrapper, which the term axioms in "Proof.Cache.SMT.Axioms"
-- put in place of the wrapper.
module Proof.Cache.SMT.Array
  ( TagArrSMT (..),
    freshTag,
    freshTagE,
    tagReadE,
    tagWriteE,
    tagEmptyE,
    DataArrSMT (..),
    freshData,
    freshRaw,
    freshDataE,
    dataReadE,
    dataWriteE,
    dataZeroE,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Coerce (coerce)
import Memory.Cache4K (DataArr, TagArr)
import Proof.Cache.SMT.Raw (RawData)
import qualified SymFC.BuiltIn as P
import SymFC.BuiltIn (Embeddable, Embedding (..), embedding)
import qualified SymFC.Clash as Clash
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | 'Memory.Cache4K.TagArr' on the SMT side.
newtype TagArrSMT = TagArrSMT (P.Array (P.BitVec 8) (P.BitVec 21))

-- | A fresh tag array. Like 'Proof.SMT.Array.freshRA' it has no Haskell
-- meaning: it halts unless 'freshTagE' replaces it.
{-# OPAQUE freshTag #-}
freshTag :: P.Sym TagArr
freshTag = P.halt

freshTagE :: forall arr. Embeddable arr TagArrSMT => P.Sym arr
freshTagE | Embedding <- embedding @arr = coerce . TagArrSMT <$> P.fresh

tagReadE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  Embeddable arr TagArrSMT =>
  Embeddable bvI Clash.BitVec =>
  Embeddable bvV Clash.BitVec =>
  bvI 8 ->
  arr ->
  bvV 21
tagReadE
  | Embedding <- embedding @arr
  , Embedding <- embedding @bvI
  , Embedding <- embedding @bvV = coerce go
  where
    go :: Clash.BitVec 8 -> TagArrSMT -> Clash.BitVec 21
    go (Clash.BitVecP k) (TagArrSMT arr) = Clash.BitVecP (P.aselect arr k)

tagWriteE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  Embeddable arr TagArrSMT =>
  Embeddable bvI Clash.BitVec =>
  Embeddable bvV Clash.BitVec =>
  bvI 8 ->
  bvV 21 ->
  arr ->
  arr
tagWriteE
  | Embedding <- embedding @arr
  , Embedding <- embedding @bvI
  , Embedding <- embedding @bvV = coerce go
  where
    go :: Clash.BitVec 8 -> Clash.BitVec 21 -> TagArrSMT -> TagArrSMT
    go (Clash.BitVecP k) (Clash.BitVecP v) (TagArrSMT arr) = TagArrSMT (P.astore arr k v)

tagEmptyE :: forall arr. Embeddable arr TagArrSMT => arr
tagEmptyE | Embedding <- embedding @arr = coerce (TagArrSMT (P.aconst 0))

-- | 'Memory.Cache4K.DataArr' on the SMT side.
newtype DataArrSMT = DataArrSMT (P.Array (P.BitVec 10) (P.BitVec 32))

-- | A fresh data array, which halts unless an embedding replaces it.
{-# OPAQUE freshData #-}
freshData :: P.Sym DataArr
freshData = P.halt

-- | A fresh raw array, which halts unless 'freshDataE' replaces it.
{-# OPAQUE freshRaw #-}
freshRaw :: P.Sym RawData
freshRaw = P.halt

freshDataE :: forall arr. Embeddable arr DataArrSMT => P.Sym arr
freshDataE | Embedding <- embedding @arr = coerce . DataArrSMT <$> P.fresh

dataReadE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  Embeddable arr DataArrSMT =>
  Embeddable bvI Clash.BitVec =>
  Embeddable bvV Clash.BitVec =>
  bvI 10 ->
  arr ->
  bvV 32
dataReadE
  | Embedding <- embedding @arr
  , Embedding <- embedding @bvI
  , Embedding <- embedding @bvV = coerce go
  where
    go :: Clash.BitVec 10 -> DataArrSMT -> Clash.BitVec 32
    go (Clash.BitVecP k) (DataArrSMT arr) = Clash.BitVecP (P.aselect arr k)

dataWriteE ::
  forall arr (bvI :: Nat -> Type) (bvV :: Nat -> Type).
  Embeddable arr DataArrSMT =>
  Embeddable bvI Clash.BitVec =>
  Embeddable bvV Clash.BitVec =>
  bvI 10 ->
  bvV 32 ->
  arr ->
  arr
dataWriteE
  | Embedding <- embedding @arr
  , Embedding <- embedding @bvI
  , Embedding <- embedding @bvV = coerce go
  where
    go :: Clash.BitVec 10 -> Clash.BitVec 32 -> DataArrSMT -> DataArrSMT
    go (Clash.BitVecP k) (Clash.BitVecP v) (DataArrSMT arr) = DataArrSMT (P.astore arr k v)

dataZeroE :: forall arr. Embeddable arr DataArrSMT => arr
dataZeroE | Embedding <- embedding @arr = coerce (DataArrSMT (P.aconst 0))
