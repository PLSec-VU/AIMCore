{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Stepper.Memory
  ( Memory (..)
  , empty
  , read
  , write
  , eq

  , RegFile (..)
  ) where

import Clash.Prelude hiding (Word, empty, read)
import Data.Coerce (coerce)
import Data.Constraint (Dict (Dict))
import Memory.Types (MemOps (..))
import Prelude hiding (Word, read)
import Types (Size (..))
import RegFile (RegFileOps (..))
import Access (extract)
import SymFC.BuiltIn qualified as SymFC
import SymFC.Clash (BitVec(..))

newtype Memory k v where
  Memory :: SymFC.Array (SymFC.BitVec k) (SymFC.BitVec v) -> Memory k v

instance 
  ( KnownNat k
  , KnownNat v
  , 1 <= k
  , 1 <= v
  ) => SymFC.Symbolic (Memory k v) where
  symbolic = Memory <$> SymFC.symbolic

empty
  :: KnownNat k
  => KnownNat v
  => 1 <= k
  => 1 <= v
  => Memory k v
empty = Memory $ SymFC.aconst 0

eq :: Memory k v -> Memory k v -> Bool
eq @k @v = coerce $ (==) @(SymFC.Array (SymFC.BitVec k) (SymFC.BitVec v))

instance Eq (Memory k v) where
  (==) = eq

read
  :: forall k v
   . Unsigned k
  -> Memory k v
  -> BitVector v
read @k @v key (Memory arr)
  | SymFC.ArrayType SymFC.BitVecType SymFC.BitVecType <- SymFC.atype arr
  , Dict <- SymFC.reifyKnownNat @k $ Dict @(KnownNat k)
  , Dict <- SymFC.reifyKnownNat @v $ Dict @(KnownNat v) = do
    let BitVecP key' = bitCoerce key
    let value = SymFC.aselect arr key'
    bitCoerce $ BitVecP value

write
  :: forall k v
   . Unsigned k
  -> BitVector v
  -> Memory k v
  -> Memory k v
write @k @v key value (Memory arr)
  | SymFC.ArrayType SymFC.BitVecType SymFC.BitVecType <- SymFC.atype arr
  , Dict <- SymFC.reifyKnownNat @k $ Dict @(KnownNat k)
  , Dict <- SymFC.reifyKnownNat @v $ Dict @(KnownNat v) = do
    let BitVecP key' = bitCoerce key
    let BitVecP value' = bitCoerce value
    Memory $ SymFC.astore arr key' value'

instance MemOps (Memory 32 8) where
  memReadByte = read
  memReadWord a m = do
    memReadByte (a + 3) m
      ++# memReadByte (a + 2) m
      ++# memReadByte (a + 1) m
      ++# memReadByte a m
  memWriteWord size a w m = case size of
    Byte -> put a b0 m
    Half -> put (a + 1) b1 (put a b0 m)
    Word -> put (a + 3) b3 (put (a + 2) b2 (put (a + 1) b1 (put a b0 m)))
    where
      b0 = slice d7 d0 w
      b1 = slice d15 d8 w
      b2 = slice d23 d16 w
      b3 = slice d31 d24 w
      put = write

-- | Register file for the stepper.
--
-- WARNING: The 'f' parameter is a lie here. We use this only for the stepper
-- where 'f' is 'Identity'.
newtype RegFile (f :: Type -> Type) where
  RegFile :: Memory 5 32 -> RegFile f

instance SymFC.Symbolic (RegFile f) where
  symbolic = RegFile <$> SymFC.symbolic

instance Eq (RegFile f) where
  RegFile lhs == RegFile rhs = lhs == rhs

instance RegFileOps RegFile where
  lookupRFg idx (RegFile memory) = pure case idx of
    0 -> 0
    _ -> read idx memory

  modifyRFg idx value rf@(RegFile memory) = case idx of
    0 -> rf
    _ -> RegFile $ write idx (extract value) memory

  initRFg = RegFile empty
