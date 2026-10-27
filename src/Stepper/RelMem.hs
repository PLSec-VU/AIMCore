{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.RelMem
  ( RelMem
  , sameMem
  , readByte
  , writeByte
  , readMemWord
  , writeMem
  , leftMem
  , rightMem
  ) where

import Clash.Prelude (d0, d15, d16, d23, d24, d31, d7, d8, slice, (++#))
import Data.Maybe (fromMaybe)
import Stepper.AddrMap (AddrMap)
import Stepper.AddrMap qualified as AddrMap
import Stepper.Concrete (concreteAddress)
import Stepper.Memory (Memory)
import Stepper.Memory qualified as Memory
import Stepper.Rel (Rel (..), left, right)
import Types (Address, Byte, Size (..), Word)
import Prelude hiding (Word)

data Base
  = SameBase (Memory 32 8)
  | PairBase (Memory 32 8) (Memory 32 8)

data RelMem = RelMem
  { memBase :: Base
  , memWrites :: AddrMap (Rel Byte)
  }

sameMem :: Memory 32 8 -> RelMem
sameMem mem = RelMem (SameBase mem) AddrMap.empty

storeByte :: Address -> Rel Byte -> Base -> Base
storeByte addr (Same byte) (SameBase mem) = SameBase (Memory.write addr byte mem)
storeByte addr (Pair byteA byteB) (SameBase mem) =
  PairBase (Memory.write addr byteA mem) (Memory.write addr byteB mem)
storeByte addr byte (PairBase memA memB) =
  PairBase (Memory.write addr (left byte) memA) (Memory.write addr (right byte) memB)

loadByte :: Address -> Base -> Rel Byte
loadByte addr (SameBase mem) = Same (Memory.read addr mem)
loadByte addr (PairBase memA memB) = Pair (Memory.read addr memA) (Memory.read addr memB)

withWritesStored :: RelMem -> Base
withWritesStored (RelMem base writes) = AddrMap.foldrWithKey storeByte base writes

readByte :: Address -> RelMem -> Rel Byte
readByte addr mem = case concreteAddress addr of
  Just known -> fromMaybe (loadByte known (memBase mem)) (AddrMap.lookup known (memWrites mem))
  Nothing -> loadByte addr (withWritesStored mem)

writeByte :: Address -> Rel Byte -> RelMem -> RelMem
writeByte addr byte mem = case concreteAddress addr of
  Just known -> mem {memWrites = AddrMap.insert known byte (memWrites mem)}
  Nothing -> RelMem (storeByte addr byte (withWritesStored mem)) AddrMap.empty

readMemWord :: Address -> RelMem -> Rel Word
readMemWord addr mem =
  (\b3 b2 b1 b0 -> b3 ++# b2 ++# b1 ++# b0)
    <$> readByte (addr + 3) mem
    <*> readByte (addr + 2) mem
    <*> readByte (addr + 1) mem
    <*> readByte addr mem

writeMem :: Size -> Address -> Rel Word -> RelMem -> RelMem
writeMem size addr word mem = case size of
  Byte -> put addr b0 mem
  Half -> put (addr + 1) b1 (put addr b0 mem)
  Word -> put (addr + 3) b3 (put (addr + 2) b2 (put (addr + 1) b1 (put addr b0 mem)))
  where
    b0 = slice d7 d0 <$> word
    b1 = slice d15 d8 <$> word
    b2 = slice d23 d16 <$> word
    b3 = slice d31 d24 <$> word
    put = writeByte

leftMem :: RelMem -> Memory 32 8
leftMem mem = case withWritesStored mem of
  SameBase base -> base
  PairBase base _ -> base

rightMem :: RelMem -> Memory 32 8
rightMem mem = case withWritesStored mem of
  SameBase base -> base
  PairBase _ base -> base
