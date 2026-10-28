{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.RelMem
  ( RelMem
  , sameMem
  , readByte
  , writeByte
  , readMemWord
  , loadKnownWord
  , writeMem
  , leftMem
  , rightMem
  ) where

import Clash.Prelude (d0, d15, d16, d23, d24, d31, d7, d8, slice, (++#))
import Data.Bits (complement, (.&.))
import Data.Maybe (fromMaybe)
import Stepper.AddrMap (AddrMap)
import Stepper.AddrMap qualified as AddrMap
import Stepper.Concrete (concreteAddress)
import Stepper.Memory (Memory)
import Stepper.Memory qualified as Memory
import Stepper.Rel (Rel (..), left, right)
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Byte, Size (..), Word)
import Prelude hiding (Word)

data Base
  = SameBase (Memory 32 8)
  | PairBase (Memory 32 8) (Memory 32 8)

data Lane = Lane0 | Lane1 | Lane2 | Lane3

data Cell
  = WholeWord (Rel Word)
  | Lanes (Maybe (Rel Byte)) (Maybe (Rel Byte)) (Maybe (Rel Byte)) (Maybe (Rel Byte))

data RelMem = RelMem
  { memBase :: Base
  , memWrites :: AddrMap Cell
  }

sameMem :: Memory 32 8 -> RelMem
sameMem mem = RelMem (SameBase mem) AddrMap.empty

storeByte :: Address -> Rel Byte -> Base -> Base
storeByte addr (Same byte) (SameBase mem) = SameBase (Memory.write addr byte mem)
storeByte addr (Pair byteA byteB) (SameBase mem) =
  PairBase (Memory.write addr byteA mem) (Memory.write addr byteB mem)
storeByte addr byte (PairBase memA memB) =
  PairBase (Memory.write addr (left byte) memA) (Memory.write addr (right byte) memB)

storeLane :: Address -> Maybe (Rel Byte) -> Base -> Base
storeLane addr = maybe id (storeByte addr)

loadByte :: Address -> Base -> Rel Byte
loadByte addr (SameBase mem) = Same (Memory.read addr mem)
loadByte addr (PairBase memA memB) = Pair (Memory.read addr memA) (Memory.read addr memB)

lowBits :: Address
lowBits = 3

wordStart :: Address -> Address
wordStart addr = addr .&. complement lowBits

isWordStart :: Address -> Bool
isWordStart addr = addr .&. lowBits == 0

laneOf :: Address -> Lane
laneOf addr = case addr .&. lowBits of
  0 -> Lane0
  1 -> Lane1
  2 -> Lane2
  _ -> Lane3

byte0, byte1, byte2, byte3 :: Rel Word -> Rel Byte
byte0 word = slice d7 d0 <$> word
byte1 word = slice d15 d8 <$> word
byte2 word = slice d23 d16 <$> word
byte3 word = slice d31 d24 <$> word

joinBytes :: Rel Byte -> Rel Byte -> Rel Byte -> Rel Byte -> Rel Word
joinBytes b3 b2 b1 b0 = (\x3 x2 x1 x0 -> x3 ++# x2 ++# x1 ++# x0) <$> b3 <*> b2 <*> b1 <*> b0

cellLanes :: Cell -> (Maybe (Rel Byte), Maybe (Rel Byte), Maybe (Rel Byte), Maybe (Rel Byte))
cellLanes (WholeWord word) = (Just (byte0 word), Just (byte1 word), Just (byte2 word), Just (byte3 word))
cellLanes (Lanes l0 l1 l2 l3) = (l0, l1, l2, l3)

laneIn :: Lane -> Cell -> Maybe (Rel Byte)
laneIn lane cell = case (lane, cellLanes cell) of
  (Lane0, (l0, _, _, _)) -> l0
  (Lane1, (_, l1, _, _)) -> l1
  (Lane2, (_, _, l2, _)) -> l2
  (Lane3, (_, _, _, l3)) -> l3

withLane :: Lane -> Rel Byte -> Maybe Cell -> Cell
withLane lane byte old = case (lane, maybe (Nothing, Nothing, Nothing, Nothing) cellLanes old) of
  (Lane0, (_, l1, l2, l3)) -> Lanes (Just byte) l1 l2 l3
  (Lane1, (l0, _, l2, l3)) -> Lanes l0 (Just byte) l2 l3
  (Lane2, (l0, l1, _, l3)) -> Lanes l0 l1 (Just byte) l3
  (Lane3, (l0, l1, l2, _)) -> Lanes l0 l1 l2 (Just byte)

storeCell :: Address -> Cell -> Base -> Base
storeCell start cell base =
  storeLane start l0 (storeLane (start + 1) l1 (storeLane (start + 2) l2 (storeLane (start + 3) l3 base)))
  where
    (l0, l1, l2, l3) = cellLanes cell

withWritesStored :: RelMem -> Base
withWritesStored (RelMem base writes) = AddrMap.foldrWithKey storeCell base writes

readKnownByte :: Address -> RelMem -> Rel Byte
readKnownByte known mem =
  fromMaybe (loadByte known (memBase mem)) (AddrMap.lookup (wordStart known) (memWrites mem) >>= laneIn (laneOf known))

readByte :: Address -> RelMem -> Rel Byte
readByte addr mem = case concreteAddress addr of
  Just known -> readKnownByte known mem
  Nothing -> loadByte addr (withWritesStored mem)

outOfReach :: Bool -> Address -> Address -> Address -> SymFC.Sym Bool
outOfReach path addr lo hi = do
  result <- SymFC.valid $ SymFC.boolean $ not path || addr < lo || hi + lowBits < addr
  pure case result of
    SymFC.None -> True
    _ -> False

writeByte :: Bool -> Address -> Rel Byte -> RelMem -> SymFC.Sym RelMem
writeByte path addr byte mem = case concreteAddress addr of
  Just known ->
    pure mem {memWrites = AddrMap.insertWith (withLane (laneOf known) byte) (wordStart known) (memWrites mem)}
  Nothing -> do
    (mayAlias, untouched) <- AddrMap.partitionByRange (outOfReach path addr) (memWrites mem)
    let spilled = AddrMap.foldrWithKey storeCell (memBase mem) mayAlias
    pure (RelMem (storeByte addr byte spilled) untouched)

knownWordStart :: Address -> Maybe Address
knownWordStart addr = case concreteAddress addr of
  Just known | isWordStart known -> Just known
  _ -> Nothing

readWordAt :: Address -> RelMem -> Rel Word
readWordAt start mem = case AddrMap.lookup start (memWrites mem) of
  Just (WholeWord word) -> word
  cell -> wordOver start cell (memBase mem)

wordOver :: Address -> Maybe Cell -> Base -> Rel Word
wordOver start cell base = joinBytes (laneOr 3 Lane3) (laneOr 2 Lane2) (laneOr 1 Lane1) (laneOr 0 Lane0)
  where
    laneOr offset lane = fromMaybe (loadByte (start + offset) base) (cell >>= laneIn lane)

readMemWord :: Address -> RelMem -> Rel Word
readMemWord addr mem = case knownWordStart addr of
  Just start -> readWordAt start mem
  Nothing -> joinBytes (readByte (addr + 3) mem) (readByte (addr + 2) mem) (readByte (addr + 1) mem) (readByte addr mem)

loadKnownWord :: Address -> RelMem -> Maybe (Rel Word, RelMem)
loadKnownWord addr mem = loadAt <$> knownWordStart addr
  where
    loadAt start = case AddrMap.lookup start (memWrites mem) of
      Nothing -> recordBaseWord start mem
      Just _ -> (readWordAt start mem, mem)

recordBaseWord :: Address -> RelMem -> (Rel Word, RelMem)
recordBaseWord start mem = (word, mem {memWrites = AddrMap.insert start (WholeWord word) (memWrites mem)})
  where
    word = wordOver start Nothing (memBase mem)

writeMem :: Bool -> Size -> Address -> Rel Word -> RelMem -> SymFC.Sym RelMem
writeMem path size addr word mem = case (size, knownWordStart addr) of
  (Word, Just start) -> pure mem {memWrites = AddrMap.insert start (WholeWord word) (memWrites mem)}
  (Byte, _) -> put addr (byte0 word) mem
  (Half, _) -> put addr (byte0 word) mem >>= put (addr + 1) (byte1 word)
  (Word, Nothing) ->
    put addr (byte0 word) mem >>= put (addr + 1) (byte1 word) >>= put (addr + 2) (byte2 word) >>= put (addr + 3) (byte3 word)
  where
    put = writeByte path

leftMem :: RelMem -> Memory 32 8
leftMem mem = case withWritesStored mem of
  SameBase base -> base
  PairBase base _ -> base

rightMem :: RelMem -> Memory 32 8
rightMem mem = case withWritesStored mem of
  SameBase base -> base
  PairBase _ base -> base
