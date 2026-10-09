-- | A 4 KiB direct-mapped cache: 256 lines of 16 bytes, four words each.
--
-- An address splits into a tag (bits 31 to 12), a set (bits 11 to 4), a word
-- within the line (bits 3 and 2) and a byte within the word (bits 1 and 0). The
-- tags and the data live in two arrays, 'TagArr' indexed by set and 'DataArr'
-- indexed by set and word. A tag entry is the valid bit followed by the tag.
--
-- The arrays are 'Vec's, read and written only through the wrappers below.
-- Those are @OPAQUE@ so that the proof can replace them by SMT array operations
-- ("Proof.SMT.CacheArray"), as it does for the register file and memory: the
-- Clash 'Vec' operations themselves cannot be symbolically executed.
module Memory.Cache4K
  ( -- * Arrays
    TagArr (..),
    tagRead,
    tagWrite,
    tagEmpty,
    DataArr (..),
    dataRead,
    dataWrite,
    dataZero,

    -- * Addresses
    SetIdx,
    WordIdx,
    Tag,
    setOf,
    wordOf,
    tagOf,
    wordAddr,

    -- * The cache
    Cache4K (..),
    emptyCache4K,
    entryValid,
    entryTag,
    hitK,
    wordAtK,
    fillK,
    updateK,
    invalidateK,
    mergeWord,
    coherentAtK,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Memory.Types (MemOps (..))
import Types (Address, Size (..), Word)
import Prelude hiding (Ordering (..), Word, init, log, not, repeat, undefined, (!!), (&&), (++), (||))

-- Arrays ------------------------------------------------------------------------

-- | The tag entries, one per set: the valid bit, then the tag.
newtype TagArr = TagArr (Vec 256 (BitVector 21))
  deriving (Eq, Show)

{-# OPAQUE tagRead #-}
tagRead :: TagArr -> BitVector 8 -> BitVector 21
tagRead (TagArr v) i = v !! i

{-# OPAQUE tagWrite #-}
tagWrite :: TagArr -> BitVector 8 -> BitVector 21 -> TagArr
tagWrite (TagArr v) i e = TagArr (replace i e v)

-- | No line valid.
{-# OPAQUE tagEmpty #-}
tagEmpty :: TagArr
tagEmpty = TagArr (repeat 0)

-- | The data, one word per set and word within the line.
newtype DataArr = DataArr (Vec 1024 Word)
  deriving (Eq, Show)

{-# OPAQUE dataRead #-}
dataRead :: DataArr -> BitVector 10 -> Word
dataRead (DataArr v) i = v !! i

{-# OPAQUE dataWrite #-}
dataWrite :: DataArr -> BitVector 10 -> Word -> DataArr
dataWrite (DataArr v) i w = DataArr (replace i w v)

{-# OPAQUE dataZero #-}
dataZero :: DataArr
dataZero = DataArr (repeat 0)

-- Addresses ---------------------------------------------------------------------

type SetIdx = BitVector 8

type WordIdx = BitVector 2

type Tag = BitVector 20

setOf :: Address -> SetIdx
setOf a = slice d11 d4 (pack a)

wordOf :: Address -> WordIdx
wordOf a = slice d3 d2 (pack a)

tagOf :: Address -> Tag
tagOf a = slice d31 d12 (pack a)

-- | The address of word @w@ of the line with tag @t@ in set @s@.
wordAddr :: Tag -> SetIdx -> WordIdx -> Address
wordAddr t s w = unpack (t ++# s ++# w ++# (0 :: BitVector 2))

-- The cache ---------------------------------------------------------------------

data Cache4K = Cache4K
  { ckTags :: TagArr,
    ckData :: DataArr
  }
  deriving (Eq, Show)

emptyCache4K :: Cache4K
emptyCache4K = Cache4K tagEmpty dataZero

entryValid :: BitVector 21 -> Bool
entryValid e = slice d20 d20 e == 1

entryTag :: BitVector 21 -> Tag
entryTag e = slice d19 d0 e

-- | Does the line of an address hold its block?
hitK :: Address -> Cache4K -> Bool
hitK a c = entryValid e && entryTag e == tagOf a
  where
    e = tagRead (ckTags c) (setOf a)

-- | The cached word of an address's block, meaningful on a hit.
wordAtK :: Address -> Cache4K -> Word
wordAtK a c = dataRead (ckData c) (setOf a ++# wordOf a)

-- | Fill the line of an address with the four words of its block.
fillK :: Address -> Word -> Word -> Word -> Word -> Cache4K -> Cache4K
fillK a w0 w1 w2 w3 (Cache4K t d) =
  Cache4K
    (tagWrite t s ((1 :: BitVector 1) ++# tagOf a))
    (dataWrite (dataWrite (dataWrite (dataWrite d (ix 0) w0) (ix 1) w1) (ix 2) w2) (ix 3) w3)
  where
    s = setOf a
    ix :: WordIdx -> BitVector 10
    ix w = s ++# w

-- | A write that stays inside one word: on a hit, the written bytes are merged
-- into the cached word.
updateK :: Address -> Size -> Word -> Cache4K -> Cache4K
updateK a size v c
  | hitK a c = c {ckData = dataWrite (ckData c) i (mergeWord a size v (dataRead (ckData c) i))}
  | otherwise = c
  where
    i = setOf a ++# wordOf a

-- | Drop the line of an address, if it holds the address's block.
invalidateK :: Address -> Cache4K -> Cache4K
invalidateK a c
  | hitK a c = c {ckTags = tagWrite (ckTags c) (setOf a) 0}
  | otherwise = c

-- | A word with the bytes of a write at an address put in, as memory stores
-- them ('Memory.Types.memWriteWord'): the low bytes of the value, from the
-- address's byte on. Only for a write that stays inside the word; any other
-- leaves the word as it was.
mergeWord :: Address -> Size -> Word -> Word -> Word
mergeWord a size v old = case size of
  Byte -> case off of
    0 -> slice d31 d8 old ++# slice d7 d0 v
    1 -> slice d31 d16 old ++# slice d7 d0 v ++# slice d7 d0 old
    2 -> slice d31 d24 old ++# slice d7 d0 v ++# slice d15 d0 old
    _ -> slice d7 d0 v ++# slice d23 d0 old
  Half -> case off of
    0 -> slice d31 d16 old ++# slice d15 d0 v
    1 -> slice d31 d24 old ++# slice d15 d0 v ++# slice d7 d0 old
    2 -> slice d15 d0 v ++# slice d15 d0 old
    _ -> old
  Word -> case off of
    0 -> v
    _ -> old
  where
    off = slice d1 d0 (pack a)

-- | Coherence at one word of one set: if the line there is valid, the word is
-- the one memory holds at its address. The cache is coherent if this holds at
-- every set and word.
coherentAtK :: (MemOps m) => m -> SetIdx -> WordIdx -> Cache4K -> Bool
coherentAtK mem s w c =
  not (entryValid e) || dataRead (ckData c) (s ++# w) == memReadWord (wordAddr (entryTag e) s w) mem
  where
    e = tagRead (ckTags c) s
