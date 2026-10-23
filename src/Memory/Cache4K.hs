{- HLINT ignore "Eta reduce" -}
-- | A 4 KiB direct-mapped cache: 256 lines of 16 bytes, four words each.
--
-- An address splits into a tag (bits 31 to 12), a set (bits 11 to 4), a word
-- within the line (bits 3 and 2) and a byte within the word (bits 1 and 0). The
-- tags and the data live in two arrays, 'TagArr' indexed by set and 'DataArr'
-- indexed by set and word. A tag entry is the valid bit followed by the tag.
--
-- The arrays are 'Vec's, read and written only through the wrappers below.
-- Those are @OPAQUE@ so that the proof can replace them by SMT array operations
-- ("Proof.Cache.Array"), as it does for the register file and memory: the
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
    Tag,
    SetIdx,
    WordIdx,
    ByteIdx,
    tagOf,
    setOf,
    wordOf,
    byteOf,
    wordAddr,

    -- * The cache
    Cache4K (..),
    emptyCache4K,
    entryValid,
    entryTag,
    cacheHit,
    cacheFill,
    cacheRead,
    cacheWrite,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Memory.Types (MemOps (..))
import Types (Address, Size (..), Word, Byte)
import Prelude hiding (Ordering (..), Word, init, log, not, repeat, undefined, (!!), (&&), (++), (||))

-- Arrays ------------------------------------------------------------------------

-- | The tag entries, one per set: the valid bit, then the tag.
newtype TagArr = TagArr (Vec 256 (BitVector 21))
  deriving (Eq, Show)

-- | The tag entry of set @s@.
{-# OPAQUE tagRead #-}
tagRead :: BitVector 8 -> TagArr -> BitVector 21
tagRead s (TagArr v) = v !! s

-- | Set the tag entry of set @s@ to @e@.
{-# OPAQUE tagWrite #-}
tagWrite :: BitVector 8 -> BitVector 21 -> TagArr -> TagArr
tagWrite s e (TagArr v) = TagArr (replace s e v)

-- | No line valid.
{-# OPAQUE tagEmpty #-}
tagEmpty :: TagArr
tagEmpty = TagArr (repeat 0)

-- | The data, one word per set and word within the line.
newtype DataArr = DataArr (Vec 1024 Word)
  deriving (Eq, Show)

-- | The word at index @i@ (set, then word within the line).
{-# OPAQUE dataRead #-}
dataRead :: BitVector 10 -> DataArr -> Word
dataRead i (DataArr v) = v !! i

-- | Set the word at index @i@ to @w@.
{-# OPAQUE dataWrite #-}
dataWrite :: BitVector 10 -> Word -> DataArr -> DataArr
dataWrite i w (DataArr v) = DataArr (replace i w v)

-- | All words zero.
{-# OPAQUE dataZero #-}
dataZero :: DataArr
dataZero = DataArr (repeat 0)

-- Addresses ---------------------------------------------------------------------

-- | Which block of memory a line holds, among those that map to its set: bits
-- 31 to 12 of an address.
type Tag = BitVector 20

-- | The set, and so the line, an address maps to: bits 11 to 4.
type SetIdx = BitVector 8

-- | The word within a line: bits 3 and 2.
type WordIdx = BitVector 2

-- | The byte within a word: bits 1 and 0.
type ByteIdx = BitVector 2

-- | The tag of an address.
tagOf :: Address -> Tag
tagOf a = slice d31 d12 (pack a)

-- | The set of an address.
setOf :: Address -> SetIdx
setOf a = slice d11 d4 (pack a)

-- | The word of an address within its line.
wordOf :: Address -> WordIdx
wordOf a = slice d3 d2 (pack a)

-- | The byte of an address within its word.
byteOf :: Address -> ByteIdx
byteOf a = slice d1 d0 (pack a)

-- | The address of byte @bi@ of word @wi@ of the line with tag @t@ in set @s@.
wordAddr :: Tag -> SetIdx -> WordIdx -> ByteIdx -> Address
wordAddr t s wi bi = unpack (t ++# s ++# wi ++# bi)

-- The cache ---------------------------------------------------------------------

-- | The cache: a tag entry per set, and the four words of each line.
data Cache4K = Cache4K
  { -- | The tag entries, one per set.
    tagArr :: TagArr,
    -- | The cached words, four per set.
    dataArr :: DataArr
  }
  deriving (Eq, Show)

-- | The empty cache: no line valid.
emptyCache4K :: Cache4K
emptyCache4K = Cache4K tagEmpty dataZero

-- | Does a tag entry hold a block? Its top bit, the valid bit.
entryValid :: BitVector 21 -> Bool
entryValid e = slice d20 d20 e == 1

-- | The tag of the block a tag entry holds: its low 20 bits.
entryTag :: BitVector 21 -> Tag
entryTag e = slice d19 d0 e

-- | Does the line of an address hold its block?
cacheHit :: Address -> Cache4K -> Bool
cacheHit a c = entryValid e && entryTag e == tagOf a
  where
    e = tagRead (setOf a) (tagArr c)

-- | Load the line of an address from memory: the four words of its block, and
-- the line's tag entry made valid with the address's tag.
cacheFill :: (MemOps m) => Address -> m -> Cache4K -> Cache4K
cacheFill a mem c = Cache4K tags' dat4
  where
    tags' = tagWrite (setOf a) ((1 :: BitVector 1) ++# tagOf a) (tagArr c)

    dat1 = dataWrite (i 0) (rd 0) (dataArr c)
    dat2 = dataWrite (i 1) (rd 1) dat1
    dat3 = dataWrite (i 2) (rd 2) dat2
    dat4 = dataWrite (i 3) (rd 3) dat3

    i :: WordIdx -> BitVector 10
    i wi = setOf a ++# wi
    rd :: WordIdx -> Word
    rd wi = memReadWord (wordAddr (tagOf a) (setOf a) wi 0) mem

-- | Byte @bi@ of a word.
cacheReadByteInWord :: ByteIdx -> Word -> Byte
cacheReadByteInWord bi w = case bi of
  0 -> slice d7 d0 w
  1 -> slice d15 d8 w
  2 -> slice d23 d16 w
  _ -> slice d31 d24 w

-- | The cached byte at an address, meaningful on a hit.
cacheReadByte :: Address -> Cache4K -> Byte
cacheReadByte a c =
  let w = dataRead (setOf a ++# wordOf a) (dataArr c)
  in cacheReadByteInWord (byteOf a) w

-- | What a read of @size@ at an address gets from the cache: its bytes, the
-- byte at the address lowest, zero above, as memory answers
-- ('Memory.Types.readResp'). Byte by byte, so a read may cross a word or line
-- boundary. Meaningful when the lines of all its bytes hit.
cacheRead :: Address -> Size -> Cache4K -> Word
cacheRead a size c = case size of
  Byte -> zeroExtend b0
  Half -> zeroExtend (b1 ++# b0)
  Word -> b3 ++# b2 ++# b1 ++# b0
  where
    b0 = cacheReadByte a c
    b1 = cacheReadByte (a + 1) c
    b2 = cacheReadByte (a + 2) c
    b3 = cacheReadByte (a + 3) c

-- | A word with byte @bi@ replaced by @b@.
cacheWriteByteInWord :: ByteIdx -> Byte -> Word -> Word
cacheWriteByteInWord bi b w = case bi of
  0 -> slice d31 d8 w ++# b
  1 -> slice d31 d16 w ++# b ++# slice d7 d0 w
  2 -> slice d31 d24 w ++# b ++# slice d15 d0 w
  _ -> b ++# slice d23 d0 w

-- | Put a byte into the cached word of its address, if its line holds the
-- address's block; otherwise leave the cache alone (no allocation).
cacheWriteByte :: Address -> Byte -> Cache4K -> Cache4K
cacheWriteByte a b c
  | cacheHit a c = c {dataArr = dataWrite i (cacheWriteByteInWord (byteOf a) b w) (dataArr c)}
  | otherwise = c
  where
    i = setOf a ++# wordOf a
    w = dataRead i (dataArr c)

-- | What a write does to the cache. Memory always takes the write
-- (write-through); the cache only makes sure its copy does not go stale. Byte by
-- byte, as memory stores it ('Memory.Types.memWrite'): each byte goes into
-- its cached word if its line hits, so a write may cross a word or line
-- boundary.
cacheWrite :: Address -> Size -> Word -> Cache4K -> Cache4K
cacheWrite a size w c = case size of
  Byte -> c1
  Half -> c2
  Word -> c4
  where
    c1 = cacheWriteByte a (slice d7 d0 w) c
    c2 = cacheWriteByte (a + 1) (slice d15 d8 w) c1
    c3 = cacheWriteByte (a + 2) (slice d23 d16 w) c2
    c4 = cacheWriteByte (a + 3) (slice d31 d24 w) c3
