{-# LANGUAGE UndecidableInstances #-}

-- | A small, swappable cache model.
--
-- A cache holds, per four-byte block, the word that starts at the block's first
-- byte. Lookups, inserts and invalidations are keyed by any address in the
-- block. A read is answered from the block's word by 'fromBlock', which is only
-- correct for a read that stays inside one block ('inBlock').
module Memory.Cache
  ( CacheOps (..),
    CacheOutcome (..),
    cacheAccess,
    blockStart,
    inBlock,
    fromBlock,
    CacheLine (..),
    DirectMapped (..),
    mkDirectMapped,
    coherent,
    Cache4 (..),
    emptyCache4,
    coherent4,
    tags4,
  )
where

import Clash.Prelude hiding (Word, init)
import Data.Proxy (Proxy (..))
import Memory.Types (MemOps (..))
import Types (Address, Size (..), Word)
import Prelude hiding (Word, repeat, (!!), (&&))

-- | Whether a lookup was a hit or a miss.
data CacheOutcome = Hit | Miss
  deriving (Eq, Show, Generic, NFDataX)

-- | Operations supported by a cache model.
class CacheOps c where
  -- | Look up a word at an address.
  cacheLookup :: Address -> c -> Maybe Word

  -- | Install a word at an address.
  cacheInsert :: Address -> Word -> c -> c

  -- | Invalidate the line for an address.
  cacheInvalidate :: Address -> c -> c

-- | Read the word of an address's block through a cache, from backing memory on
-- a miss. 'fromBlock' cuts the bytes at the address out of it.
cacheAccess ::
  (CacheOps c) =>
  (Address -> Word) ->
  Address ->
  c ->
  (c, Word, CacheOutcome)
cacheAccess readBacking addr c = case cacheLookup addr c of
  Just w -> (c, w, Hit)
  Nothing ->
    let w = readBacking (blockStart addr)
     in (cacheInsert addr w c, w, Miss)

-- | The address of the first byte of the block an address lies in.
blockStart :: Address -> Address
blockStart addr = addr .&. complement 3

-- | Does an access of this size at this address stay inside one block?
inBlock :: Address -> Size -> Bool
inBlock addr size = (addr .&. 3) + bytes size <= 4
  where
    bytes Byte = 1
    bytes Half = 2
    bytes Word = 4

-- | What a read at an address returns, cut from the word of its block: the byte
-- at the address moved to the bottom, the bytes before it dropped. The bytes the
-- read's width covers are exactly memory's when the read stays inside the
-- block; the core uses no others ('Instruction.loadExtend').
--
-- Written with slices rather than a shift by @8 * (addr .&. 3)@: a shift amount
-- computed as an 'Int' takes the symbolic proof out of the bitvector fragment
-- that Bitwuzla reads.
fromBlock :: Address -> Word -> Word
fromBlock addr w = case slice d1 d0 (pack addr) of
  0 -> w
  1 -> zeroExtend (slice d31 d8 w)
  2 -> zeroExtend (slice d31 d16 w)
  _ -> zeroExtend (slice d31 d24 w)

-- | One line of a direct-mapped cache.
data CacheLine = CacheLine
  { clValid :: Bool,
    clTag :: Address,
    clWord :: Word
  }
  deriving (Eq, Show, Generic, NFDataX)

invalidLine :: CacheLine
invalidLine = CacheLine {clValid = False, clTag = 0, clWord = 0}

-- | A direct-mapped cache of @n@ lines.
newtype DirectMapped n = DirectMapped (Vec n CacheLine)
  deriving (Eq, Show, Generic, NFDataX)

mkDirectMapped :: (KnownNat n) => DirectMapped n
mkDirectMapped = DirectMapped (repeat invalidLine)

-- | Does every valid line hold the word its block starts with in this memory?
-- The invariant that makes the cache answer reads as memory would.
coherent :: forall n. (KnownNat n) => (Address -> Word) -> DirectMapped n -> Bool
coherent readWordAt (DirectMapped ls) = and (imap ok ls)
  where
    ok :: Index n -> CacheLine -> Bool
    ok i l
      | clValid l = clWord l == readWordAt (4 * (clTag l * fromIntegral (natVal (Proxy @n)) + fromIntegral i))
      | otherwise = True

blockOf :: Address -> Address
blockOf addr = addr `div` 4

indexOf :: forall n. (KnownNat n) => Address -> Index n
indexOf addr = fromIntegral (blockOf addr `mod` fromIntegral (natVal (Proxy @n)))

tagOf :: forall n. (KnownNat n) => Address -> Address
tagOf addr = blockOf addr `div` fromIntegral (natVal (Proxy @n))

lineAt :: forall n. (KnownNat n) => Address -> DirectMapped n -> CacheLine
lineAt addr (DirectMapped ls) = ls !! (indexOf @n addr)

instance (KnownNat n, 1 <= n) => CacheOps (DirectMapped n) where
  cacheLookup addr dm
    | clValid line && clTag line == tagOf @n addr = Just (clWord line)
    | otherwise = Nothing
    where
      line = lineAt @n addr dm

  cacheInsert addr w (DirectMapped ls) =
    DirectMapped (replace (indexOf @n addr) (CacheLine True (tagOf @n addr) w) ls)

  cacheInvalidate addr dm@(DirectMapped ls)
    | clValid line && clTag line == tagOf @n addr =
        DirectMapped (replace (indexOf @n addr) invalidLine ls)
    | otherwise = dm
    where
      line = lineAt @n addr dm

-- | A direct-mapped cache of four lines, held as four fields rather than a 'Vec'.
--
-- It is 'DirectMapped' 4 (the test suite checks the two agree), written so that
-- it can be symbolically executed: the Clash 'Vec' operations are opaque to the
-- verifier. An address's line is its bits 3 and 2, and its tag the bits above.
data Cache4 = Cache4
  { line0 :: CacheLine,
    line1 :: CacheLine,
    line2 :: CacheLine,
    line3 :: CacheLine
  }
  deriving (Eq, Show, Generic, NFDataX)

emptyCache4 :: Cache4
emptyCache4 = Cache4 invalidLine invalidLine invalidLine invalidLine

lineIndex4 :: Address -> BitVector 2
lineIndex4 addr = slice d3 d2 (pack addr)

lineTag4 :: Address -> Address
lineTag4 addr = unpack (zeroExtend (slice d31 d4 (pack addr)))

lineAt4 :: BitVector 2 -> Cache4 -> CacheLine
lineAt4 i c = case i of
  0 -> line0 c
  1 -> line1 c
  2 -> line2 c
  _ -> line3 c

setLine4 :: BitVector 2 -> CacheLine -> Cache4 -> Cache4
setLine4 i l c = case i of
  0 -> c {line0 = l}
  1 -> c {line1 = l}
  2 -> c {line2 = l}
  _ -> c {line3 = l}

instance CacheOps Cache4 where
  cacheLookup addr c
    | clValid l && clTag l == lineTag4 addr = Just (clWord l)
    | otherwise = Nothing
    where
      l = lineAt4 (lineIndex4 addr) c

  cacheInsert addr w = setLine4 (lineIndex4 addr) (CacheLine True (lineTag4 addr) w)

  cacheInvalidate addr c
    | clValid l && clTag l == lineTag4 addr = setLine4 i invalidLine c
    | otherwise = c
    where
      i = lineIndex4 addr
      l = lineAt4 i c

-- | 'coherent' for 'Cache4': every valid line has a tag of 28 bits, as
-- 'lineTag4' makes them, and holds the word its block starts with in this
-- memory. The block of line @i@ with tag @t@ starts at @t ++ i ++ 00@.
coherent4 :: (MemOps m) => m -> Cache4 -> Bool
coherent4 mem c =
  ok 0 (line0 c) && ok 1 (line1 c) && ok 2 (line2 c) && ok 3 (line3 c)
  where
    ok :: BitVector 2 -> CacheLine -> Bool
    ok i l
      | clValid l =
          slice d31 d28 (pack (clTag l)) == 0
            && clWord l == memReadWord (unpack (slice d27 d0 (pack (clTag l)) ++# i ++# (0 :: BitVector 2))) mem
      | otherwise = True

-- | The cache without its data: valid bits and tags, which is all a hit or a
-- miss depends on.
tags4 :: Cache4 -> Cache4
tags4 (Cache4 a b c d) = Cache4 (t a) (t b) (t c) (t d)
  where
    t l = l {clWord = 0}

