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
  )
where

import Clash.Prelude hiding (Word, init)
import Data.Proxy (Proxy (..))
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
fromBlock :: Address -> Word -> Word
fromBlock addr w = w `shiftR` (8 * fromIntegral (addr .&. 3))

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
