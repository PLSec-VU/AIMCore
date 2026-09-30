{-# LANGUAGE UndecidableInstances #-}

-- | A small, swappable cache model.
module Memory.Cache
  ( CacheOps (..),
    CacheOutcome (..),
    cacheAccess,
    CacheLine (..),
    DirectMapped (..),
    mkDirectMapped,
  )
where

import Clash.Prelude hiding (Word, init)
import Data.Proxy (Proxy (..))
import Types (Address, Word)
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

-- | Access memory through a cache, reading backing memory on a miss.
cacheAccess ::
  (CacheOps c) =>
  (Address -> Word) ->
  Address ->
  c ->
  (c, Word, CacheOutcome)
cacheAccess readBacking addr c = case cacheLookup addr c of
  Just w -> (c, w, Hit)
  Nothing ->
    let w = readBacking addr
     in (cacheInsert addr w c, w, Miss)

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
