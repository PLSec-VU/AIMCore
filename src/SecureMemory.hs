{-# LANGUAGE UndecidableInstances #-}

module SecureMemory
  ( SecureMem (..),
    MemoryRegion (..),
    initSecureMem,
  )
where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad.State
import Data.List (find)
import RegFile
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

-- | A memory region with security classification
data MemoryRegion = MemoryRegion
  { regionStart :: Address,
    regionEnd :: Address,
    regionIsSecret :: Bool
  }
  deriving (Eq, Show, Generic, NFDataX)

-- | Secure memory that tracks security levels
data SecureMem n = SecureMem
  { secureRAM :: Vec n Byte,
    secureRF :: RegFile,
    -- | List of memory regions and their security levels
    securityRegions :: [MemoryRegion]
  }
  deriving (Eq, Show, Generic, NFDataX)

-- | Initialize secure memory with no security regions
initSecureMem :: (KnownNat n) => Vec n Byte -> RegFile -> SecureMem n
initSecureMem ram rf = SecureMem
  { secureRAM = ram,
    secureRF = rf,
    securityRegions = []
  }

instance (KnownNat n, Monad m, MonadState (SecureMem n) m) => MonadMemory m where
  getRegFile = gets secureRF
  putRegFile rf = modify $ \s -> s {secureRF = rf}
  ramRead addr = readWord addr <$> gets secureRAM
  ramWrite addr size w = do
    ram <- gets secureRAM
    modify $ \s -> s {secureRAM = write size addr w ram}
  
  -- | Mark a region of memory as public (False) or secret (True)
  markMemoryRegion startAddr endAddr isSecret = do
    let region = MemoryRegion startAddr endAddr isSecret
    modify $ \s -> s {securityRegions = region : securityRegions s}
  
  -- | Check if a memory address is in a secret region
  isMemorySecret addr = do
    regions <- gets securityRegions
    pure $ any (isAddrInSecretRegion addr) regions
    where
      isAddrInSecretRegion :: Address -> MemoryRegion -> Bool
      isAddrInSecretRegion a (MemoryRegion start end secret) =
        secret && a >= start && a <= end