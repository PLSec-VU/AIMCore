{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Memory.Vec
  ( Mem (..),
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad.State
import Memory.Types
import Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

data Mem n = Mem
  { memRAM :: Vec n Byte
  }
  deriving (Eq, Show, Generic, NFDataX)

instance (KnownNat n, Monad m, MonadState (Mem n) m) => MonadMemory m where
  ramRead _isInstr addr _size = readWord addr <$> gets memRAM
  ramWrite addr size w = do
    ram <- gets memRAM
    modify $ \s -> s {memRAM = write size addr w ram}
  -- Identity implementation: no-op security functions
  markMemoryRegion _ _ _ = pure ()
  isMemorySecret _ = pure False
