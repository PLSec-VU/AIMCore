{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Memory.ST
  ( STMemory (..),
    STMemoryT (..),
    runSTMemoryT,
    runSTSim,
    newSTMemory,
    runUntilHalt,
  )
where

import Access (Access)
import Clash.Prelude hiding (Ordering (..), Word, init, lift)
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.ST (RealWorld, ST, stToIO)
import Core
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Word (Word8)
import Memory.Types
import Types
import Util (CircuitSim (..), run1)
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))
import qualified Prelude as P

-- | Memory state in ST: a single byte array that instruction fetches, loads
-- and stores all share, so a store to a code address is seen by later fetches
-- (as in "Memory.Vec", "Memory.IO" and the hardware RAM).
data STMemory s = STMemory
  { -- | The memory array.
    stData :: STUArray s Address Word8,
    -- | Base address of the data array.
    stDataBase :: Address,
    -- | Initial PC from ELF.
    stEntry :: Address
  }

-- | STMemory monad transformer.
newtype STMemoryT s m a = STMemoryT {unSTMemoryT :: ReaderT (STMemory s) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (STMemory s), MonadTrans)

runSTMemoryT :: STMemory s -> STMemoryT s m a -> m a
runSTMemoryT mem (STMemoryT m) = runReaderT m mem

instance {-# OVERLAPPING #-} (MonadIO m) => MonadMemory (STMemoryT RealWorld m) where
  ramRead addr size = do
    st <- ask
    let offset = addr - stDataBase st
    if offset < 0x1000000 && offset >= 0
      then liftIO $ stToIO $ readDataArray offset size (stData st)
      else pure 0
  ramWrite addr size w = do
    st <- ask
    let offset = addr - stDataBase st
    when (offset < 0x1000000 && offset >= 0) $
      liftIO $
        stToIO $
          writeDataArray offset size w (stData st)

-- | Read a sized value from the data array.
readDataArray :: Address -> Size -> STUArray s Address Word8 -> ST s Word
readDataArray addr size arr = case size of
  Byte -> do
    b0 <- readArray arr addr
    pure $ zeroExtend (bitCoerce b0 :: Byte)
  Half -> do
    b0 <- readArray arr addr
    b1 <- readArray arr (addr + 1)
    pure $ zeroExtend ((bitCoerce b1 :: Byte) ++# (bitCoerce b0 :: Byte))
  Word -> do
    b0 <- readArray arr addr
    b1 <- readArray arr (addr + 1)
    b2 <- readArray arr (addr + 2)
    b3 <- readArray arr (addr + 3)
    pure $ (bitCoerce b3 :: Byte) ++# (bitCoerce b2 :: Byte) ++# (bitCoerce b1 :: Byte) ++# (bitCoerce b0 :: Byte)

-- | Write a sized value to the data array.
writeDataArray :: Address -> Size -> Word -> STUArray s Address Word8 -> ST s ()
writeDataArray addr size w arr = case size of
  Byte -> writeArray arr addr (bitCoerce $ slice d7 d0 w)
  Half -> do
    writeArray arr addr (bitCoerce $ slice d7 d0 w)
    writeArray arr (addr + 1) (bitCoerce $ slice d15 d8 w)
  Word -> do
    writeArray arr addr (bitCoerce $ slice d7 d0 w)
    writeArray arr (addr + 1) (bitCoerce $ slice d15 d8 w)
    writeArray arr (addr + 2) (bitCoerce $ slice d23 d16 w)
    writeArray arr (addr + 3) (bitCoerce $ slice d31 d24 w)

-- | Create a new STMemory.
newSTMemory :: forall s. Address -> Address -> ST s (STMemory s)
newSTMemory entry base = do
  -- Use a smaller 16MB array to start with, as RISC-V tests are small.
  let dataSize = 0x1000000
  arr <- newArray (0, dataSize) 0
  pure $ STMemory arr base entry

-- | Run a simulation in the ST monad.
runSTSim :: forall i s o a. (forall st. CircuitSim (ST st) i s o -> ST st a) -> CircuitSim (ST s) i s o -> ST s a
runSTSim f sim = f sim

-- | Utility to run a full simulation until halt.
--   Uses deepseqX to ensure the state is fully evaluated and prevent thunk leaks.
runUntilHalt :: (NFDataX (f Word), Generic (f Word), MonadMemory m) => CircuitSim m (Core.Input f) (Core.State f) (Core.Output f) -> m (Core.State f)
runUntilHalt sim = go sim
  where
    go sim' = do
      (s', _, mi') <- run1 sim'
      -- Use deepseqX to fully evaluate the state and avoid memory leaks
      deepseqX s' $ case mi' of
        Nothing -> pure s'
        Just i' -> i' `seq` go (sim' {circuitInput = i', circuitState = s'})
