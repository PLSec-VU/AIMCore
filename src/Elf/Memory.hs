
module Elf.Memory where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Types
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Prelude as P
import Data.Word (Word8)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad (forM_, filterM)
import Numeric (showHex)
import RegFile (initRF, RegFile, modifyRF)
import Util
import Control.Monad.Reader
import GHC.IORef
import Data.Traversable (forM)
import Data.Array.IO
import Control.Exception.Base (throwIO)
import Data.Maybe (listToMaybe)
import Data.Elf (Elf)
import Elf.ElfLoader (baseAddr, loadElf)
import qualified Data.ByteString.Lazy as BSL

-- | IO-based memory implementation for testing.
data IOMem = IOMem
  { ioMemRF :: IORef RegFile,
    -- | List of RAM arrays that store writable memory regions.
    ioMemRAM :: IOUArray Int Word8
  }
  deriving Eq

-- Newtype wrapper to avoid overlapping instances
newtype IOMemT m a = IOMemT (ReaderT IOMem m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader IOMem)

instance MonadTrans IOMemT where
  lift = IOMemT . lift

runIOMemT :: IOMem -> IOMemT m a -> m a
runIOMemT iomem (IOMemT m) = runReaderT m iomem

newIOMem :: MonadIO m => Elf -> m IOMem
newIOMem elf = do
  let memSize = 0x4000000
  base <- liftIO $ fromIntegral <$> baseAddr elf
  let sp = base + memSize - 0x1000000
  rfRef <- liftIO $ newIORef $ modifyRF 2 (fromIntegral sp) initRF
  ramArray <- liftIO $ newArray (base, base+memSize) 0
  pure $ IOMem rfRef ramArray

loadProgram :: (MonadIO m, MonadReader IOMem m) => Elf -> m ()
loadProgram elf = do
  ramArray <- asks ioMemRAM
  loadElf elf $ \addr body ->
    forM_ (P.zip [addr..] (BSL.unpack body)) $ \(i, byte) ->
      liftIO $ writeArray ramArray (fromIntegral i) byte

instance {-# OVERLAPPING #-} MonadMemory (IOMemT IO) where
  getRegFile = do
    asks ioMemRF >>= lift . readIORef
  putRegFile rf = do
    rfRef <- asks ioMemRF
    lift $ writeIORef rfRef rf
    -- liftIO $ putStrLn $ "putRegFile: " P.++ show rf
  ramRead addr = do
    ramArray <- asks ioMemRAM
    bytes <- forM [0 .. 3] $ \i -> do
      w8 <- lift $ readArray ramArray (fromIntegral addr + i)
      pure $ bitCoerce w8
    let word = readWord @4 0 $ unsafeFromList bytes
    -- liftIO $ putStrLn $ "ramRead addr=0x" P.++ showHex addr "" P.++ " => " P.++ show (flip showHex "" <$> bytes)
    pure word
  ramWrite addr size w = do
    ramArray <- asks ioMemRAM
    let b0 = slice d7 d0 w      -- Extract bits 7-0 (least significant byte)
        b1 = slice d15 d8 w     -- Extract bits 15-8
        b2 = slice d23 d16 w    -- Extract bits 23-16
        b3 = slice d31 d24 w    -- Extract bits 31-24 (most significant byte)
        writeBytes bytes = do
          forM_ (P.zip [0..] bytes) $ \(i, byte) ->
            lift $ writeArray ramArray (fromIntegral addr + i) byte
    case size of
      Byte -> writeBytes $ bitCoerce <$> [b0]           -- Write 1 byte
      Half -> writeBytes $ bitCoerce <$> [b0, b1]       -- Write 2 bytes (halfword)
      Word -> writeBytes $ bitCoerce <$> [b0, b1, b2, b3] -- Write 4 bytes (word)
  -- IOMemT implementation: no-op security functions (like Identity)
  markMemoryRegion _ _ _ = pure ()
  isMemorySecret _ = pure False
