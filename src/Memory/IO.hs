module Memory.IO
  ( IOMem (..),
    IOMemT (..),
    runIOMemT,
    newIOMem,
    loadProgram,
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad (forM_)
import Control.Monad.Reader
import Data.Array.IO
import Data.Elf (Elf)
import Data.Traversable (forM)
import Data.Word (Word8)
import Elf.ElfLoader (baseAddr, loadElf)
import Memory.Types
import Types
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Prelude as P

-- | IO-based memory implementation for testing.
data IOMem = IOMem
  { ioMemInitSP :: Word,
    -- | List of RAM arrays that store writable memory regions.
    ioMemRAM :: IOUArray Int Word8
  }
  deriving (Eq)

-- Newtype wrapper to avoid overlapping instances
newtype IOMemT m a = IOMemT (ReaderT IOMem m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader IOMem)

instance MonadTrans IOMemT where
  lift = IOMemT . lift

runIOMemT :: IOMem -> IOMemT m a -> m a
runIOMemT iomem (IOMemT m) = runReaderT m iomem

newIOMem :: (MonadIO m) => Elf -> m IOMem
newIOMem elf = do
  let memSize = 0xf000000
  base <- liftIO $ fromIntegral <$> baseAddr elf
  let sp = base + memSize - 0x1000000
  ramArray <- liftIO $ newArray (base, base + memSize) 0
  pure $ IOMem (fromIntegral sp) ramArray

loadProgram :: (MonadMemory m) => Elf -> m ()
loadProgram elf = loadElf elf

instance {-# OVERLAPPING #-} MonadMemory (IOMemT IO) where
  ramRead addr _size = do
    ramArray <- asks ioMemRAM
    bytes <- forM [0 .. 3] $ \i -> do
      w8 <- lift $ readArray ramArray (fromIntegral addr + i)
      pure $ bitCoerce w8
    let word = readWord @4 0 $ unsafeFromList bytes
    -- liftIO $ putStrLn $ "ramRead addr=0x" P.++ showHex addr "" P.++ " => " P.++ show (flip showHex "" <$> bytes)
    pure word
  ramWrite addr size w = do
    ramArray <- asks ioMemRAM
    let b0 = slice d7 d0 w -- Extract bits 7-0 (least significant byte)
        b1 = slice d15 d8 w -- Extract bits 15-8
        b2 = slice d23 d16 w -- Extract bits 23-16
        b3 = slice d31 d24 w -- Extract bits 31-24 (most significant byte)
        writeBytes bytes = do
          forM_ (P.zip [0 ..] bytes) $ \(i, byte) ->
            lift $ writeArray ramArray (fromIntegral addr + i) byte
    case size of
      Byte -> writeBytes [bitCoerce b0] -- Write 1 byte
      Half -> writeBytes [bitCoerce b0, bitCoerce b1] -- Write 2 bytes (halfword)
      Word -> writeBytes [bitCoerce b0, bitCoerce b1, bitCoerce b2, bitCoerce b3] -- Write 4 bytes (word)
