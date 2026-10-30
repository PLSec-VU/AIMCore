module Elf.Syscall (handleSyscall, ProgramExitException(..)) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize, zip, (++))
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (||))
import Access
import Memory.Types
import Elf.ElfLoader
import Data.Int (Int32)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (throwIO, Exception)
import System.Entropy
import qualified Data.ByteString as BS

import Types
import qualified Core
import RegFile

-- Custom exception type for non-zero program exit codes
newtype ProgramExitException = ProgramExitException Int32
  deriving (Show, Eq)

instance Exception ProgramExitException

-- | File descriptor for /dev/random and /dev/urandom
randomFd :: BitVector 32
randomFd = 67

handleSyscall :: (Access f, MonadIO m, MonadMemory m) => Core.State f -> m (Maybe (f Word))
handleSyscall s = do
  let rf = Core.stateRegFile s
      reg n = unAccess (lookupRF n rf)
      a0 = reg 10
      a1 = reg 11
      a2 = reg 12
  case reg 17 of
    64 -> do -- write
      when (a0 == 1 || a0 == 2) $ do
        str <- readStringFromMemory (fromIntegral a1) (fromIntegral a2)
        liftIO $ putStr str
      pure $ Just (pure a2)
    93 -> do -- exit
      when (a0 /= 0) $
        liftIO $ throwIO $ ProgramExitException (bitCoerce a0 :: Int32)
      pure Nothing
    80 | a0 /= randomFd -> -- fstat
      pure $ Just (pure $ bitCoerce (-1 :: Int32))
    80 -> do
      let buf = bitCoerce a1
      forM_ [0 .. 0x70 - 1] $ \offset -> ramWrite (buf + offset) Byte 0
      ramWrite (buf + 8) Half 0x67      -- st_dev
      ramWrite (buf + 16) Half 0x2000   -- st_mode = S_IFCHR
      pure $ Just (pure 0)
    63 | a0 /= randomFd -> -- read
      pure $ Just (pure $ bitCoerce (-1 :: Int32))
    63 -> do
      entropy <- liftIO $ getEntropy (fromIntegral a2)
      forM_ (zip [0 ..] (BS.unpack entropy)) $ \(i, byte) ->
        ramWrite (bitCoerce a1 + i) Byte (fromIntegral byte)
      pure $ Just (pure $ bitCoerce a2)
    57 -> -- close
      pure $ Just (pure 0)
    214 -> -- brk
      pure $ Just (pure $ bitCoerce (-1 :: Int32))
    1024 -> do -- open
      path <- readStringFromMemory (bitCoerce a0) 256
      pure $ Just $ pure $ case path of
        "/dev/random"  -> randomFd
        "/dev/urandom" -> randomFd
        _              -> bitCoerce (-1 :: Int32)
    676767 -> -- mark_memory_region, kept so older benchmark binaries still run
      pure $ Just (pure 0)
    n -> do
      liftIO $ putStrLn $ "Syscall: Unknown " ++ show (toInteger n)
      pure Nothing
