module Elf.Syscall (handleSyscall, ProgramExitException(..)) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize, zip, (++))
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (||))
import Util
import Elf.ElfLoader
import Data.Int (Int32)
import Control.Monad (when, forM_, forM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (throwIO, throw, Exception)
import Types
import Numeric (showHex)
import System.Entropy
import qualified Data.ByteString as BS

-- Custom exception type for non-zero program exit codes
data ProgramExitException = ProgramExitException Int32
  deriving (Show, Eq)

instance Exception ProgramExitException

-- | File descriptor for /dev/random and /dev/urandom
randomFd :: BitVector 32
randomFd = 67

handleSyscall :: (MonadIO m, MonadMemory m) => m Bool
handleSyscall = regRead 17 >>= \case
  64 -> do -- write syscall
    fd <- regRead 10    -- a0: file descriptor
    buf <- regRead 11   -- a1: buffer pointer
    count <- regRead 12 -- a2: number of bytes
    when (fd == 1 || fd == 2) $ do -- stdout or stderr
      str <- readStringFromMemory (fromIntegral buf) (fromIntegral count)
      liftIO $ putStr str
    regWrite 10 count
    pure True
  93 -> do -- exit
    code <- regRead 10
    if code /= 0
      then liftIO $ throwIO $ ProgramExitException (bitCoerce code :: Int32)
      else pure ()
    pure False
  80 -> do -- fstat
    regRead 10 >>= \case
      a0 | a0 /= randomFd ->
        regWrite 10 (-1)
      _ -> do
        buf <- bitCoerce <$> regRead 11
        -- Write zeroed struct stat (size = 128 bytes)
        forM_ [0..0x70-1] $ \offset -> ramWrite (buf + offset) Byte 0
        ramWrite (buf+8)  Half 0x67 -- st_dev
        ramWrite (buf+16) Half 0x2000 -- st_mode = S_IFCHR
        regWrite 10 0
    pure True
  63 -> do -- read
    regRead 10 >>= \case
      a0 | a0 /= randomFd ->
        regWrite 10 (-1)
      _ -> do
        buf <- regRead 11
        count <- regRead 12
        entropy <- liftIO $ getEntropy (fromIntegral count)
        forM_ (zip [0..] (BS.unpack entropy)) $ \(i, byte) ->
          ramWrite (bitCoerce buf + i) Byte (fromIntegral byte)
        regWrite 10 (bitCoerce count)
    pure True
  57 -> do -- close
    pure True
  214 -> do -- brk
    regWrite 10 (-1)
    pure True
  1024 -> do -- open
    a0 <- regRead 10
    path <- readStringFromMemory (bitCoerce a0) 256
    case path of
      "/dev/random"  -> regWrite 10 randomFd
      "/dev/urandom" -> regWrite 10 randomFd
      _              -> liftIO (print path) *> regWrite 10 (-1)
    pure True
  n -> do
    liftIO $ putStrLn $ "Syscall: Unknown " ++ show (toInteger n)
    pure True
