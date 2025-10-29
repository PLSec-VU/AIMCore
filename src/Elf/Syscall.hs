module Elf.Syscall (handleSyscall, ProgramExitException(..)) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import Util
import Elf.ElfLoader
import Data.Int (Int32)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (throwIO, throw, Exception)

-- Custom exception type for non-zero program exit codes
data ProgramExitException = ProgramExitException Int32
  deriving (Show, Eq)

instance Exception ProgramExitException

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
  80 -> do -- newfstat
    pure True
  57 -> do -- close
    pure True
  214 -> do -- brk
    regWrite 10 (-1)
    pure True
  n -> do
    -- liftIO $ putStrLn $ "Syscall: Unknown " P.++ show (toInteger n)
    pure True
