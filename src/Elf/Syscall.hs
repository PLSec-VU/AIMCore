module Elf.Syscall (handleSyscall, ProgramExitException(..)) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize, zip, (++))
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (||))
import Access
import Util
import Memory.Types
import Elf.ElfLoader
import Data.Functor.Identity
import Data.Int (Int32)
import Control.Monad (when, forM_, forM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (throwIO, throw, Exception)
import Types
import Numeric (showHex)
import System.Entropy
import qualified Data.ByteString as BS

import qualified Core
import RegFile

-- Custom exception type for non-zero program exit codes
data ProgramExitException = ProgramExitException Int32
  deriving (Show, Eq)

instance Exception ProgramExitException

-- | File descriptor for /dev/random and /dev/urandom
randomFd :: BitVector 32
randomFd = 67

handleSyscall :: (Access f, MonadIO m, MonadMemory m) => Core.State f -> m (Bool, Maybe (f Word))
handleSyscall s = do
  let rf = Core.stateRegFile s
  noSecrets (lookupRF 17 rf) (pure (True, Nothing)) $ \a17 -> case a17 of
    64 -> do -- write syscall
      noSecrets (lookupRF 10 rf) (pure (True, Nothing)) $ \fd ->
        noSecrets (lookupRF 11 rf) (pure (True, Nothing)) $ \buf ->
          noSecrets (lookupRF 12 rf) (pure (True, Nothing)) $ \count -> do
            when (fd == 1 || fd == 2) $ do -- stdout or stderr
              str <- readStringFromMemory (fromIntegral buf) (fromIntegral count)
              liftIO $ putStr str
            pure (True, Just (pure count))
    93 -> do -- exit
      noSecrets (lookupRF 10 rf) (pure (True, Nothing)) $ \code -> do
        if code /= 0
          then liftIO $ throwIO $ ProgramExitException (bitCoerce code :: Int32)
          else pure ()
        pure (False, Nothing)
    80 -> do -- fstat
      noSecrets (lookupRF 10 rf) (pure (True, Nothing)) $ \a0 ->
        if a0 /= randomFd then
          pure (True, Just (pure $ bitCoerce (-1 :: Int32)))
        else do
          noSecrets (lookupRF 11 rf) (pure (True, Nothing)) $ \buf' -> do
            let buf = bitCoerce buf'
            -- Write zeroed struct stat (size = 128 bytes)
            forM_ [0..0x70-1] $ \offset -> ramWrite (buf + offset) Byte 0
            ramWrite (buf+8)  Half 0x67 -- st_dev
            ramWrite (buf+16) Half 0x2000 -- st_mode = S_IFCHR
            pure (True, Just (pure 0))
    63 -> do -- read
      noSecrets (lookupRF 10 rf) (pure (True, Nothing)) $ \a0 ->
        if a0 /= randomFd then
          pure (True, Just (pure $ bitCoerce (-1 :: Int32)))
        else do
          noSecrets (lookupRF 11 rf) (pure (True, Nothing)) $ \buf ->
            noSecrets (lookupRF 12 rf) (pure (True, Nothing)) $ \count -> do
              entropy <- liftIO $ getEntropy (fromIntegral count)
              forM_ (zip [0..] (BS.unpack entropy)) $ \(i, byte) ->
                ramWrite (bitCoerce buf + i) Byte (fromIntegral byte)
              pure (True, Just (pure $ bitCoerce count))
    57 -> do -- close
      pure (True, Just (pure 0))
    214 -> do -- brk
      pure (True, Just (pure $ bitCoerce (-1 :: Int32)))
    1024 -> do -- open
      noSecrets (lookupRF 10 rf) (pure (True, Nothing)) $ \a0 -> do
        path <- readStringFromMemory (bitCoerce a0) 256
        case path of
          "/dev/random"  -> pure (True, Just (pure randomFd))
          "/dev/urandom" -> pure (True, Just (pure randomFd))
          _              -> liftIO (print path) *> pure (True, Just (pure $ bitCoerce (-1 :: Int32)))
    676767 -> do -- mark_memory_region syscall
      noSecrets (lookupRF 10 rf) (pure (True, Nothing)) $ \addr ->
        noSecrets (lookupRF 11 rf) (pure (True, Nothing)) $ \size ->
          noSecrets (lookupRF 12 rf) (pure (True, Nothing)) $ \level -> do
            let endAddr = addr + size - 1
            markMemoryRegion (fromIntegral addr) (fromIntegral endAddr) (level /= 0)
            pure (True, Just (pure 0))  -- return success
    n -> do
      liftIO $ putStrLn $ "Syscall: Unknown " ++ show (toInteger n)
      pure (True, Nothing)
