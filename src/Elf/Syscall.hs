{-# LANGUAGE LambdaCase #-}

module Elf.Syscall (handleSyscall, ProgramExitException(..)) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize, zip, (++))
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (||))
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
import Core.RegFile

-- Custom exception type for non-zero program exit codes
newtype ProgramExitException = ProgramExitException Int32
  deriving (Show, Eq)

instance Exception ProgramExitException

-- | File descriptor for /dev/random and /dev/urandom
randomFd :: BitVector 32
randomFd = 67

handleSyscall :: (MonadIO m, MonadMemory m) => Core.State -> m (Maybe Word)
handleSyscall s = do
  let rf = Core.stateRegFile s
      reg n = lookupRF n rf
  case reg 17 of
    64 -> do -- write
      let fd = reg 10
          buf = reg 11
          count = reg 12
      when (fd == 1 || fd == 2) $ do
        str <- readStringFromMemory (fromIntegral buf) (fromIntegral count)
        liftIO $ putStr str
      pure $ Just count
    93 -> do -- exit
      let code = reg 10
      when (code /= 0) $
        liftIO $ throwIO $ ProgramExitException (bitCoerce code :: Int32)
      pure Nothing
    80 -> -- fstat
      case reg 10 of
        a0 | a0 /= randomFd ->
          pure $ Just (bitCoerce (-1 :: Int32))
        _ -> do
          let buf = bitCoerce (reg 11)
          forM_ [0 .. 0x70 - 1] $ \offset -> ramWrite (buf + offset) Byte 0
          ramWrite (buf + 8) Half 0x67      -- st_dev
          ramWrite (buf + 16) Half 0x2000   -- st_mode = S_IFCHR
          pure $ Just 0
    63 -> -- read
      case reg 10 of
        a0 | a0 /= randomFd ->
          pure $ Just (bitCoerce (-1 :: Int32))
        _ -> do
          let buf = reg 11
              count = reg 12
          entropy <- liftIO $ getEntropy (fromIntegral count)
          forM_ (zip [0 ..] (BS.unpack entropy)) $ \(i, byte) ->
            ramWrite (bitCoerce buf + i) Byte (fromIntegral byte)
          pure $ Just (bitCoerce count)
    57 -> -- close
      pure $ Just 0
    214 -> -- brk
      pure $ Just (bitCoerce (-1 :: Int32))
    1024 -> do -- open
      path <- readStringFromMemory (bitCoerce (reg 10)) 256
      pure $ Just $ case path of
        "/dev/random"  -> randomFd
        "/dev/urandom" -> randomFd
        _              -> bitCoerce (-1 :: Int32)
    676767 -> -- mark_memory_region: accepted and ignored; the core tracks no secrets
      pure $ Just 0
    n -> do
      liftIO $ putStrLn $ "Syscall: Unknown " ++ show (toInteger n)
      pure Nothing
