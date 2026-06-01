{-# LANGUAGE LambdaCase #-}

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
      reg n = lookupRF n rf
  noSecrets (reg 17) (pure Nothing) $ \case
    64 -> do -- write
      noSecrets (reg 10) (pure Nothing) $ \fd ->
        noSecrets (reg 11) (pure Nothing) $ \buf ->
          noSecrets (reg 12) (pure Nothing) $ \count -> do
            when (fd == 1 || fd == 2) $ do
              str <- readStringFromMemory (fromIntegral buf) (fromIntegral count)
              liftIO $ putStr str
            pure $ Just (pure count)
    93 -> do -- exit
      noSecrets (reg 10) (pure Nothing) $ \code -> do
        when (code /= 0) $
          liftIO $ throwIO $ ProgramExitException (bitCoerce code :: Int32)
        pure Nothing
    80 -> do -- fstat
      noSecrets (reg 10) (pure Nothing) $ \case
        a0 | a0 /= randomFd ->
          pure $ Just (pure $ bitCoerce (-1 :: Int32))
        _ ->
          noSecrets (reg 11) (pure Nothing) $ \buf' -> do
            let buf = bitCoerce buf'
            forM_ [0 .. 0x70 - 1] $ \offset -> ramWrite (buf + offset) Byte 0
            ramWrite (buf + 8) Half 0x67      -- st_dev
            ramWrite (buf + 16) Half 0x2000   -- st_mode = S_IFCHR
            pure $ Just (pure 0)
    63 -> do -- read
      noSecrets (reg 10) (pure Nothing) $ \case
        a0 | a0 /= randomFd ->
          pure $ Just (pure $ bitCoerce (-1 :: Int32))
        _ ->
          noSecrets (reg 11) (pure Nothing) $ \buf ->
            noSecrets (reg 12) (pure Nothing) $ \count -> do
              entropy <- liftIO $ getEntropy (fromIntegral count)
              forM_ (zip [0 ..] (BS.unpack entropy)) $ \(i, byte) ->
                ramWrite (bitCoerce buf + i) Byte (fromIntegral byte)
              pure $ Just (pure $ bitCoerce count)
    57 -> -- close
      pure $ Just (pure 0)
    214 -> -- brk
      pure $ Just (pure $ bitCoerce (-1 :: Int32))
    1024 -> do -- open
      noSecrets (reg 10) (pure Nothing) $ \a0 -> do
        path <- readStringFromMemory (bitCoerce a0) 256
        pure $ Just $ pure $ case path of
          "/dev/random"  -> randomFd
          "/dev/urandom" -> randomFd
          _              -> bitCoerce (-1 :: Int32)
    676767 -> do -- mark_memory_region
      noSecrets (reg 10) (pure Nothing) $ \addr ->
        noSecrets (reg 11) (pure Nothing) $ \size ->
          noSecrets (reg 12) (pure Nothing) $ \level -> do
            let endAddr = addr + size - 1
            markMemoryRegion (fromIntegral addr) (fromIntegral endAddr) (level /= 0)
            pure $ Just (pure 0)
    n -> do
      liftIO $ putStrLn $ "Syscall: Unknown " ++ show (toInteger n)
      pure Nothing
