{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BenchmarkSpec (benchmarkTests) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import Simulate
import Util
import qualified Core as Core
import ElfLoader
import Memory
import Data.Int (Int32)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (throwIO)
import qualified Prelude as P
import Data.Monoid (First(getFirst))

-- | Data type for benchmark test configuration
data BenchmarkTest = BenchmarkTest
  { benchmarkPath :: String
  , benchmarkInstrument :: forall m. (MonadIO m, MonadMemory m) => Instrument m
  }

cryptoInstrument :: (MonadIO m, MonadMemory m) => Instrument m
cryptoInstrument i s o step = do
  -- when True $ do
  --   let pc = Core.stateExPc s'
  --   liftIO $ print $ "stateExPc=0x" ++ showHex pc "" ++ " stateExInstr=0x" ++ show (Core.stateExInstr s')
  case getFirst $ Core.outSyscall o of
    Just True -> handleSyscall
    _ -> pure True
  where
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
          then liftIO $ throwIO $ userError $ "Program exited with code " P.++ show (bitCoerce code :: Int32)
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
        liftIO $ putStrLn $ "Syscall: Unknown " P.++ show (toInteger n)
        pure True

-- | Create a test case for a benchmark binary
mkBenchmarkTest :: String -> BenchmarkTest -> TestTree
mkBenchmarkTest testName _benchmark =
  testCase testName $ do
    elf <- readElf (benchmarkPath _benchmark)
    entryOffset <- startAddr elf
    ioMem <- newIOMem 0x7000000 0x8000000

    -- Run the simulator with IOMem and MonadMemory interface
    _ <- runIOMemT ioMem $ do
      loadProgram elf
      runElf (benchmarkInstrument _benchmark) (simulator @(IOMemT IO)) {
        circuitState = Core.init {
          Core.stateFePc = fromIntegral entryOffset
        }
      }

    pure ()

-- | Main benchmark test group
benchmarkTests :: TestTree
benchmarkTests =
  testGroup
    "Libsodium Benchmark Tests"
    [ testGroup
        "Crypto benchmark Execution"
        [ mkBenchmarkTest "Vulnerable strcmp timing attack" BenchmarkTest
            { benchmarkPath = "benchmark/bench_vuln_strcmp"
            , benchmarkInstrument = cryptoInstrument
            },
          mkBenchmarkTest "ChaCha20 execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_chacha20"
            , benchmarkInstrument = cryptoInstrument
            },
          mkBenchmarkTest "BLAKE2b execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_blake2b"
            , benchmarkInstrument = cryptoInstrument
            },
          mkBenchmarkTest "SHA-256 execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_sha256"
            , benchmarkInstrument = cryptoInstrument
            }
          -- mkBenchmarkTest "X25519 execution" BenchmarkTest
          --   { benchmarkPath = "benchmark/bench_x25519"
          --   }
        ],
      testGroup
        "Test suite benchmark Execution"
        [
        ]
    ]