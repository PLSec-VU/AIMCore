{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BenchmarkSpec (benchmarkTests) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Data.ByteString.Lazy as BSL
import qualified Prelude as P
import Control.Monad (forM_)
import Numeric (showHex)
import Simulate
import Util
import qualified Core as Core
import Control.Monad.Reader
import ElfLoader
import Memory
import GHC.Base (when)
import Data.Monoid (First(getFirst))

-- | Data type for benchmark test configuration
data BenchmarkTest = BenchmarkTest
  { benchmarkPath :: String
  }
  deriving (Show, Eq)

watch' :: forall m. (MonadMemory m, MonadIO m) => CircuitSim m Core.Input Core.State Core.Output -> m ()
watch' c = watchWithStep (0 :: Int) c
  where
    watchWithStep step sim = do
      (s', o, mi') <- run1 sim
      -- let pc = Core.stateExPc s'
      -- liftIO $ print $ "stateExPc=0x" P.++ showHex pc "" P.++ " stateExInstr=0x" P.++ show (Core.stateExInstr s')
      cont <- case getFirst $ Core.outSyscall o of
          Just True -> handleSyscall
          _ -> pure True
      case mi' of
        Just i' | cont -> do
          watchWithStep (step + 1) $ sim {circuitInput = i', circuitState = s'}
        _ -> pure ()
    handleSyscall :: m Bool
    handleSyscall = regRead 17 >>= \case
      93 -> do -- exit
        pure False
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
      watch' (simulator @(IOMemT IO)) {
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
        "Benchmark Execution"
        [ mkBenchmarkTest "Vulnerable strcmp timing attack" BenchmarkTest
            { benchmarkPath = "benchmark/bench_vuln_strcmp"
            }
        ---   mkBenchmarkTest "ChaCha20 execution" BenchmarkTest
        ---   { benchmarkPath = "benchmark/bench_chacha20"
        ---   },
        --   mkBenchmarkTest "X25519 execution" BenchmarkTest
        --     { benchmarkPath = "benchmark/bench_x25519"
        --     },
        --   mkBenchmarkTest "SHA-256 execution" BenchmarkTest
        --     { benchmarkPath = "benchmark/bench_sha256"
        --     },
        --   mkBenchmarkTest "BLAKE2b execution" BenchmarkTest
        --     { benchmarkPath = "benchmark/bench_blake2b"
        --     }
        ]
    ]