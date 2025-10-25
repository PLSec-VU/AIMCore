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
import Data.Data (Proxy (Proxy))
import Control.Monad (forM_)
import Numeric (showHex)
import Simulate
import RegFile (initRF, RegFile)
import Util
import qualified Core as Core
import Data.Array.IO (IOUArray, hPutArray, newArray, readArray, writeArray)
import Control.Monad.Reader
import GHC.IORef
import Data.Traversable (forM)
import System.IO (withBinaryFile, IOMode(..))
import ElfLoader
import Memory
import Types (Size(Byte))

-- | Data type for benchmark test configuration
data BenchmarkTest = BenchmarkTest
  { benchmarkPath :: String
  }
  deriving (Show, Eq)

watch' :: (MonadIO m) => CircuitSim m i Core.State o -> m [(Core.State, o, Maybe i)]
watch' c = watchWithStep (0 :: Int) c
  where
    watchWithStep step sim = do
      (s', o, mi') <- run1 sim
      liftIO $ print $ "stateExPc=0x" P.++ showHex (Core.stateExPc s') "" P.++ " stateExInstr=0x" P.++ show (Core.stateExInstr s') 

      --when (step `mod` 1000 == 0) $
      --  liftIO $ print s'
      case mi' of
        Just i' -> do
          rest <- watchWithStep (step + 1) $ sim {circuitInput = i', circuitState = s'}
          pure $ (s', o, mi') : rest
        _ -> pure [(s', o, mi')]

-- | Create a test case for a benchmark binary
mkBenchmarkTest :: String -> BenchmarkTest -> TestTree
mkBenchmarkTest testName _benchmark =
  testCase testName $ do
    elf <- readElf (benchmarkPath _benchmark)
    entryOffset <- startAddr elf
    ioMem <- newIOMem
      [ (0, 0x400000) -- Code and data segment
      ]

    -- Run the simulator with IOMem and MonadMemory interface
    _ <- runIOMemT ioMem $ do
      loadElf elf $ \addr body ->
        forM_ (P.zip [addr..] (BSL.unpack body)) $ \(i, byte) ->
          ramWrite i Byte (fromIntegral byte)
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
        [ mkBenchmarkTest "ChaCha20 execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_chacha20"
            }
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