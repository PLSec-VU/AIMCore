{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BenchmarkSpec (benchmarkTests) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import "uc-risc-v" Types
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import Data.ByteString.Lazy as BSL
import qualified Prelude as P
import Data.Elf
import Data.Elf.Headers
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Clash.Sized.Vector (unsafeFromList)
import qualified GHC.TypeLits as TL
import Data.Data (Proxy (Proxy))
import Control.Monad (when)
import Simulate (simResult, Mem (..), simulator)
import RegFile (initRF)
import Util (result, CircuitSim (circuitState))
import Control.Monad.State (evalState, State)
import qualified Core as Core
import Clash.Class.BitPack (bitCoerce)
import System.Timeout (timeout)

-- | Data type for benchmark test configuration
data BenchmarkTest = BenchmarkTest
  { benchmarkPath :: String
  }
  deriving (Show, Eq)

type SPACE_SIZE = 4194304 -- 4MB

-- | Create a test case for a benchmark binary
mkBenchmarkTest :: String -> BenchmarkTest -> TestTree
mkBenchmarkTest testName _benchmark =
  testCase testName $ do
    prog <- BSL.readFile (benchmarkPath _benchmark)
    when (BSL.length prog >= fromIntegral (natVal (Proxy :: Proxy SPACE_SIZE))) $
      error "Benchmark binary too large"
    Elf SELFCLASS32 elf <- parseElf prog
    header <- elfFindHeader elf
    let mem :: Mem SPACE_SIZE = Mem {
      memRAM = bitCoerce <$> unsafeFromList $ BSL.unpack prog P.++ P.repeat 0,
      memRF = initRF
    }
    let resultRam = evalState (result simulator {
      circuitState = Core.init {
        Core.stateFePc = fromIntegral $ ehEntry header
      }
    }) mem
    ok <- timeout 10000000
      $ BSL.writeFile (benchmarkPath _benchmark P.++ ".core")
      $ BSL.pack
      $ bitCoerce <$> toList resultRam
    assertBool "Benchmark did not complete in time" (isJust ok)

-- | Main benchmark test group
benchmarkTests :: TestTree
benchmarkTests =
  testGroup
    "Libsodium Benchmark Tests"
    [ testGroup
        "Benchmark Execution"
        [ mkBenchmarkTest "ChaCha20 execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_chacha20"
            },
          mkBenchmarkTest "X25519 execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_x25519"
            },
          mkBenchmarkTest "SHA-256 execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_sha256"
            },
          mkBenchmarkTest "BLAKE2b execution" BenchmarkTest
            { benchmarkPath = "benchmark/bench_blake2b"
            }
        ]
    ]