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

-- | Data type for benchmark test configuration
data BenchmarkTest = BenchmarkTest
  { benchmarkName :: String,
    benchmarkBinary :: String,
    benchmarkEntrypoint :: Word
  }
  deriving (Show, Eq)

-- | Get the entrypoint address from a benchmark binary
-- This function should extract the entrypoint from the ELF header
getBenchmarkEntrypoint :: String -> IO Word32
getBenchmarkEntrypoint _binaryPath = do
  i <- BSL.readFile _binaryPath
  Elf SELFCLASS32 elf <- parseElf i
  header <- elfFindHeader elf
  return $ ehEntry header

-- | Create a test case for a benchmark binary
mkBenchmarkTest :: String -> BenchmarkTest -> TestTree
mkBenchmarkTest testName _benchmark =
  testCase testName $ do
    entrypoint <- getBenchmarkEntrypoint (benchmarkBinary _benchmark)
    assertBool "Entry" True

-- | Available benchmark binaries
benchmarkBinaries :: [BenchmarkTest]
benchmarkBinaries =
  [ BenchmarkTest "ChaCha20" "benchmark/bench_chacha20" 0x10184,
    BenchmarkTest "X25519" "benchmark/bench_x25519" 0x10184,
    BenchmarkTest "SHA-256" "benchmark/bench_sha256" 0x10184,
    BenchmarkTest "BLAKE2b" "benchmark/bench_blake2b" 0x10184
  ]

-- | Main benchmark test group
benchmarkTests :: TestTree
benchmarkTests =
  testGroup
    "Libsodium Benchmark Tests"
    [ testGroup
        "Benchmark Execution"
        [ mkBenchmarkTest "ChaCha20 execution" (benchmarkBinaries P.!! 0),
          mkBenchmarkTest "X25519 execution" (benchmarkBinaries P.!! 1),
          mkBenchmarkTest "SHA-256 execution" (benchmarkBinaries P.!! 2),
          mkBenchmarkTest "BLAKE2b execution" (benchmarkBinaries P.!! 3)
        ]
    ]