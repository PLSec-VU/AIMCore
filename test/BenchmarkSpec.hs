{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BenchmarkSpec (benchmarkTests) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Control.Exception (throw, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Core as Core
import Data.Monoid (First (getFirst))
import Elf.ElfLoader
import Elf.Instrument (elfInstrument)
import Elf.Memory
import Numeric (showHex)
import Simulate
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Util
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Prelude as P

-- | Data type for benchmark test configuration
data BenchmarkTest = BenchmarkTest
  { benchmarkPath :: String,
    benchmarkInstrument :: forall m. (MonadIO m, MonadMemory m) => Instrument m
  }

cryptoInstrument :: (MonadIO m, MonadMemory m) => Bool -> Instrument m
cryptoInstrument shouldLog i s o step = do
  when shouldLog $ do
    let pc = Core.stateExPc s
    -- liftIO $ print $ "stateExPc=0x" P.++ showHex pc "" P.++ " stateExInstr=0x" P.++ show (Core.stateExInstr s)
    when (pc == 0x1e05c) $ do
      s9 <- regRead 25
      s11 <- regRead 27
      liftIO $ print $ "stateExPc=0x" P.++ showHex pc "" P.++ " stateExInstr=0x" P.++ show (Core.stateExInstr s) P.++ " s9=0x" P.++ showHex s9 "" P.++ " s11=0x" P.++ showHex s11 ""
  elfInstrument i s o step

testSuiteInstrument :: (MonadIO m, MonadMemory m) => Bool -> Instrument m
testSuiteInstrument shouldLog i s o step = do
  when shouldLog $ do
    let pc = Core.stateExPc s
    gp <- regRead 3
    a4 <- regRead 14
    t2 <- regRead 7
    liftIO $ print $ "stateExPc=0x" P.++ showHex pc "" P.++ " stateExInstr=0x" P.++ show (Core.stateExInstr s) P.++ " a4=0x" P.++ showHex a4 "" P.++ " t2=0x" P.++ showHex t2 "" P.++ " gp=0x" P.++ showHex gp ""
  case getFirst $ Core.outSyscall o of
    Just True -> do
      gp <- toInteger <$> regRead 3
      a0 <- toInteger <$> regRead 10
      if gp == 1 && a0 == 0
        then pure False
        else do
          regFile <- getRegFile
          liftIO $ throwIO (userError $ "Test suite exited with failure: " P.++ show regFile)
    _ -> pure True

-- | Create a test case for a benchmark binary
mkBenchmarkTest :: String -> BenchmarkTest -> TestTree
mkBenchmarkTest testName _benchmark =
  testCase testName $ do
    elf <- readElf (benchmarkPath _benchmark)
    entryOffset <- startAddr elf
    ioMem <- newIOMem elf

    -- Run the simulator with IOMem and MonadMemory interface
    _ <- runIOMemT ioMem $ do
      loadProgram elf
      runElf
        (benchmarkInstrument _benchmark)
        (simulator @(IOMemT IO))
          { circuitState =
              Core.init
                { Core.stateFePc = fromIntegral entryOffset
                }
          }

    pure ()

-- | Main benchmark test group
benchmarkTests :: TestTree
benchmarkTests =
  testGroup
    "Software Benchmark Tests"
    [ testGroup
        "Cryptographic Benchmark Execution"
        [ testGroup
            "Timing attacks"
            [ mkBenchmarkTest
                "Vulnerable strcmp timing attack"
                BenchmarkTest
                  { benchmarkPath = "benchmark/bench_vuln_strcmp",
                    benchmarkInstrument = cryptoInstrument False
                  }
            ],
          testGroup
            "LibSodium"
            [ mkBenchmarkTest
                "ChaCha20 execution"
                BenchmarkTest
                  { benchmarkPath = "benchmark/bench_chacha20",
                    benchmarkInstrument = cryptoInstrument False
                  },
              mkBenchmarkTest
                "BLAKE2b execution"
                BenchmarkTest
                  { benchmarkPath = "benchmark/bench_blake2b",
                    benchmarkInstrument = cryptoInstrument False
                  },
              mkBenchmarkTest
                "SHA-256 execution"
                BenchmarkTest
                  { benchmarkPath = "benchmark/bench_sha256",
                    benchmarkInstrument = cryptoInstrument False
                  }
                  -- This takes 20 minutes to run, so disabled for now
                  -- mkBenchmarkTest "X25519 execution" BenchmarkTest
                  --   { benchmarkPath = "benchmark/bench_x25519"
                  --   , benchmarkInstrument = cryptoInstrument True
                  --   }
            ]
        ],
      testGroup
        "Test suite benchmark Execution"
        [ mkBenchmarkTest
            "rv32ui-p-add"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-add",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-addi"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-addi",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-and"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-and",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-andi"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-andi",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-auipc"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-auipc",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-beq"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-beq",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-bge"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-bge",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-bgeu"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-bgeu",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-blt"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-blt",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-bltu"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-bltu",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-bne"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-bne",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-fence_i"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-fence_i",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-jal"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-jal",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-jalr"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-jalr",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-lb"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-lb",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-lbu"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-lbu",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-ld_st"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-ld_st",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-lh"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-lh",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-lhu"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-lhu",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-lui"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-lui",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-lw"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-lw",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-ma_data"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-ma_data",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-or"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-or",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-ori"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-ori",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sb"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sb",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sh"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sh",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-simple"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-simple",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sll"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sll",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-slli"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-slli",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-slt"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-slt",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-slti"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-slti",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sltiu"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sltiu",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sltu"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sltu",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sra"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sra",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-srai"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-srai",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-srl"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-srl",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-srli"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-srli",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-st_ld"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-st_ld",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sub"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sub",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-sw"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-sw",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-xor"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-xor",
                benchmarkInstrument = testSuiteInstrument False
              },
          mkBenchmarkTest
            "rv32ui-p-xori"
            BenchmarkTest
              { benchmarkPath = "test/rv32ui/rv32ui-p-xori",
                benchmarkInstrument = testSuiteInstrument False
              }
        ]
    ]