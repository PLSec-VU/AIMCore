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
import Numeric (showHex)

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

testSuiteInstrument :: (MonadIO m, MonadMemory m) => Instrument m
testSuiteInstrument i s o step = do
  case getFirst $ Core.outSyscall o of
    Just True -> do
      gp <- toInteger <$> regRead 3
      a0 <- toInteger <$> regRead 10
      if gp == 1 && a0 == 0
        then pure False
        else liftIO $ throwIO (userError $ "Test suite exited with failure: gp=" P.++ show gp P.++ " a0=" P.++ show a0)
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
    "Software Benchmark Tests"
    [ testGroup
        "Cryptographic Benchmark Execution"
        [ testGroup
            "Timing attacks"
            [
              mkBenchmarkTest "Vulnerable strcmp timing attack" BenchmarkTest
                { benchmarkPath = "benchmark/bench_vuln_strcmp"
                , benchmarkInstrument = cryptoInstrument
                }
             ],
          testGroup
            "LibSodium"
            [
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
            ]
        ],
      testGroup
        "Test suite benchmark Execution"
        [
          mkBenchmarkTest "rv32ui-p-add" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-add"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-addi" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-addi"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-and" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-and"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-andi" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-andi"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-auipc" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-auipc"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-beq" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-beq"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-bge" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-bge"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-bgeu" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-bgeu"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-blt" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-blt"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-bltu" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-bltu"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-bne" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-bne"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-fence_i" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-fence_i"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-jal" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-jal"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-jalr" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-jalr"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-lb" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-lb"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-lbu" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-lbu"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-ld_st" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-ld_st"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-lh" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-lh"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-lhu" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-lhu"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-lui" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-lui"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-lw" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-lw"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-ma_data" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-ma_data"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-or" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-or"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-ori" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-ori"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sb" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sb"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sh" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sh"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-simple" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-simple"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sll" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sll"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-slli" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-slli"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-slt" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-slt"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-slti" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-slti"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sltiu" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sltiu"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sltu" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sltu"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sra" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sra"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-srai" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-srai"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-srl" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-srl"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-srli" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-srli"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-st_ld" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-st_ld"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sub" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sub"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-sw" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-sw"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-xor" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-xor"
            , benchmarkInstrument = testSuiteInstrument
            },
          mkBenchmarkTest "rv32ui-p-xori" BenchmarkTest
            { benchmarkPath = "test/rv32ui/rv32ui-p-xori"
            , benchmarkInstrument = testSuiteInstrument
            }
        ]
    ]