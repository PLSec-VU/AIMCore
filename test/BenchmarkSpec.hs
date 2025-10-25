{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BenchmarkSpec (benchmarkTests) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import "uc-risc-v" Types
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Data.ByteString.Lazy as BSL
import qualified Prelude as P
import Data.Word (Word8)
import Clash.Sized.Vector (unsafeFromList)
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

-- | Data type for benchmark test configuration
data BenchmarkTest = BenchmarkTest
  { benchmarkPath :: String
  }
  deriving (Show, Eq)

type SPACE_SIZE = 4194304 -- 4MB

data IOMem = IOMem
  { ioMemRAM :: IOUArray Int Word8,
    ioMemRF :: IORef RegFile
  }
  deriving Eq

-- Newtype wrapper to avoid overlapping instances
newtype IOMemT m a = IOMemT (ReaderT IOMem m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader IOMem)

instance MonadTrans IOMemT where
  lift = IOMemT . lift

runIOMemT :: IOMemT m a -> IOMem -> m a
runIOMemT (IOMemT m) = runReaderT m

instance {-# OVERLAPPING #-} MonadMemory SPACE_SIZE (IOMemT IO) where
  getRegFile = do
    IOMem _ rfRef <- ask
    lift $ readIORef rfRef
  putRegFile rf = do
    IOMem _ rfRef <- ask
    lift $ writeIORef rfRef rf
  ramRead addr = do
    IOMem ramArray _ <- ask
    bytes <- forM [0 .. 3] $ \i -> do
      w8 <- lift $ readArray ramArray (fromIntegral addr + i)
      pure $ bitCoerce w8
    let word = readWord @4 0 $ unsafeFromList bytes
    -- liftIO $ putStrLn $ "ramRead addr=0x" P.++ showHex addr "" P.++ " => " P.++ show (flip showHex "" <$> bytes)
    pure word
  ramWrite addr size w = do
    IOMem ramArray _ <- ask
    let b0 = slice d7 d0 w      -- Extract bits 7-0 (least significant byte)
        b1 = slice d15 d8 w     -- Extract bits 15-8
        b2 = slice d23 d16 w    -- Extract bits 23-16
        b3 = slice d31 d24 w    -- Extract bits 31-24 (most significant byte)
        writeBytes bytes = do
          forM_ (P.zip [0..] bytes) $ \(i, byte) ->
            lift $ writeArray ramArray (fromIntegral addr + i) byte
    case size of
      Byte -> writeBytes $ bitCoerce <$> [b0]           -- Write 1 byte
      Half -> writeBytes $ bitCoerce <$> [b0, b1]       -- Write 2 bytes (halfword)
      Word -> writeBytes $ bitCoerce <$> [b0, b1, b2, b3] -- Write 4 bytes (word)

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

    -- Create IOMem with IOArray and IORef
    ramArray <- newArray (0, fromIntegral (natVal (Proxy :: Proxy SPACE_SIZE)) - 1) 0
    loadElf elf $ \addr body ->
      forM_ (P.zip [fromIntegral addr..] (BSL.unpack body)) $ \(i, byte) ->
        writeArray ramArray i (bitCoerce byte)

    entryOffset <- startAddr elf

    rfRef <- newIORef initRF
    let ioMem = IOMem ramArray rfRef

    -- Run the simulator with IOMem and MonadMemory interface
    _ <- runIOMemT (watch' (simulator @(IOMemT IO) @SPACE_SIZE) {
      circuitState = Core.init {
        Core.stateFePc = fromIntegral entryOffset
      }
    }) ioMem

    -- Write the final RAM state directly using hPutArray
    withBinaryFile (benchmarkPath _benchmark P.++ ".core") WriteMode $ \handle ->
      hPutArray handle ramArray (fromIntegral (natVal (Proxy :: Proxy SPACE_SIZE)))

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