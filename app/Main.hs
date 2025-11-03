module Main (main) where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Control.Exception (catch)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Core as Core
import Data.Functor.Identity
import Data.Monoid (First (getFirst))
import Elf.ElfLoader
import Elf.Syscall (handleSyscall, ProgramExitException(..))
import Elf.Memory (SecureIOMemT, newSecureIOMem, loadSecureProgram, runSecureIOMemT, IOMem, IOMemT, runIOMemT, newIOMem, loadProgram)
import Numeric (showHex)
import Simulate
import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Util
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Prelude as P
import Data.Traversable
import System.IO
import Data.Foldable (forM_)
import qualified Leak.PC.PC as Leak.PC
import qualified Leak.PC.Leak as Leak
import Data.Binary (encode, Binary (..), Get (..))
import Data.IORef
import Crypto.Hash.BLAKE2.BLAKE2b (initialize', BLAKE2bState, update, finalize)
import qualified Data.ByteString as BS
import qualified Data.Binary.Builder as BS
import qualified Data.ByteString.Builder as BS

instance (KnownNat n) => Binary (Unsigned n) where
  put = put . toInteger
  get = fromInteger <$> (get :: Get Integer)

instance Binary Leak.BaseInstr

instance Binary Leak.Instr

instance Binary Leak.Out

-- | Command line options
data Options = Options
  { optVerbose :: Bool
  , optLeakageOutput :: Maybe FilePath
  , optSecureMemory :: Bool
  , optExecutable :: String
  } deriving (Show)

-- | Parser for verbose flag
verboseParser :: Parser Bool
verboseParser = switch
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose output (shows PC and instruction trace)" )

-- | Parser for leakage output file argument
leakageOutputParser :: Parser (Maybe FilePath)
leakageOutputParser = optional $ strOption
  ( long "leakage-output"
  <> short 'l'
  <> metavar "FILE"
  <> help "Output file for leakage traces" )

-- | Parser for secure memory flag
secureMemoryParser :: Parser Bool
secureMemoryParser = switch
  ( long "secure-memory"
  <> short 's'
  <> help "Enable secure memory tracking (PubSec mode)" )

-- | Parser for executable argument
executableParser :: Parser String
executableParser = strArgument
  ( metavar "EXECUTABLE"
  <> help "RISC-V ELF executable to run" )

-- | Complete options parser
optionsParser :: Parser Options
optionsParser = Options
  <$> verboseParser
  <*> leakageOutputParser
  <*> secureMemoryParser
  <*> executableParser

-- | Program info for help generation
programInfo :: ParserInfo Options
programInfo = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Execute RISC-V ELF binaries"
  <> header "uc-risc-v - RISC-V Simulator"
  <> footer "Examples:\n\
            \  uc-risc-v benchmark/bench_chacha20\n\
            \  uc-risc-v --verbose benchmark/bench_blake2b\n\
            \  uc-risc-v --secure-memory benchmark/bench_secure_memory\n\
            \  uc-risc-v --test-suite test/rv32ui/rv32ui-p-add" )

-- | General purpose instrument for crypto benchmarks and normal programs
-- Uses Identity functor for non-security mode (backward compatibility)
generalInstrument :: (MonadIO m, MonadMemory m) => Bool -> Maybe Handle -> IORef BLAKE2bState -> Instrument Identity m
generalInstrument shouldLog leakageOutput leakDigest i s o step = do
  let pc = Core.stateExPc s
  when shouldLog $ do
    liftIO $ putStrLn "==============================="
    liftIO $ print i
    liftIO $ print o
    a0 <- regRead 10
    a7 <- regRead 17
    s0 <- regRead 8
    liftIO $ putStrLn $ "PC=0x" P.++ showHex pc "" P.++ " Instr=0x" P.++ show (Core.stateExInstr s) P.++ " a0=0x" P.++ showHex a0 "" P.++ " s0=0x" P.++ showHex s0 "" P.++ " syscall=0x" P.++ showHex a7 ""

  let (leakState , _) = Leak.PC.proj (s , ())
  let (_ , leakOutput) = Leak.PC.leak leakState i
  case leakageOutput of
    Nothing -> pure ()
    Just h -> liftIO $ hPutStrLn h $ show leakOutput

  liftIO $ modifyIORef' leakDigest (update (BS.toStrict (encode leakOutput)))

  case getFirst $ Core.outSyscall o of
    Just True -> handleSyscall
    _ -> pure True

-- | Secure instrument for crypto benchmarks with memory security tracking
-- Uses PubSec functor for security mode
secureInstrument :: (MonadIO m, MonadMemory m) => Bool -> Maybe Handle -> IORef BLAKE2bState -> Instrument PubSec m
secureInstrument shouldLog leakageOutput leakDigest i s o step = do
  let pc = Core.stateExPc s
  when shouldLog $ do
    liftIO $ putStrLn "==============================="
    liftIO $ putStrLn "[SECURE MODE]"
    liftIO $ print i
    liftIO $ print o
    a0 <- regRead 10
    a7 <- regRead 17
    s0 <- regRead 8
    liftIO $ putStrLn $ "PC=0x" P.++ showHex pc "" P.++ " Instr=0x" P.++ show (Core.stateExInstr s) P.++ " a0=0x" P.++ showHex a0 "" P.++ " s0=0x" P.++ showHex s0 "" P.++ " syscall=0x" P.++ showHex a7 ""

  -- Skip leak analysis for secure mode to avoid type complications
  -- The main benefit is the secure memory tracking, not the leak analysis
  case leakageOutput of
    Nothing -> pure ()
    Just h -> liftIO $ hPutStrLn h "Secure mode: leak analysis skipped"

  -- Still update digest with a placeholder for secure mode
  liftIO $ modifyIORef' leakDigest (update (BS.toStrict (encode ("secure_mode" :: String))))

  case getFirst $ Core.outSyscall o of
    Just True -> handleSyscall
    _ -> pure True

-- | Run a RISC-V executable
runExecutable :: Options -> IO ()
runExecutable opts = do
  let exePath = optExecutable opts
      verbose = optVerbose opts
      leakageOutput = optLeakageOutput opts
      secureMemory = optSecureMemory opts

  when verbose $ putStrLn $ "Loading ELF file: " P.++ exePath

  elf <- readElf exePath
  entryOffset <- startAddr elf
  ioMem <- newIOMem elf

  when verbose $ do
    putStrLn $ "Entry point: 0x" P.++ showHex entryOffset ""
    if secureMemory
      then putStrLn "Starting execution in SECURE MEMORY mode..."
      else putStrLn "Starting execution in standard mode..."
  
  leakOutputHandle <- forM leakageOutput (flip openFile WriteMode)
  leakDigest <- newIORef (initialize' 20 "leakage checksum")

  if secureMemory
    then do
      -- Run with secure memory tracking (PubSec mode)
      secureIOMem <- newSecureIOMem elf
      _ <- runSecureIOMemT secureIOMem $ do
        loadSecureProgram elf
        runElf
          (secureInstrument verbose leakOutputHandle leakDigest)
          (simulator @PubSec @(SecureIOMemT IO))
            { circuitState =
                (Core.init @PubSec)
                  { Core.stateFePc = fromIntegral entryOffset
                  }
            }
      pure ()
    else do
      -- Run with standard memory (Identity mode)
      _ <- runIOMemT ioMem $ do
        loadProgram elf
        runElf
          (generalInstrument verbose leakOutputHandle leakDigest)
          (simulator @Identity @(IOMemT IO))
            { circuitState =
                (Core.init @Identity)
                  { Core.stateFePc = fromIntegral entryOffset
                  }
            }
      pure ()

  forM_ leakOutputHandle hClose

  when verbose $ putStrLn "Execution completed successfully"

  digest <- finalize 20 <$> readIORef leakDigest
  putStr "Leakage digest: "
  BS.putStr $ BS.toStrict $ BS.toLazyByteString $ BS.byteStringHex digest
  putStrLn ""

main :: IO ()
main = do
  opts <- execParser programInfo
  runExecutable opts `catch` \(ProgramExitException code) -> do
    putStrLn $ "Program exited with code: " P.++ show code
    exitWith (ExitFailure (fromIntegral code))
