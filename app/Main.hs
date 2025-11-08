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
import Elf.Memory (SecureIOMemT, newSecureIOMem, loadSecureProgram, runSecureIOMemT, IOMemT, runIOMemT, newIOMem, loadProgram)
import Numeric (showHex)
import Simulate
import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Types (Word)
import Util
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Prelude as P
import Data.Traversable
import System.IO
import Data.Foldable (forM_)
import qualified Leak.PC.PC as Leak.PC
import qualified Leak.PC.Leak as Leak
import qualified Leak.SecretPC.PC as SecretLeak.PC
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

-- Note: SecretLeak.Out is Input Identity, but we'll handle encoding differently

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

-- | Generalized instrument for crypto benchmarks and normal programs
-- Works with any Access functor by taking proj and leak functions as parameters
generalizedInstrument ::
  (Access f, Show (f Word), Show leakOut, MonadIO m, MonadMemory m) =>
  ((Core.State f) -> (leakState, simState)) ->  -- proj function
  (leakState -> Core.Input f -> (leakState, leakOut)) ->  -- leak function
  (leakOut -> BS.ByteString) ->  -- serialization function
  String ->  -- mode name for logging
  Bool -> Maybe Handle -> IORef BLAKE2bState -> IORef (Maybe (Core.State f)) -> Instrument f m
generalizedInstrument projFn leakFn serializeFn modeName shouldLog leakageOutput leakDigest finalStateRef i s o _step = do
  let pc = Core.stateExPc s
        
  when shouldLog $ do
    liftIO $ putStrLn "==============================="
    liftIO $ putStrLn $ "[" P.++ modeName P.++ " MODE]"
    liftIO $ print i
    liftIO $ print o
    a0 <- regRead 10
    a7 <- regRead 17
    s0 <- regRead 8
    liftIO $ putStrLn $ "PC=0x" P.++ showHex pc "" P.++ " Instr=0x" P.++ show (Core.stateExInstr s) P.++ " a0=0x" P.++ showHex a0 "" P.++ " s0=0x" P.++ showHex s0 "" P.++ " syscall=0x" P.++ showHex a7 ""

  -- Use the provided proj and leak functions
  let (leakState , _) = projFn s
  let (_ , leakOutput) = leakFn leakState i
  case leakageOutput of
    Nothing -> pure ()
    Just h -> liftIO $ hPutStrLn h $ show leakOutput
  liftIO $ modifyIORef' leakDigest (update (serializeFn leakOutput))

  -- Store the current state as the final state
  liftIO $ writeIORef finalStateRef (Just s)

  case getFirst $ Core.outSyscall o of
    Just True -> handleSyscall
    _ -> pure True

-- | General purpose instrument for crypto benchmarks and normal programs
-- Uses Identity functor for non-security mode (backward compatibility)
generalInstrument :: (MonadIO m, MonadMemory m) => Bool -> Maybe Handle -> IORef BLAKE2bState -> IORef (Maybe (Core.State Identity)) -> Instrument Identity m
generalInstrument = generalizedInstrument
  Leak.PC.proj
  Leak.PC.leak
  (BS.toStrict . encode)
  "STANDARD"

-- | Secure instrument for crypto benchmarks with memory security tracking
-- Uses PubSec functor for security mode
secureInstrument :: (MonadIO m, MonadMemory m) => Bool -> Maybe Handle -> IORef BLAKE2bState -> IORef (Maybe (Core.State PubSec)) -> Instrument PubSec m
secureInstrument = generalizedInstrument
  SecretLeak.PC.proj
  SecretLeak.PC.leak
  (BS.toStrict . encode . show)
  "SECURE"

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
      finalStateRef <- newIORef Nothing
      _ <- runSecureIOMemT secureIOMem $ do
        loadSecureProgram elf
        runElf
          (secureInstrument verbose leakOutputHandle leakDigest finalStateRef)
          (simulator @PubSec @(SecureIOMemT IO))
            { circuitState =
                (Core.init @PubSec)
                  { Core.stateFePc = fromIntegral entryOffset
                  }
            }
      -- Check for security violation
      finalState <- readIORef finalStateRef
      case finalState of
        Just state | Core.stateHalt state == Core.SecurityViolation -> do
          putStrLn "Program aborted due to security violation"
        _ -> pure ()
    else do
      -- Run with standard memory (Identity mode)
      ioMem <- newIOMem elf
      finalStateRef <- newIORef Nothing
      runIOMemT ioMem $ do
        loadProgram elf
        runElf
          (generalInstrument verbose leakOutputHandle leakDigest finalStateRef)
          (simulator @Identity @(IOMemT IO))
            { circuitState =
                (Core.init @Identity)
                  { Core.stateFePc = fromIntegral entryOffset
                  }
            }

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
