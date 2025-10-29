module Main (main) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Control.Exception (catch, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Core as Core
import Data.Monoid (First (getFirst))
import Elf.ElfLoader
import Elf.Syscall (handleSyscall, ProgramExitException(..))
import Elf.Memory
import Numeric (showHex)
import Simulate
import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Util
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Prelude as P

-- | Command line options
data Options = Options
  { optVerbose :: Bool
  , optExecutable :: String
  } deriving (Show)

-- | Parser for verbose flag
verboseParser :: Parser Bool
verboseParser = switch
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose output (shows PC and instruction trace)" )

-- | Parser for executable argument
executableParser :: Parser String
executableParser = strArgument
  ( metavar "EXECUTABLE"
  <> help "RISC-V ELF executable to run" )

-- | Complete options parser
optionsParser :: Parser Options
optionsParser = Options
  <$> verboseParser
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
            \  uc-risc-v --test-suite test/rv32ui/rv32ui-p-add" )

-- | General purpose instrument for crypto benchmarks and normal programs
generalInstrument :: (MonadIO m, MonadMemory m) => Bool -> Instrument m
generalInstrument shouldLog i s o step = do
  when shouldLog $ do
    let pc = Core.stateExPc s
    liftIO $ putStrLn $ "PC=0x" P.++ showHex pc "" P.++ " Instr=0x" P.++ show (Core.stateExInstr s)
  case getFirst $ Core.outSyscall o of
    Just True -> handleSyscall
    _ -> pure True

-- | Run a RISC-V executable
runExecutable :: Options -> IO ()
runExecutable opts = do
  let exePath = optExecutable opts
      verbose = optVerbose opts

  when verbose $ putStrLn $ "Loading ELF file: " P.++ exePath

  elf <- readElf exePath
  entryOffset <- startAddr elf
  ioMem <- newIOMem elf

  when verbose $ do
    putStrLn $ "Entry point: 0x" P.++ showHex entryOffset ""
    putStrLn "Starting execution..."

  -- Run the simulator with proper exception handling
  _ <- runIOMemT ioMem $ do
    loadProgram elf
    runElf
      (generalInstrument verbose)
      (simulator @(IOMemT IO))
        { circuitState =
            Core.init
              { Core.stateFePc = fromIntegral entryOffset
              }
        }

  when verbose $ putStrLn "Execution completed successfully"

main :: IO ()
main = do
  opts <- execParser programInfo
  runExecutable opts `catch` \(ProgramExitException code) -> do
    putStrLn $ "Program exited with code: " P.++ show code
    exitWith (ExitFailure (fromIntegral code))
