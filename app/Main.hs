module Main (main) where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Control.Exception (catch)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Core as Core
import Data.Functor.Identity
import Elf.ElfLoader
import Elf.Syscall (ProgramExitException (..), handleSyscall)
import Memory.IO (IOMemT, ioMemInitSP, loadProgram, newIOMem, runIOMemT)
import Memory.Types
import Numeric (showHex)
import Options.Applicative
import RegFile
import Simulate
import System.Exit (ExitCode (..), exitWith)
import Types (Word)
import Util
import qualified Prelude as P
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||), replicate, zip, take)

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
            \  uc-risc-v --verbose benchmark/bench_blake2b" )

-- | Called by 'runElf' whenever the core halts on a syscall: optionally log the
-- state, then service the syscall.
instrument ::
  (MonadIO m, MonadMemory m) =>
  Bool -> Instrument Identity m
instrument shouldLog s = do
  let pc = Core.stateExPc s

  when shouldLog $ do
    liftIO $ putStrLn "==============================="
    liftIO $ print pc
    let rf = Core.stateRegFile s
    let a0 = lookupRF 10 rf
    let a7 = lookupRF 17 rf
    let s0 = lookupRF 8 rf
    liftIO $ putStrLn $ "PC=0x" P.++ showHex pc "" P.++ " Instr=0x" P.++ show (Core.stateExInstr s) P.++ " a0=" P.++ show a0 P.++ " s0=" P.++ show s0 P.++ " syscall=" P.++ show a7

  handleSyscall s

runExecutable :: Options -> IO ()
runExecutable Options{..} = do
  when optVerbose $ putStrLn $ "Loading ELF file: " P.++ optExecutable

  elf <- readElf optExecutable
  entryOffset <- startAddr elf

  when optVerbose $ do
    putStrLn $ "Entry point: 0x" P.++ showHex entryOffset ""
    putStrLn "Starting execution..."

  mem <- newIOMem elf
  runIOMemT mem $ do
    loadProgram elf
    runElf
      (instrument optVerbose)
      (simulator @Identity @(IOMemT IO))
        { circuitState =
            Core.initAt
              (fromIntegral entryOffset)
              (modifyRF 2 (pure $ ioMemInitSP mem) (initRF @Identity))
        }

  when optVerbose $ putStrLn "Execution completed successfully"

main :: IO ()
main = do
  opts <- execParser programInfo
  runExecutable opts `catch` \(ProgramExitException code) -> do
    putStrLn $ "Program exited with code: " P.++ show code
    exitWith (ExitFailure (fromIntegral code))
