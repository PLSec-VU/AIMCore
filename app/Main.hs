module Main (main) where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Control.Exception (catch, throwIO, Exception)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Core as Core
import Data.Functor.Identity
import Data.Monoid (First (getFirst))
import Elf.ElfLoader
import Data.Elf (Elf)
import Elf.Syscall (handleSyscall, ProgramExitException(..))
import Memory.IO (SecureIOMemT, newSecureIOMem, loadSecureProgram, runSecureIOMemT, IOMemT, runIOMemT, newIOMem, loadProgram, IOMem, ioMemInitSP, secureIOMemInitSP)
import Memory.Types
import RegFile
import Data.Word (Word32)
import Numeric (showHex)
import Simulate
import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Types (Word)
import Util
import System.IO
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Prelude as P
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (&&), (++), (||), replicate, zip, take)
import Data.Traversable
import qualified Leak.PC.PC as Leak.PC
import qualified Leak.PC.Leak as Leak
import qualified Leak.SecretPC.PC as SecretLeak.PC
import Data.Binary (encode, Binary (..), Get)
import Data.IORef
import Crypto.Hash.BLAKE2.BLAKE2b (initialize', BLAKE2bState, update, finalize)
import qualified Data.ByteString as BS
import Data.Binary.Builder
import qualified Data.ByteString.Builder as BS

instance (KnownNat n) => Binary (Unsigned n) where
  put = put . toInteger
  get = fromInteger <$> (get :: Get Integer)

instance Binary Leak.BaseInstr

instance Binary Leak.Instr

instance Binary Leak.Out

data LeakageDivergenceException = LeakageDivergenceException
  { ldStepCount :: Int
  , ldLeakages :: [Leak.Out]
  , ldStates :: [Core.State Identity]
  , ldInputs :: [Core.Input Identity]
  }

instance Show LeakageDivergenceException where
  show (LeakageDivergenceException stepCount leakages states inputs) =
    unlines $
      [ "Leakage divergence detected:",
        "============================",
        "",
        "Step count: " P.++ show stepCount,
        "",
        "Number of instances: " P.++ show (P.length leakages),
        ""
      ] P.++
      P.concatMap formatInstance (P.zip [0..] (P.zip3 leakages states inputs)) P.++
      [ "",
        "Differing leakages:",
        "-------------------"
      ] P.++
      P.map (\(i, leak) -> "Instance " P.++ show i P.++ ": " P.++ show leak) (P.zip [0..] leakages)
    where
      formatInstance (idx, (leakage, state, input)) =
        [ "Instance " P.++ show idx P.++ ":",
          "-------------------------------",
          "",
          "Input:",
          "  inputIsInstr: " P.++ show (Core.inputIsInstr input),
          "  inputMem: " P.++ show (Core.inputMem input),
          "",
          "State:",
          "  PC (fetch): 0x" P.++ showHex (Core.stateFePc state) "",
          "  PC (decode): 0x" P.++ showHex (Core.stateDePc state) "",
          "  PC (execute): 0x" P.++ showHex (Core.stateExPc state) "",
          "  Instruction (execute): " P.++ show (Core.stateExInstr state),
          "  Instruction (memory): " P.++ show (Core.stateMeInstr state),
          "  Instruction (writeback): " P.++ show (Core.stateWbInstr state),
          "  Halt state: " P.++ show (Core.stateHalt state),
          "",
          "Leakage:",
          "  " P.++ show leakage,
          ""
        ]

instance Exception LeakageDivergenceException

data StepResult f = StepResult
  { stepMem :: IOMem
  , stepSim :: CircuitSim (IOMemT IO) (Core.Input Identity) (Core.State Identity) (Core.Output Identity)
  , stepNextInput :: Maybe (Core.Input Identity)
  , stepContinue :: Bool
  , stepFinalState :: Core.State Identity
  , stepLeakage :: Leak.Out
  }

-- | Command line options
data Options = Options
  { optVerbose :: Bool
  , optLeakageOutput :: Maybe FilePath
  , optSecureMemory :: Bool
  , optExecutable :: String
  , optNumInstances :: Int
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

-- | Parser for number of instances
optNumInstancesParser :: Parser Int
optNumInstancesParser = option auto
  ( long "num-instances"
  <> short 'n'
  <> metavar "N"
  <> value 1
  <> help "Number of instances to run concurrently (default: 1)" )

-- | Complete options parser
optionsParser :: Parser Options
optionsParser = Options
  <$> verboseParser
  <*> leakageOutputParser
  <*> secureMemoryParser
  <*> executableParser
  <*> optNumInstancesParser

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

generalizedInstrument ::
  (Access f, Show (f Word), Show leakOut, MonadIO m, MonadMemory m) =>
  ((Core.State f) -> (leakState, simState)) ->  -- proj function
  (leakState -> Core.Input f -> (leakState, leakOut)) ->  -- leak function
  (leakOut -> BS.ByteString) ->  -- serialization function
  String ->  -- mode name for logging
  Bool -> Maybe Handle -> IORef BLAKE2bState -> IORef (Maybe (Core.State f)) -> Instrument f m
generalizedInstrument projFn leakFn serializeFn modeName shouldLog leakageOutput leakDigest finalStateRef s = do
  let pc = Core.stateExPc s

  when shouldLog $ do
    liftIO $ putStrLn "==============================="
    liftIO $ putStrLn $ "[" P.++ modeName P.++ " MODE]"
    liftIO $ print pc
    let rf = Core.stateRegFile s
    let a0 = lookupRF 10 rf
    let a7 = lookupRF 17 rf
    let s0 = lookupRF 8 rf
    liftIO $ putStrLn $ "PC=0x" P.++ showHex pc "" P.++ " Instr=0x" P.++ show (Core.stateExInstr s) P.++ " a0=" P.++ show a0 P.++ " s0=" P.++ show s0 P.++ " syscall=" P.++ show a7

  -- Store the current state as the final state
  liftIO $ writeIORef finalStateRef (Just s)

  handleSyscall s

generalInstrument :: (Leak.Out -> BS.ByteString) -> Bool -> Maybe Handle -> IORef BLAKE2bState -> IORef (Maybe (Core.State Identity)) -> Instrument Identity (IOMemT IO)
generalInstrument f = generalizedInstrument
  Leak.PC.proj
  Leak.PC.leak
  f
  "STANDARD"

secureInstrument :: (MonadIO m, MonadMemory m) => Bool -> Maybe Handle -> IORef BLAKE2bState -> IORef (Maybe (Core.State PubSec)) -> Instrument PubSec m
secureInstrument = generalizedInstrument
  SecretLeak.PC.proj
  SecretLeak.PC.leak
  (BS.toStrict . encode . show)
  "SECURE"

runNormalMemory ::
  Options -> -- Command line options
  Elf -> -- ELF file
  Word32 -> -- Entry offset
  Maybe Handle -> -- Leakage output handle
  IORef BLAKE2bState -> -- Leakage digest
  IORef (Maybe (Core.State Identity)) -> -- Final state ref (simplified to Identity for now)
  IO ()
runNormalMemory Options{..} elf entryOffset leakOutputHandle leakDigest finalStateRef = do
  memInstances <- forM [1..optNumInstances] $ \_ -> newIOMem elf
  
  forM_ memInstances $ \mem -> do
    runIOMemT mem $ loadProgram elf
  
  let initialSims = P.map (\_ -> simulator @Identity @(IOMemT IO)) [1..optNumInstances]
  let initialStates = P.map (\(sim, mem) -> sim { circuitState = (Core.init @Identity @RegFile) { Core.stateFePc = fromIntegral entryOffset, Core.stateRegFile = modifyRF 2 (pure $ ioMemInitSP mem) initRF } }) (P.zip initialSims memInstances)
  
  go 0 (P.zip memInstances initialStates)
  where
    go stepCount instances = do
      when optVerbose $ putStrLn $ "=== Step " P.++ show stepCount P.++ " ==="

      -- Execute one step for each instance in its own memory context
      results <- forM (P.zip instances [(0::Int)..]) $ \((mem, sim@(CircuitSim i s step next)), idx) -> do
        (halted, s', mi', leakOutput, sysExit) <- runIOMemT mem $ do
          let (leakState, _) = Leak.PC.proj s
          let (_, leakOutput) = Leak.PC.leak leakState i

          (s', o) <- step i s
          let halted = Core.stateHalt s'
          (mi', sysExit) <- case halted of
            Nothing -> do
              mi'' <- next s' o
              pure (mi'', False)
            Just (Core.Syscall resumePc) -> do
              -- Handle syscall, write return value to a0, resume
              let serialize = if idx == 0
                    then BS.toStrict . encode
                    else const BS.empty
              let instr = generalInstrument serialize optVerbose leakOutputHandle leakDigest finalStateRef
              mRet <- instr s'
              case mRet of
                Nothing -> pure (Nothing, True)  -- exit
                Just ret -> do
                  let s'' = (Core.init :: Core.State Identity) {Core.stateFePc = resumePc,
                                       Core.stateRegFile = modifyRF 10 ret (Core.stateRegFile s')}
                  _ <- next s'' o
                  pure (Just Core.initInput, False)
            _ -> pure (Nothing, False)  -- EBreak, SecurityViolation: stop
          pure (halted, s', mi', leakOutput, sysExit)

        when optVerbose $ do
          putStrLn $ "Instance " P.++ show idx P.++ ":"
          putStrLn $ "  PC: 0x" P.++ showHex (Core.stateExPc s') ""

        let newSim = sim { circuitInput = fromMaybe i mi', circuitState = s' }
        pure StepResult
          { stepMem = mem
          , stepSim = newSim
          , stepNextInput = mi'
          , stepContinue = not sysExit && (isNothing halted || (\case { Just (Core.Syscall _) -> True; _ -> False }) halted)
          , stepFinalState = s'
          , stepLeakage = leakOutput
          }
      
      -- Extract leakages and check consistency immediately
      let leakages = P.map stepLeakage results
      case leakages of
        [] -> pure ()
        (firstLeakage:restLeakages) -> case filter (/= firstLeakage) restLeakages of
          [] -> when optVerbose $ putStrLn $ "All instances have consistent leakage: " P.++ show firstLeakage
          (diff:_) -> do
            -- Collect current inputs and states for detailed error reporting
            let currentInputs = P.map (circuitInput . stepSim) results
            let currentStates = P.map stepFinalState results
            throwIO $ LeakageDivergenceException stepCount leakages currentStates currentInputs
      
      let continuations = P.map stepContinue results
      let nextInputs = P.map stepNextInput results
      let nextInstances = P.map (\r -> (stepMem r, stepSim r)) results
      
      let shouldContinue = P.all id continuations && P.all isJust nextInputs
      if shouldContinue
        then go (stepCount + 1) nextInstances
        else do
          putStrLn $ "Execution completed after " P.++ show stepCount P.++ " steps"
          case results of
            (r:_) -> writeIORef finalStateRef (Just (stepFinalState r))
            [] -> pure ()

runExecutable :: Options -> IO ()
runExecutable opts@Options{..} = do
  when optVerbose $ putStrLn $ "Loading ELF file: " P.++ optExecutable

  elf <- readElf optExecutable
  entryOffset <- startAddr elf

  when optVerbose $ do
    putStrLn $ "Entry point: 0x" P.++ showHex entryOffset ""
    putStrLn $ "Number of instances: " P.++ show optNumInstances
    if optSecureMemory
      then putStrLn "Starting execution in SECURE MEMORY mode..."
      else putStrLn "Starting execution in standard mode..."
  
  leakOutputHandle <- forM optLeakageOutput (flip openFile WriteMode)
  leakDigest <- newIORef (initialize' 20 "leakage checksum")

  if optSecureMemory
    then do
      if optNumInstances == 1
        then do
          secureIOMem <- newSecureIOMem elf
          finalStateRef <- newIORef Nothing
          _ <- runSecureIOMemT secureIOMem $ do
            loadSecureProgram elf
            runElf
              (secureInstrument optVerbose leakOutputHandle leakDigest finalStateRef)
              (simulator @PubSec @(SecureIOMemT IO))
                { circuitState =
                    (Core.init @PubSec @RegFile)
                      { Core.stateFePc = fromIntegral entryOffset,
                        Core.stateRegFile = modifyRF 2 (pure $ secureIOMemInitSP secureIOMem) initRF
                      }
                }
          finalState <- readIORef finalStateRef
          case finalState of
            Just state | Core.stateHalt state == Just Core.SecurityViolation -> do
              putStrLn "Program aborted due to security violation"
            _ -> pure ()
        else do
          putStrLn "Secure memory mode with multiple instances not yet implemented"
    else do
      newIORef Nothing >>= runNormalMemory opts elf entryOffset leakOutputHandle leakDigest

  forM_ leakOutputHandle hClose

  when optVerbose $ putStrLn "Execution completed successfully"

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
