{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Simulate where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Debug.Trace as DB
import Instruction hiding (decode)
import Pipe
import Regfile
import Text.Read (readMaybe)
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))
import qualified Prelude

data Mem n = Mem
  { memRAM :: Vec n Byte,
    memRf :: Regfile
  }
  deriving (Eq, Show, Generic, NFDataX)

instance (MonadState (Mem MEM_SIZE_BYTES) m) => MonadMemory m where
  getRAM = gets memRAM
  putRAM ram = modify $ \s -> s {memRAM = ram}
  getRegfile = gets memRf
  putRegfile rf = modify $ \s -> s {memRf = rf}

type MonadSim m = (MonadLog m, MonadMemory m)

type SimM n = RWS () Log (Mem n)

simulator :: forall m. (MonadSim m) => CircuitSim m Input Pipe Output
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = initPipe,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: (MonadSim m) => Input -> Pipe -> m (Pipe, Output)
    step i s = do
      let (ctrl', s', o) = simPipe s i
      log $
        unlines
          [ "Input:",
            "--------------------",
            show i,
            "",
            "State:",
            "--------------------",
            show s,
            "",
            "Output:",
            "--------------------",
            show o,
            "",
            "Control:",
            "--------------------",
            show ctrl'
          ]
      pure (s', o)
      where
        simPipe :: Pipe -> Input -> (Control, Pipe, Output)
        simPipe = flip $ runRWS simPipeM
          where
            simPipeM :: CPUM Control
            simPipeM = do
              writeback
              memory
              execute
              decode
              fetch
              ctrl <- gets pipeCtrl
              resetCtrl
              pure ctrl

    next :: (MonadSim m) => Output -> m (Maybe Input)
    next (Output mem rs1 rs2 rd hlt)
      | getFirst hlt == Just True = pure Nothing
      | otherwise = do
          (rs1', rs2') <- doRegFile
          (mem_in, mem_inst) <- doMemory
          pure $
            pure $
              Input
                { inputIsInst = mem_inst,
                  inputMem = mem_in,
                  inputRs1 = rs1',
                  inputRs2 = rs2'
                }
      where
        doRegFile :: m (Word, Word)
        doRegFile = do
          maybe (pure ()) (uncurry regWrite) $ getFirst rd
          rs1' <- maybe (pure 0) regRead $ getFirst rs1
          rs2' <- maybe (pure 0) regRead $ getFirst rs2
          pure (rs1', rs2')

        doMemory :: m (Word, Bool)
        doMemory
          | Just (MemAccess isInst addr size mval) <- getFirst mem =
              case mval of
                Nothing -> (,isInst) <$> ramRead addr
                Just val -> do
                  ramWrite addr size val
                  pure (0, isInst)
          | otherwise = pure (0, False)

runSim :: Vec PROG_SIZE Word -> Mem MEM_SIZE_BYTES
runSim = fst . execRWS (run simulator) () . flip Mem initRF . mkRAM

runSimIO :: Vec PROG_SIZE Word -> IO ()
runSimIO = void . execRWST (runIO simulator) () . flip Mem initRF . mkRAM
