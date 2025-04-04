{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Simulate
  ( Mem (..),
    simulator,
    runSimulator,
    watchSim,
    simResult,
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad.RWS
import Control.Monad.State
import Data.Monoid
import Pipe
import Regfile
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

data Mem n = Mem
  { memRAM :: Vec n Byte,
    memRf :: Regfile
  }
  deriving (Eq, Show, Generic, NFDataX)

instance (Monad m, MonadState (Mem MEM_SIZE_BYTES) m) => MonadMemory m where
  getRAM = gets memRAM
  putRAM ram = modify $ \s -> s {memRAM = ram}
  getRegfile = gets memRf
  putRegfile rf = modify $ \s -> s {memRf = rf}

simulator :: forall m. (MonadState (Mem MEM_SIZE_BYTES) m) => CircuitSim m Input Pipe Output
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = initPipe,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: Input -> Pipe -> m (Pipe, Output)
    step i s = do
      let (_ctrl', s', o) = simPipe s i
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

    next :: Output -> m (Maybe Input)
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

runSimulator ::
  ( CircuitSim (State (Mem MEM_SIZE_BYTES)) Input Pipe Output ->
    State (Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f = evalState (f simulator) . flip Mem initRF . mkRAM

watchSim :: Vec PROG_SIZE Word -> [(Pipe, Output, Maybe Input)]
watchSim = runSimulator watch

simResult :: Vec PROG_SIZE Word -> Vec MEM_SIZE_BYTES Byte
simResult = runSimulator result
