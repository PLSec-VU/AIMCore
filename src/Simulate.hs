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
import Core hiding (State)
import qualified Core
import Data.Monoid
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

simulator :: forall m. (MonadState (Mem MEM_SIZE_BYTES) m) => CircuitSim m Input Core.State Output
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = init,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: Input -> Core.State -> m (Core.State, Output)
    step i s = do
      let (_ctrl', s', o) = simCore s i
      pure (s', o)
      where
        simCore :: Core.State -> Input -> (Control, Core.State, Output)
        simCore = flip $ runRWS simCoreM
          where
            simCoreM :: CPUM Control
            simCoreM = do
              writeback
              memory
              execute
              decode
              fetch
              ctrl <- gets stateCtrl
              resetCtrl
              pure ctrl

    next :: Output -> m (Maybe Input)
    next (Output mem rs1 rs2 rd hlt)
      | getFirst hlt == Just True = pure Nothing
      | otherwise = do
          (rs1', rs2') <- doRegFile
          (mem_in, mem_instr) <- doMemory
          pure $
            pure $
              Input
                { inputIsInstr = mem_instr,
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
          | Just (MemAccess isInstr addr size mval) <- getFirst mem =
              case mval of
                Nothing -> (,isInstr) <$> ramRead addr
                Just val -> do
                  ramWrite addr size val
                  pure (0, isInstr)
          | otherwise = pure (0, False)

runSimulator ::
  ( CircuitSim (State (Mem MEM_SIZE_BYTES)) Input Core.State Output ->
    State (Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f = evalState (f simulator) . flip Mem initRF . mkRAM

watchSim :: Vec PROG_SIZE Word -> [(Core.State, Output, Maybe Input)]
watchSim = runSimulator watch

simResult :: Vec PROG_SIZE Word -> Vec MEM_SIZE_BYTES Byte
simResult = runSimulator result
