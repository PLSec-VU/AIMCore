{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Simulate
  ( Mem (..),
    result,
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
import RegFile
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

data Mem n = Mem
  { memRAM :: Vec n Byte,
    memRF :: RegFile
  }
  deriving (Eq, Show, Generic, NFDataX)

instance (KnownNat n, Monad m, MonadState (Mem n) m) => MonadMemory n m where
  getRegFile = gets memRF
  putRegFile rf = modify $ \s -> s {memRF = rf}
  ramRead addr = readWord addr <$> gets memRAM
  ramWrite addr size w = do
    ram <- gets memRAM
    modify $ \s -> s {memRAM = write size addr w ram}

result :: (MonadState (Mem n) m) => CircuitSim m i s o -> m (Vec n Byte)
result c = watch c *> gets memRAM

data Syscall
  = SysExit
  | SysUnknown Word
  deriving (Eq, Show)

simulator :: forall m n. (MonadMemory n m) => CircuitSim m Input Core.State Output
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
            simCoreM = withCtrlReset $ do
              writeback
              memory
              execute
              decode
              fetch

    next :: Output -> m (Maybe Input)
    next (Output mem rs1 rs2 rd syscall hlt)
      | getFirst hlt == Just True = pure Nothing
      | otherwise = do
          (rs1', rs2') <- doRegFile
          (mem_in, mem_instr) <- doMemory
          _syscall <- case getFirst syscall of
            Just True -> Just <$> doSyscall
            _ -> pure Nothing
          case _syscall of
            Just SysExit -> pure Nothing
            _ -> pure $
              Just $
                Input
                  { inputIsInstr = mem_instr,
                    inputMem = mem_in,
                    inputRs1 = rs1',
                    inputRs2 = rs2'
                  }
      where
        doRegFile :: m (Word, Word)
        doRegFile = do
          maybe (pure ()) (uncurry (regWrite @n)) $ getFirst rd
          rs1' <- maybe (pure 0) (regRead @n) $ getFirst rs1
          rs2' <- maybe (pure 0) (regRead @n) $ getFirst rs2
          pure (rs1', rs2')

        doMemory :: m (Word, Bool)
        doMemory
          | Just (MemAccess isInstr addr size mval) <- getFirst mem =
              case mval of
                Nothing -> (,isInstr) <$> ramRead @n addr
                Just val -> do
                  ramWrite @n addr size val
                  pure (0, isInstr)
          | otherwise = pure (0, False)

        doSyscall :: m Syscall
        doSyscall = do
          a7 <- regRead @n 17
          case a7 of
            93 -> pure SysExit
            -- Implement other syscalls as needed
            n -> pure $ SysUnknown n
        

runSimulator :: forall ramSize progSize a. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) =>
  ( CircuitSim (State (Mem (MemSizeFrom progSize ramSize))) Input Core.State Output ->
    State (Mem (MemSizeFrom progSize ramSize)) a
  ) ->
  Vec progSize Word ->
  a
runSimulator f = evalState (f simulator) . flip Mem initRF . mkRAM

watchSim :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> [(Core.State, Output, Maybe Input)]
watchSim = runSimulator @ramSize @progSize watch

simResult :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> Vec (MemSizeFrom progSize ramSize) Byte
simResult = runSimulator result
