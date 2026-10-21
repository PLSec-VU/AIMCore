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
import Data.Maybe (isJust)
import Memory.Types
import Memory.Vec
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

result :: (MonadState (Mem n) m) => CircuitSim m i s o -> m (Vec n Byte)
result c = watch c *> gets memRAM

simulator :: forall m. (MonadMemory m) => CircuitSim m Input Core.State Output
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = init,
      circuitStep = \i s -> pure $ Core.circuit s i,
      circuitNext = next
    }
  where
    next :: Core.State -> Output -> m (Maybe Input)
    next s (Output mem) = do
      mem_in <- doMemory
      if isJust (Core.stateHalt s)
        then pure Nothing
        else pure $
          Just $
            Input
              { inputMem = mem_in,
                inputMemReady = True
              }
      where
        doMemory :: m Word
        doMemory = case mem of
          Just (MemFetch addr) -> ramRead addr Word
          Just (MemRead addr size) -> ramRead addr size
          Just (MemWrite addr size val) -> do
            ramWrite addr size val
            pure 0
          Nothing -> pure 0

runSimulator ::
  forall ramSize progSize a.
  (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) =>
  ( CircuitSim (State (Mem (MemSizeFrom progSize ramSize))) Input Core.State Output ->
    State (Mem (MemSizeFrom progSize ramSize)) a
  ) ->
  Vec progSize Word ->
  a
runSimulator f = evalState (f simulator) . Mem . mkRAM

watchSim :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> [(Core.State, Output, Maybe Input)]
watchSim = runSimulator @ramSize @progSize watch

simResult :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> Vec (MemSizeFrom progSize ramSize) Byte
simResult = runSimulator result
