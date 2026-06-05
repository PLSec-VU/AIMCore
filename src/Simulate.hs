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

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad.RWS
import Control.Monad.State
import Core hiding (State)
import qualified Core
import Data.Functor.Identity
import Data.Maybe (isJust)
import Data.Monoid
import Memory.Types
import Memory.Vec
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

result :: (MonadState (Mem n) m) => CircuitSim m i s o -> m (Vec n Byte)
result c = watch c *> gets memRAM

simulator :: forall f m. (Access f, MonadMemory m) => CircuitSim m (Input f) (Core.State f) (Output f)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = init,
      circuitStep = \i s -> pure $ Core.circuit s i,
      circuitNext = next
    }
  where
    next :: Core.State f -> Output f -> m (Maybe (Input f))
    next s (Output mem) = do
      (mem_in, mem_instr) <- doMemory
      if isJust (Core.stateHalt s)
        then pure Nothing
        else pure $
          Just $
            Input
              { inputIsInstr = mem_instr,
                inputMem = mem_in
              }
      where
        doMemory :: m (f Word, Bool)
        doMemory
          | Just (MemAccess isInstr addr size mval) <- getFirst mem =
              case mval of
                Nothing -> do
                  word <- ramRead isInstr addr size
                  isSecret <- isMemorySecret addr
                  pure (conditionalSecret isSecret word, isInstr)
                Just val -> do
                  ramWrite addr size (unAccess val)
                  pure (pure 0, isInstr)
          | otherwise = pure (pure 0, False)

runSimulator ::
  forall f ramSize progSize a.
  (Access f, KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) =>
  ( CircuitSim (State (Mem (MemSizeFrom progSize ramSize))) (Input f) (Core.State f) (Output f) ->
    State (Mem (MemSizeFrom progSize ramSize)) a
  ) ->
  Vec progSize Word ->
  a
runSimulator f = evalState (f simulator) . Mem . mkRAM

watchSim :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> [(Core.State Identity, Output Identity, Maybe (Input Identity))]
watchSim = runSimulator @Identity @ramSize @progSize watch

simResult :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> Vec (MemSizeFrom progSize ramSize) Byte
simResult = runSimulator @Identity result
