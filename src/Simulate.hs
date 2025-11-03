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
import Data.Functor.Identity
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

instance (KnownNat n, Monad m, MonadState (Mem n) m) => MonadMemory m where
  getRegFile = gets memRF
  putRegFile rf = modify $ \s -> s {memRF = rf}
  ramRead addr = readWord addr <$> gets memRAM
  ramWrite addr size w = do
    ram <- gets memRAM
    modify $ \s -> s {memRAM = write size addr w ram}

result :: (MonadState (Mem n) m) => CircuitSim m i s o -> m (Vec n Byte)
result c = watch c *> gets memRAM

simulator :: forall m. (MonadMemory m) => CircuitSim m (Input Identity) (Core.State Identity) (Output Identity)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = init,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: Input Identity -> Core.State Identity -> m (Core.State Identity, Output Identity)
    step i s = do
      let (_ctrl', s', o) = simCore s i
      pure (s', o)
      where
        simCore :: Core.State Identity -> Input Identity -> (Control Identity, Core.State Identity, Output Identity)
        simCore = flip $ runRWS simCoreM
          where
            simCoreM :: CPUM Identity (Control Identity)
            simCoreM = withCtrlReset $ do
              writeback
              memory
              execute
              decode
              fetch

    next :: Output Identity -> m (Maybe (Input Identity))
    next (Output mem rs1 rs2 rd _ hlt)
      | getFirst hlt == Just True = pure Nothing
      | otherwise = do
          (rs1', rs2') <- doRegFile
          (mem_in, mem_instr) <- doMemory
          pure $
            Just $
              Input
                { inputIsInstr = mem_instr,
                  inputMem = Identity mem_in,
                  inputRs1 = Identity rs1',
                  inputRs2 = Identity rs2'
                }
      where
        doRegFile :: m (Word, Word)
        doRegFile = do
          maybe (pure ()) (\(idx, val) -> regWrite idx (runIdentity val)) $ getFirst rd
          rs1' <- maybe (pure 0) regRead $ getFirst rs1
          rs2' <- maybe (pure 0) regRead $ getFirst rs2
          pure (rs1', rs2')

        doMemory :: m (Word, Bool)
        doMemory
          | Just (MemAccess isInstr addr size mval) <- getFirst mem =
              case mval of
                Nothing -> (,isInstr) <$> ramRead addr
                Just val -> do
                  ramWrite addr size (runIdentity val)
                  pure (0, isInstr)
          | otherwise = pure (0, False)

runSimulator :: forall ramSize progSize a. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) =>
  ( CircuitSim (State (Mem (MemSizeFrom progSize ramSize))) (Input Identity) (Core.State Identity) (Output Identity) ->
    State (Mem (MemSizeFrom progSize ramSize)) a
  ) ->
  Vec progSize Word ->
  a
runSimulator f = evalState (f simulator) . flip Mem initRF . mkRAM

watchSim :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> [(Core.State Identity, Output Identity, Maybe (Input Identity))]
watchSim = runSimulator @ramSize @progSize watch

simResult :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> Vec (MemSizeFrom progSize ramSize) Byte
simResult = runSimulator result
