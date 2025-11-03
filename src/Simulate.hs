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

simulator :: forall f m. (Access f, MonadMemory m) => CircuitSim m (Input f) (Core.State f) (Output f)
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = init,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: Input f -> Core.State f -> m (Core.State f, Output f)
    step i s = do
      let (_ctrl', s', o) = simCore s i
      pure (s', o)
      where
        simCore :: Core.State f -> Input f -> (Control f, Core.State f, Output f)
        simCore = flip $ runRWS simCoreM
          where
            simCoreM :: CPUM f (Control f)
            simCoreM = withCtrlReset $ do
              writeback
              memory
              execute
              decode
              fetch

    next :: Output f -> m (Maybe (Input f))
    next (Output mem rs1 rs2 rd _ hlt)
      | getFirst hlt == Just True = pure Nothing
      | otherwise = do
          (rs1', rs2') <- doRegFile
          (mem_in, mem_instr) <- doMemory
          pure $
            Just $
              Input
                { inputIsInstr = mem_instr,
                  inputMem = pure mem_in,
                  inputRs1 = pure rs1',
                  inputRs2 = pure rs2'
                }
      where
        doRegFile :: m (Word, Word)
        doRegFile = do
          maybe (pure ()) (\(idx, val) -> regWrite idx (unAccess val)) $ getFirst rd
          rs1' <- maybe (pure 0) regRead $ getFirst rs1
          rs2' <- maybe (pure 0) regRead $ getFirst rs2
          pure (rs1', rs2')

        doMemory :: m (Word, Bool)
        doMemory
          | Just (MemAccess isInstr addr size mval) <- getFirst mem =
              case mval of
                Nothing -> (,isInstr) <$> ramRead addr
                Just val -> do
                  ramWrite addr size (unAccess val)
                  pure (0, isInstr)
          | otherwise = pure (0, False)

runSimulator :: forall f ramSize progSize a. (Access f, KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) =>
  ( CircuitSim (State (Mem (MemSizeFrom progSize ramSize))) (Input f) (Core.State f) (Output f) ->
    State (Mem (MemSizeFrom progSize ramSize)) a
  ) ->
  Vec progSize Word ->
  a
runSimulator f = evalState (f simulator) . flip Mem initRF . mkRAM

watchSim :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> [(Core.State Identity, Output Identity, Maybe (Input Identity))]
watchSim = runSimulator @Identity @ramSize @progSize watch

simResult :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> Vec (MemSizeFrom progSize ramSize) Byte
simResult = runSimulator @Identity result
