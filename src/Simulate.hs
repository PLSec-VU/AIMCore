{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Simulate
  ( Mem (..),
    result,
    Machine (..),
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
  -- Identity implementation: no-op security functions
  markMemoryRegion _ _ _ = pure ()
  isMemorySecret _ = pure False

result :: (MonadState (Mem n) m) => CircuitSim m i s o -> m (Vec n Byte)
result c = watch c *> gets memRAM

-- | Bare minimum functionality to fit in the simulator
class Machine s where
  type MachineInput s :: *
  type MachineOutput s :: *
  mInitInput :: MachineInput s
  mInitState :: s
  mStep :: (Monad m) => MachineInput s -> s -> m (s, MachineOutput s)
  mNext :: (MonadMemory m) => MachineOutput s -> m (Maybe (MachineInput s))

-- | Pipelined RISC-V implementation of Machine
instance (Access f) => Machine (Core.State f) where
  type MachineInput (Core.State f) = Input f
  type MachineOutput (Core.State f) = Output f
  mInitInput = initInput
  mInitState = init
  mStep i s = do
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

  mNext (Output mem rs1 rs2 rd _ hlt)
    | getFirst hlt == Just True = pure Nothing
    | otherwise = do
        (rs1', rs2') <- doRegFile
        (mem_in, mem_instr) <- doMemory
        pure $
          Just $
            Input
              { inputIsInstr = mem_instr,
                inputMem = mem_in,
                inputRs1 = pure rs1',
                inputRs2 = pure rs2'
              }
    where
      doRegFile :: (MonadMemory m) => m (Word, Word)
      doRegFile = do
        maybe (pure ()) (\(idx, val) -> regWrite idx (unAccess val)) $ getFirst rd
        rs1' <- maybe (pure 0) regRead $ getFirst rs1
        rs2' <- maybe (pure 0) regRead $ getFirst rs2
        pure (rs1', rs2')

      doMemory :: (MonadMemory m) => m (f Word, Bool)
      doMemory
        | Just (MemAccess isInstr addr size mval) <- getFirst mem =
            case mval of
              Nothing -> do
                word <- ramRead addr
                isSecret <- isMemorySecret addr
                let secureWord = conditionalSecret isSecret word
                pure (secureWord, isInstr)
              Just val -> do
                ramWrite addr size (unAccess val)
                pure (pure 0, isInstr)
        | otherwise = pure (pure 0, False)

simulator :: forall s m. (Machine s, MonadMemory m) => CircuitSim m (MachineInput s) s (MachineOutput s)
simulator =
  CircuitSim
    { circuitInput = mInitInput @s,
      circuitState = mInitState @s,
      circuitStep = mStep @s,
      circuitNext = mNext @s
    }

runSimulator ::
  forall s ramSize progSize a.
  (Machine s, KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) =>
  ( CircuitSim (State (Mem (MemSizeFrom progSize ramSize))) (MachineInput s) s (MachineOutput s) ->
    State (Mem (MemSizeFrom progSize ramSize)) a
  ) ->
  Vec progSize Word ->
  a
runSimulator f = evalState (f simulator) . flip Mem initRF . mkRAM

watchSim :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> [(Core.State Identity, Output Identity, Maybe (Input Identity))]
watchSim = runSimulator @(Core.State Identity) @ramSize @progSize watch

simResult :: forall ramSize progSize. (KnownNat ramSize, KnownNat (MemSizeFrom progSize ramSize)) => Vec progSize Word -> Vec (MemSizeFrom progSize ramSize) Byte
simResult = runSimulator @(Core.State Identity) @ramSize @progSize result

