{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Monoid
import RegFile
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

data Mem f n = Mem
  { memRAM :: Vec n (f Byte),
    memRF :: RegFile f
  }

deriving instance (KnownNat n, Eq (f Word), Eq (f Byte)) => Eq (Mem f n)

deriving instance
  ( Show (f String),
    Access f,
    Show (f Word),
    Show (f Byte)
  ) =>
  Show (Mem f n)

deriving instance Generic (Mem f n)

deriving instance
  ( KnownNat n,
    Generic (f Word),
    NFDataX (f Word),
    Generic (f Byte),
    NFDataX (f Byte)
  ) =>
  NFDataX (Mem f n)

instance (KnownNat n, Access f, Monad m, MonadState (Mem f n) m) => MonadMemory f m where
  getRegFile = gets memRF
  putRegFile rf = modify $ \s -> s {memRF = rf}
  ramRead addr = readWord addr <$> gets memRAM
  ramWrite addr size w = do
    ram <- gets memRAM
    modify $ \s -> s {memRAM = write size addr w ram}

result :: (MonadState (Mem f n) m) => CircuitSim m i s o -> m (Vec n (f Byte))
result c = watch c *> gets memRAM

simulator :: forall m f. (MonadMemory f m) => CircuitSim m (Input f) (Core.State f) (Output f)
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
        simCore = flip $ runRWS $ Core.runCPUM simCoreM
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
                  inputMem = mem_in,
                  inputRs1 = rs1',
                  inputRs2 = rs2'
                }
      where
        doRegFile :: m (f Word, f Word)
        doRegFile = do
          maybe (pure ()) (uncurry regWrite) $ getFirst rd
          rs1' <- maybe (pure $ pure 0) regRead $ getFirst rs1
          rs2' <- maybe (pure $ pure 0) regRead $ getFirst rs2
          pure (rs1', rs2')

        doMemory :: m (f Word, Bool)
        doMemory
          | Just (MemAccess isInstr addr size mval) <- getFirst mem =
              case mval of
                Nothing -> (,isInstr) <$> ramRead addr
                Just val -> do
                  ramWrite addr size val
                  pure (pure 0, isInstr)
          | otherwise = pure (pure 0, False)

runSimulator ::
  forall f ramSize progSize a.
  ( Access f,
    KnownNat ramSize,
    KnownNat (MemSizeFrom progSize ramSize)
  ) =>
  ( CircuitSim (State (Mem f (MemSizeFrom progSize ramSize))) (Input f) (Core.State f) (Output f) ->
    State (Mem f (MemSizeFrom progSize ramSize)) a
  ) ->
  Vec progSize (f Word) ->
  a
runSimulator f = evalState (f simulator) . flip Mem initRF . mkRAM

watchSim ::
  forall f ramSize progSize.
  ( Access f,
    KnownNat ramSize,
    KnownNat (MemSizeFrom progSize ramSize)
  ) =>
  Vec progSize (f Word) ->
  [(Core.State f, Output f, Maybe (Input f))]
watchSim = runSimulator @f @ramSize @progSize watch

simResult ::
  forall
    f
    ramSize
    progSize.
  ( Access f,
    KnownNat ramSize,
    KnownNat (MemSizeFrom progSize ramSize)
  ) =>
  Vec progSize (f Word) ->
  Vec (MemSizeFrom progSize ramSize) (f Byte)
simResult = runSimulator result
