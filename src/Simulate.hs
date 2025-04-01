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

type SimM n = RWS () () (Mem n)

simulator :: forall m. (MonadMemory m) => CircuitSim m Input Pipe Output
simulator =
  CircuitSim
    { circuitInput = initInput,
      circuitState = initPipe,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: (MonadMemory m) => Input -> Pipe -> m (Pipe, Output)
    step i s = do
      let (ctrl', s', o) = simPipe s i
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

    next :: (MonadMemory m) => Output -> m (Maybe Input)
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

prog1 =
  mkProg $
    -- r2 := r0 + 5
    IType (Arith ADD) 2 0 5
      :>
      -- mem[0 + r0] := r2
      SType Word 0 0 2
      :> halt
      :> Nil

prog2 =
  mkProg $
    -- r2 := r0 + 5
    IType (Arith ADD) 2 0 5
      :>
      -- mem[0 + r0] := r2
      SType Word 0 0 2
      :>
      -- r3 := mem[r0 + 0],
      IType (Load Word Signed) 3 0 0
      :>
      -- r4 := r0 + r3
      RType ADD 4 0 3
      :>
      -- mem[1 + r0] := r4
      SType Word 4 0 4
      :> halt
      :> Nil

prog3 =
  mkProg $
    -- r2 := r0 + 3
    IType (Arith ADD) 2 0 3
      :>
      -- r3 := r0 + r2
      RType ADD 3 0 2
      :>
      -- r2 == r3 ? jump pc + 8
      BType EQ 8 2 3
      :>
      -- mem[0 + r0] := r2
      SType Word 0 0 2
      :>
      -- mem[1 + r0] := r2
      SType Word 4 0 2
      :> halt
      :> Nil

sumTo :: Int -> Vec PROG_SIZE Word
sumTo n =
  mkProg $
    ( ( unsafeFromList
          [ -- r1 := r0 + n
            IType (Arith ADD) 1 0 $ fromIntegral n,
            -- r2 := 0 (res = 0)
            IType (Arith ADD) 2 0 0,
            -- r1 == r0 ? jump pc + 16
            BType EQ 16 1 0,
            -- r2 := r2 + r1 (res += n)
            RType ADD 2 2 1,
            -- r1 := r1 - 1 (n -= 1)
            IType (Arith ADD) 1 1 (-1),
            -- jump back to the branch
            JType 0 (-12),
            -- mem[0] := r2
            SType Word 0 0 2,
            halt
          ]
      ) ::
        Vec 8 Instruction
    )
