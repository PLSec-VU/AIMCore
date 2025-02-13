{-# LANGUAGE KindSignatures #-}
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
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))
import qualified Prelude

data Mem n = Mem
  { memRAM :: Vec n Word,
    memRf :: Regfile
  }
  deriving (Eq, Show, Generic, NFDataX)

class (Monad m) => MonadMemory m where
  ramRead :: Address -> m Word
  ramWrite :: Address -> Word -> m ()
  regRead :: RegIdx -> m Word
  regWrite :: RegIdx -> Word -> m ()

instance (KnownNat n, MonadState (Mem n) m) => MonadMemory m where
  ramRead addr = gets ((!! addr) . memRAM)
  ramWrite addr w =
    modify $ \s -> s {memRAM = replace addr w $ memRAM s}
  regRead idx = do
    gets $ lookupRF idx . memRf
  regWrite idx val = do
    modify $ \s -> s {memRf = modifyRF idx val $ memRf s}

type Log = [String]

log :: (MonadWriter Log m) => String -> m ()
log = tell . pure

report :: (MonadWriter Log m, MonadIO m) => m a -> m a
report m = do
  (a, w) <- listen m
  liftIO $ putStrLn $ unlines w
  pure a

simMemStep :: forall m. (MonadMemory m) => Output -> m Input
simMemStep (Output mem rs1 rs2 rd) = do
  (rs1', rs2') <- doRegFile
  mem_in <- doMemory
  pure $
    Input
      { inputMem = mem_in,
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

    doMemory :: m Word
    doMemory
      | Just (MemAccess addr mval) <- getFirst mem =
          case mval of
            Nothing -> ramRead addr
            Just val -> do
              ramWrite addr val
              pure 0
      | otherwise = pure 0

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

simStep :: (MonadWriter Log m, MonadMemory m) => Input -> Pipe -> m (Input, Pipe)
simStep i s = do
  let (ctrl', s', o) = simPipe s i
  log $
    unlines
      [ "Input:",
        "--------------------",
        show i,
        "",
        "State:",
        "--------------------",
        show s,
        "",
        "Output:",
        "--------------------",
        show o,
        "",
        "Control:",
        "--------------------",
        show ctrl'
      ]
  i' <- simMemStep o
  pure (i', s')

iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = pure a
iterateM n m a = iterateM (n - 1) m =<< m a

simToHalt :: forall n. (KnownNat n) => Vec n Word -> Mem n
simToHalt =
  fst
    . execRWS
      (simulate initInput initPipe)
      ()
    . flip Mem initRF
  where
    simulate :: Input -> Pipe -> RWS () Log (Mem n) ()
    simulate _ s
      | pipeHalt s = pure ()
    simulate i s = simStep i s >>= uncurry simulate

simIO :: forall n. (KnownNat n) => Vec n Word -> IO ()
simIO =
  void
    . runRWST
      (simulate initInput initPipe 0)
      ()
    . flip Mem initRF
  where
    simulate :: Input -> Pipe -> Int -> RWST () Log (Mem n) IO ()
    simulate i s n =
      void $ forever $ do
        lift $ putStrLn "Press Enter to continue or enter a number to do that many steps."
        n <- (fromMaybe 1 . readMaybe) <$> lift getLine
        (i', s') <- iterateM n (report . uncurry simStep) (i, s)
        simulate i' s' n

type RAM_SIZE = 50

mkRAM :: Vec n Word -> Vec ((+) RAM_SIZE ((+) n 20)) Word
mkRAM prog =
  (repeat 0 :: Vec RAM_SIZE Word) ++ prog ++ repeat 0

prog1 :: Vec 3 Word
prog1 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        halt
      ]

prog2 :: Vec 5 Word
prog2 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- r3 := mem[r0 + 0],
        IType (Load Word Signed) 3 0 0,
        -- r4 := r0 + r3
        RType ADD 4 0 3,
        -- mem[1 + r0] := r4
        SType Word 1 0 4
      ]

prog3 :: Vec 5 Word
prog3 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 3
        IType (Arith ADD) 2 0 3,
        -- r3 := r0 + r2
        RType ADD 3 0 2,
        -- r2 == r3 ? jump pc + 2
        BType EQ 2 2 3,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- mem[1 + r0] := r2
        SType Word 1 0 2
      ]

prog4 :: Vec 2 Word
prog4 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- mem[1 + r0] := r2
        SType Word 1 0 2
      ]

prog5 :: Vec 3 Word
prog5 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- r3 := mem[r0 + 0]
        IType (Load Word Signed) 3 0 0
        ---- r4 := r3 + r3
        -- RType ADD 4 3 3,
        ---- r5 := r4 - r3
        -- RType SUB 5 4 3,
        ---- r6 := r4 + r5
        -- RType ADD 6 4 5,
        ---- mem[1 + r0] := r6
        -- SType Word 1 0 6,
        ---- r7 := mem[r0 + 1]
        -- IType (Load Word Signed) 7 0 1,
        ---- r8 := r7 + r3
        -- RType ADD 8 7 3,
        ---- mem[2 + r0] := r8
        -- SType Word 2 0 8
      ]
