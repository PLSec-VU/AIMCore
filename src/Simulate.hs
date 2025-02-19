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
import GHC.TypeNats
import Instruction hiding (decode)
import Pipe
import Regfile
import Text.Read (readMaybe)
import Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))
import qualified Prelude

data Mem n = Mem
  { memRAM :: Vec n Byte,
    memRf :: Regfile
  }
  deriving (Eq, Show, Generic, NFDataX)

readWord :: (KnownNat n) => Address -> Vec n Byte -> Word
readWord addr m =
  (m !! (addr + 3)) ++# (m !! (addr + 2)) ++# (m !! (addr + 1)) ++# (m !! addr)

write :: (KnownNat n) => Size -> Address -> Word -> Vec n Byte -> Vec n Byte
write size addr w mem =
  let b0 = slice d7 d0 w
      b1 = slice d15 d8 w
      b2 = slice d23 d16 w
      b3 = slice d31 d24 w
      writeByte =
        replace addr b0
      writeHalf =
        replace (addr + 1) b1 . writeByte
      writeWord =
        replace (addr + 3) b3
          . replace (addr + 2) b2
          . writeHalf
   in case size of
        Byte -> writeByte mem
        Half -> writeHalf mem
        Word -> writeWord mem

class (Monad m) => MonadMemory m where
  ramRead :: Address -> m Word
  ramWrite :: Address -> Size -> Word -> m ()
  regRead :: RegIdx -> m Word
  regWrite :: RegIdx -> Word -> m ()

instance (KnownNat n, MonadState (Mem n) m) => MonadMemory m where
  ramRead addr = gets $ readWord addr . memRAM
  ramWrite addr size w =
    modify $ \s -> s {memRAM = write size addr w $ memRAM s}
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
simMemStep (Output mem rs1 rs2 rd hlt) = do
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
      | Just (MemAccess addr size mval) <- getFirst mem =
          case mval of
            Nothing -> ramRead addr
            Just val -> do
              ramWrite addr size val
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

simToHalt' :: (KnownNat n) => Vec n Byte -> Word
simToHalt' = (readWord 0) . memRAM . simToHalt

simToHalt :: forall n. (KnownNat n) => Vec n Byte -> Mem n
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

simIO :: forall n. (KnownNat n) => Vec n Byte -> IO ()
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

type RAM_SIZE = (GHC.TypeNats.*) 4 50

type MEM_SIZE n = (+) RAM_SIZE ((+) ((GHC.TypeNats.*) n 4) RAM_SIZE)

mkRAM :: (KnownNat n) => Vec n Word -> Vec (MEM_SIZE n) Byte
mkRAM prog =
  (repeat 0 :: Vec RAM_SIZE Byte) ++ vecWordToByte prog ++ repeat 0
