{-# LANGUAGE UndecidableInstances #-}

module Util where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Proxy (Proxy (..))
import qualified GHC.TypeNats
import Instruction
import Regfile
import Types
import Prelude hiding (Ordering (..), Word, init, iterate, log, map, not, repeat, replicate, take, undefined, (!!), (&&), (++), (||))

data CircuitSim m i s o = CircuitSim
  { circuitInput :: i,
    circuitState :: s,
    circuitStep :: i -> s -> m (s, o),
    circuitNext :: o -> m (Maybe i)
  }

run1 :: (Monad m) => CircuitSim m i s o -> m (s, o, Maybe i)
run1 (CircuitSim i s step next) = do
  (s', o) <- step i s
  mi' <- next o
  pure (s', o, mi')

watch :: (Monad m) => CircuitSim m i s o -> m [(s, o, Maybe i)]
watch c = do
  (s', o, mi') <- run1 c
  case mi' of
    Nothing -> pure [(s', o, mi')]
    Just i' -> do
      rest <- watch $ c {circuitInput = i', circuitState = s'}
      pure $ (s', o, mi') : rest

result :: (MonadMemory m) => CircuitSim m i s o -> m (Vec MEM_SIZE_BYTES Byte)
result c = watch c *> getRAM

pageIO :: (Show a) => [a] -> IO ()
pageIO = mapM_ $ \a -> do
  putStrLn $ show a
  putStrLn ""
  putStrLn "------------------------"
  putStrLn "Press Enter to continue."
  putStrLn "------------------------"
  void getLine

class (Monad m) => MonadMemory m where
  getRAM :: m (Vec MEM_SIZE_BYTES Byte)
  putRAM :: Vec MEM_SIZE_BYTES Byte -> m ()
  getRegfile :: m Regfile
  putRegfile :: Regfile -> m ()

ramRead :: (MonadMemory m) => Address -> m Word
ramRead addr = readWord addr <$> getRAM

ramWrite :: (MonadMemory m) => Address -> Size -> Word -> m ()
ramWrite addr size w = do
  ram <- getRAM
  putRAM $ write size addr w ram

regRead :: (MonadMemory m) => RegIdx -> m Word
regRead idx = lookupRF idx <$> getRegfile

regWrite :: (MonadMemory m) => RegIdx -> Word -> m ()
regWrite idx val = do
  regfile <- getRegfile
  putRegfile $ modifyRF idx val regfile

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

type RAM_SIZE = 50

type RAM_SIZE_BYTES = ((GHC.TypeNats.*) RAM_SIZE 4)

type PROG_SIZE = 50

type MEM_SIZE = RAM_SIZE + PROG_SIZE

type MEM_SIZE_BYTES = ((GHC.TypeNats.*) MEM_SIZE 4)

initPc :: Address
initPc = fromIntegral $ natVal (Proxy @RAM_SIZE_BYTES)

mkProg ::
  forall size.
  ( KnownNat (size + (PROG_SIZE - size)),
    KnownNat (PROG_SIZE - size),
    KnownNat size,
    KnownNat PROG_SIZE,
    PROG_SIZE ~ (size + (PROG_SIZE - size))
  ) =>
  Vec size Instruction ->
  Vec PROG_SIZE Word
mkProg prog =
  prog' ++ (repeat 0 :: Vec (PROG_SIZE - size) Word)
  where
    prog' = map encode prog

mkRAM :: Vec PROG_SIZE Word -> Vec MEM_SIZE_BYTES Byte
mkRAM prog =
  (repeat 0 :: Vec RAM_SIZE_BYTES Byte) ++ vecWordToByte prog
