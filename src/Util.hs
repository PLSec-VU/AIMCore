{-# LANGUAGE UndecidableInstances #-}

module Util
  ( CircuitSim (..),
    run1,
    watch,
    cmpIO,
    pageIO,
    MonadMemory (..),
    ramRead,
    ramWrite,
    regRead,
    regWrite,
    readWord,
    write,
    RAM_SIZE,
    RAM_SIZE_BYTES,
    PROG_SIZE,
    MEM_SIZE,
    MEM_SIZE_BYTES,
    MemSizeFrom,
    initPc,
    mkProg,
    mkRAM,
    try,
    ifM,
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Proxy (Proxy (..))
import qualified GHC.TypeNats
import Instruction
import RegFile
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

cmpIO :: (Show a, Show b) => [(a, b)] -> IO ()
cmpIO = mapM_ $ \(a, b) -> do
  print a
  putStrLn ""
  putStrLn "*************************"
  putStrLn ""
  print b
  putStrLn ""
  putStrLn "------------------------"
  putStrLn "Press Enter to continue."
  putStrLn "------------------------"
  void getLine

pageIO :: (Show a) => [a] -> IO ()
pageIO = mapM_ $ \a -> do
  print a
  putStrLn ""
  putStrLn "------------------------"
  putStrLn "Press Enter to continue."
  putStrLn "------------------------"
  void getLine

class (KnownNat n, Monad m) => MonadMemory n m where
  getRegFile :: m RegFile
  putRegFile :: RegFile -> m ()
  ramRead :: Address -> m Word
  ramWrite :: Address -> Size -> Word -> m ()

  -- ramRead :: (MonadMemory n m) => Address -> m Word
  -- ramRead addr = readWord addr <$> getRAM @n

  -- ramWrite :: (MonadMemory n m) => Address -> Size -> Word -> m ()
  -- ramWrite addr size w = do
  --   ram <- getRAM @n
  --   putRAM $ write size addr w ram

regRead :: forall n m. (MonadMemory n m) => RegIdx -> m Word
regRead idx = lookupRF idx <$> getRegFile @n

regWrite :: forall n m. (MonadMemory n m) => RegIdx -> Word -> m ()
regWrite idx val = do
  regfile <- getRegFile @n
  putRegFile @n $ modifyRF idx val regfile

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
  forall progSize size.
  ( KnownNat (progSize - size),
    progSize ~ (size + (progSize - size))
  ) =>
  Vec size Instruction ->
  Vec progSize Word
mkProg prog =
  prog' ++ (repeat 0 :: Vec (progSize - size) Word)
  where
    prog' = map encode prog

type MemSizeFrom progSize ramSizeBytes =
  ramSizeBytes + ((GHC.TypeNats.*) progSize 4)

mkRAM :: forall progSize ramSize. (KnownNat ramSize) => Vec progSize Word -> Vec (MemSizeFrom progSize ramSize) Byte
mkRAM prog =
  (repeat 0 :: Vec ramSize Byte) ++ vecWordToByte prog

try :: (Monad m) => MaybeT m () -> m ()
try m = runMaybeT m >>= maybe (pure ()) pure

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb mt mf = do
  b <- mb
  if b then mt else mf
