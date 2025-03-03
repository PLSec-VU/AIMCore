{-# LANGUAGE UndecidableInstances #-}

module Util where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import qualified GHC.TypeNats
import Instruction
import Regfile
import Types
import Prelude hiding (Ordering (..), Word, init, iterate, log, map, not, repeat, replicate, take, undefined, (!!), (&&), (++), (||))

type Log = [String]

class (Monad m) => MonadLog m where
  log :: String -> m ()
  withLog :: m a -> m (a, Log)

instance (MonadWriter Log m) => MonadLog m where
  log = tell . pure
  withLog = listen

data CircuitSim m i s o = CircuitSim
  { circuitInput :: i,
    circuitState :: s,
    circuitStep :: i -> s -> m (s, o),
    circuitNext :: o -> m (Maybe i)
  }

run :: (Monad m) => CircuitSim m i s o -> m (s, o)
run (CircuitSim i s step next) =
  go i s
  where
    go i s = do
      (s', o) <- step i s
      mi' <- next o
      case mi' of
        Nothing -> pure (s', o)
        Just i' -> go i' s'

runIO :: (Monad m, MonadLog m, MonadIO m) => CircuitSim m i s o -> m ()
runIO cs = do
  (_, log) <- withLog $ run cs
  liftIO $ forM_ log $ \msg -> do
    putStrLn msg
    putStrLn "Press Enter to continue."
    void getLine

class (Monad m) => MonadMemory m where
  ramRead :: Address -> m Word
  ramWrite :: Address -> Size -> Word -> m ()
  regRead :: RegIdx -> m Word
  regWrite :: RegIdx -> Word -> m ()

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
