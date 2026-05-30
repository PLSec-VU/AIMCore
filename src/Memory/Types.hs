{-# LANGUAGE UndecidableInstances #-}

module Memory.Types
  ( MonadMemory (..),
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
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Data.Proxy (Proxy (..))
import qualified GHC.TypeNats
import Instruction
import Types
import Prelude hiding (Ordering (..), Word, init, iterate, log, map, not, repeat, replicate, take, undefined, (!!), (&&), (++), (||))

class Monad m => MonadMemory m where
  ramRead :: Bool -> Address -> Size -> m Word
  ramWrite :: Address -> Size -> Word -> m ()
  -- | Mark a region of memory as public or secret
  markMemoryRegion :: Address -> Address -> Bool -> m ()
  -- | Check if a memory address is marked as secret
  isMemorySecret :: Address -> m Bool

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
  (repeat 0 :: Vec ramSize Byte) ++ Clash.Prelude.concatMap splitWord prog
  where
    splitWord :: Word -> Vec 4 Byte
    splitWord word =
      let b0 = slice d7 d0 word
          b1 = slice d15 d8 word
          b2 = slice d23 d16 word
          b3 = slice d31 d24 word
       in b0 :> b1 :> b2 :> b3 :> Nil
