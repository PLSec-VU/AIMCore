{-# LANGUAGE UndecidableInstances #-}

module Memory.Types
  ( MonadMemory (..),
    MemOps (..),
    MemBytes,
    MemFn (..),
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

-- | The byte-addressed memory the ISA model and the proof harness use. Same
-- size as the one the simulation tests use.
type MemBytes = Vec MEM_SIZE_BYTES Byte

-- | The operations a memory representation has to provide.
--
-- Parameterised for the same reason as 'RegFile.RegFileOps': the @Vec@-backed
-- 'MemBytes' cannot be symbolically executed, while the function-backed 'MemFn'
-- can.
class MemOps m where
  memReadWord :: Address -> m -> Word
  memWriteWord :: Size -> Address -> Word -> m -> m

  -- | Single byte, which is what the pointwise form of the invariant compares.
  memReadByte :: Address -> m -> Byte

instance (KnownNat n) => MemOps (Vec n Byte) where
  memReadWord = readWord
  memWriteWord = write
  memReadByte a m = m !! a

-- | Verification-only memory: a function rather than a container.
newtype MemFn = MemFn {memByte :: Address -> Byte}

instance MemOps MemFn where
  memReadWord a (MemFn m) = m (a + 3) ++# m (a + 2) ++# m (a + 1) ++# m a
  memWriteWord size a w m =
    case size of
      Byte -> put a b0 m
      Half -> put (a + 1) b1 (put a b0 m)
      Word -> put (a + 3) b3 (put (a + 2) b2 (put (a + 1) b1 (put a b0 m)))
    where
      b0 = slice d7 d0 w
      b1 = slice d15 d8 w
      b2 = slice d23 d16 w
      b3 = slice d31 d24 w
      put i v (MemFn f) = MemFn (\j -> if j == i then v else f j)

  memReadByte a (MemFn m) = m a

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
