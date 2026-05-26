{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module STSimulate
  ( STMemory (..),
    runSTSim,
    stSimulator,
    newSTMemory,
    loadSTMemory,
    readProgramWord,
    runUntilHalt,
  )
where

import Access (unAccess, Access)
import Clash.Prelude hiding (Ordering (..), Word, init, lift)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Core
import Types
import Util
import Data.Monoid (First (getFirst))
import Prelude hiding (Ordering (..), Word, init, lines, not, undefined, (&&), (||))
import qualified Prelude as P
import Data.Word (Word8)

-- | Separated memory state in ST.
data STMemory s = STMemory
  { -- | Read-only program memory, indexed by address.
    stProgram :: Map Address ByteString,
    -- | Writable data memory array.
    stData :: STUArray s Address Word8,
    -- | Base address of the data array.
    stDataBase :: Address,
    -- | Initial PC from ELF.
    stEntry :: Address
  }

-- | Implementation of tagged memory read/write logic for the simulator's 'next' function.
nextST :: forall f s. (Access f) => STMemory s -> Output f -> ST s (Maybe (Input f))
nextST st (Output mem syscall hlt)
  | getFirst hlt == Just True = pure Nothing
  | getFirst syscall == Just True = pure Nothing -- Halt on syscall for simplicity in this model
  | otherwise = case getFirst mem of
      Nothing -> pure $ Just $ initInput
      -- W^X: Instruction reads directly from the program's static data.
      Just (MemAccess True addr size mval) -> do
        let word = readProgramWord addr (stProgram st)
        pure $ Just $ Input True (pure word)
      -- Data accesses (reads and writes) go to the data array.
      Just (MemAccess False addr size mval) -> do
        let offset = addr - stDataBase st
        case mval of
          Nothing -> do
            val <- readDataArray offset size (stData st)
            pure $ Just $ Input False (pure val)
          Just val -> do
            writeDataArray offset size (unAccess val) (stData st)
            pure $ Just $ Input False (pure 0)

-- | Read a 32-bit word from the program segments.
readProgramWord :: Address -> Map Address ByteString -> Word
readProgramWord addr prog =
  let b0 = readProgramByte addr
      b1 = readProgramByte (addr + 1)
      b2 = readProgramByte (addr + 2)
      b3 = readProgramByte (addr + 3)
   in b3 ++# b2 ++# b1 ++# b0
  where
    readProgramByte a = case Map.lookupLE a prog of
      Just (base, bs) ->
        let offset = fromIntegral (a - base)
         in if offset < BS.length bs
              then bitCoerce (BS.index bs offset)
              else 0
      Nothing -> 0

-- | Read a sized value from the data array.
readDataArray :: Address -> Size -> STUArray s Address Word8 -> ST s Word
readDataArray addr size arr = case size of
  Byte -> do
    b0 <- readArray arr addr
    pure $ zeroExtend (bitCoerce b0 :: Byte)
  Half -> do
    b0 <- readArray arr addr
    b1 <- readArray arr (addr + 1)
    pure $ zeroExtend ((bitCoerce b1 :: Byte) ++# (bitCoerce b0 :: Byte))
  Word -> do
    b0 <- readArray arr addr
    b1 <- readArray arr (addr + 1)
    b2 <- readArray arr (addr + 2)
    b3 <- readArray arr (addr + 3)
    pure $ (bitCoerce b3 :: Byte) ++# (bitCoerce b2 :: Byte) ++# (bitCoerce b1 :: Byte) ++# (bitCoerce b0 :: Byte)

-- | Write a sized value to the data array.
writeDataArray :: Address -> Size -> Word -> STUArray s Address Word8 -> ST s ()
writeDataArray addr size w arr = case size of
  Byte -> writeArray arr addr (bitCoerce $ slice d7 d0 w)
  Half -> do
    writeArray arr addr (bitCoerce $ slice d7 d0 w)
    writeArray arr (addr + 1) (bitCoerce $ slice d15 d8 w)
  Word -> do
    writeArray arr addr (bitCoerce $ slice d7 d0 w)
    writeArray arr (addr + 1) (bitCoerce $ slice d15 d8 w)
    writeArray arr (addr + 2) (bitCoerce $ slice d23 d16 w)
    writeArray arr (addr + 3) (bitCoerce $ slice d31 d24 w)

-- | Initialize the simulator with ST memory.
stSimulator :: forall f s. (Access f) => STMemory s -> CircuitSim (ST s) (Input f) (Core.State f) (Output f)
stSimulator st =
  CircuitSim
    { circuitInput = initInput,
      circuitState = init { stateFePc = stEntry st },
      circuitStep = \i s -> pure $ circuit s i,
      circuitNext = nextST st
    }

-- | Create a new STMemory.
newSTMemory :: forall s. Address -> Address -> [(Address, ByteString)] -> ST s (STMemory s)
newSTMemory entry base segments = do
  -- Use a smaller 16MB array to start with, as RISC-V tests are small.
  let dataSize = 0x1000000
  arr <- newArray (0, dataSize) 0
  let progMap = Map.fromList segments
  let stMem = STMemory progMap arr base entry
  loadSTMemory stMem segments
  pure stMem

-- | Load segments into the STMemory.
loadSTMemory :: forall s. STMemory s -> [(Address, ByteString)] -> ST s ()
loadSTMemory st segments = do
  forM_ segments $ \(addr, bs) -> do
    let offset = addr - stDataBase st
    -- Check if within bounds before loading
    when (offset < 0x1000000) $ do
      let len = fromIntegral (BS.length bs)
      let count = min len (0x1000000 - offset)
      forM_ (P.zip [0..count-1] (BS.unpack (BS.take (fromIntegral count) bs))) $ \(i, byte) ->
        writeArray (stData st) (offset + i) (fromIntegral byte)

-- | Run a simulation in the ST monad.
runSTSim :: forall i s o a. (forall st. CircuitSim (ST st) i s o -> ST st a) -> CircuitSim (ST s) i s o -> ST s a
runSTSim f sim = f sim

-- | Utility to run a full simulation until halt.
--   Uses deepseqX to ensure the state is fully evaluated and prevent thunk leaks.
runUntilHalt :: forall f s. (Access f, NFDataX (f Word), Generic (f Word)) => STMemory s -> ST s (Core.State f)
runUntilHalt st = go (stSimulator st)
  where
    go sim = do
      (s', _, mi') <- run1 sim
      -- Use deepseqX to fully evaluate the state and avoid memory leaks
      deepseqX s' $ case mi' of
        Nothing -> pure s'
        Just i' -> i' `seq` go (sim { circuitInput = i', circuitState = s' })
