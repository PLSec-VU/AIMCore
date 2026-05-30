module Elf.ElfLoader
  ( loadElf
  , readElf
  , startAddr
  , baseAddr
  , Instrument
  , runElf
  , readStringFromMemory
  , getElfSegments
  ) where

import Access
import Clash.Explicit.Prelude (Unsigned, bitCoerce, zeroExtend)
import Clash.Explicit.Prelude.Safe ((.&.))
import Control.Monad (forM_, forM)
import Control.Monad.Catch
import qualified Core
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Int
import Data.Word
import Types
import Memory.Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, undefined, (&&), (++), (||))
import qualified Prelude as P

loadElf :: (MonadMemory m) => Elf -> m ()
loadElf elf@(Elf classS _) = withSingElfClassI classS $ do
  let segments = getElfSegments elf
  forM_ segments $ \(addr, bs) -> do
    let bytes = BS.unpack bs
    forM_ (zip [0 ..] bytes) $ \(i, byte) ->
      ramWrite (addr + fromIntegral i) Byte (zeroExtend $ bitCoerce byte)

-- | Extract all loadable sections from an ELF into a list of (Address, ByteString)
getElfSegments :: Elf -> [(Address, BS.ByteString)]
getElfSegments (Elf classS elfs) = withSingElfClassI classS $
  let loadable = loadableSegments elfs
   in P.concatMap extractSegmentSections loadable
  where
    extractSegmentSections :: (SingElfClassI a) => ElfXX 'Segment a -> [(Address, BS.ByteString)]
    extractSegmentSections ElfSegment {..} =
      let sections = gatherSections epData
       in -- We ignore epAddMemSize here to avoid massive allocations;
          -- BSS should be handled by the memory model (zero-initialized).
          sections

    gatherSections :: (SingElfClassI a) => ElfListXX a -> [(Address, BS.ByteString)]
    gatherSections ElfListNull = []
    gatherSections (ElfListCons (ElfSection {esData = ElfSectionData d, ..}) xs) =
      (fromIntegral esAddr, BSL.toStrict d) : gatherSections xs
    gatherSections (ElfListCons _ xs) = gatherSections xs

loadableSegments :: ElfListXX a -> [ElfXX 'Segment a]
loadableSegments (ElfListCons v@(ElfSegment {..}) l) =
  if epType == PT_LOAD
    then v : loadableSegments l
    else loadableSegments l
loadableSegments (ElfListCons _ l) = loadableSegments l
loadableSegments ElfListNull = []

readElf :: FilePath -> IO Elf
readElf path = BSL.readFile path >>= parseElf

startAddr :: (MonadCatch m) => Elf -> m Word32
startAddr (Elf SELFCLASS32 elfs) = do
  ep <- ehEntry <$> elfFindHeader elfs
  if ep <= 0x7fffffff
    then pure ep
    else esAddr <$> elfFindSectionByName elfs ".text.init"
startAddr (Elf SELFCLASS64 _) = throwM $ userError "64-bit ELF not supported"

baseAddr :: (MonadCatch m) => Elf -> m Word32
baseAddr (Elf SELFCLASS64 _) = throwM $ userError "64-bit ELF not supported"
baseAddr (Elf SELFCLASS32 elfs) = go elfs
  where
    go (ElfListCons (ElfSegment {..}) _) = pure $ fromIntegral epVirtAddr
    go (ElfListCons _ l) = go l
    go ElfListNull = pure 0

-- | Read a string from memory starting at the given address for count bytes
readStringFromMemory :: (MonadMemory m) => Unsigned 32 -> Unsigned 32 -> m String
readStringFromMemory addr count = do
  bytes <-
    forM [0 .. count - 1] $ \i -> do
      byte <- ramRead False (addr + i) Byte
      pure $ fromIntegral (byte .&. 0xFF)
  pure $ P.map chr $ takeWhile (/= 0) bytes

-- | Called on each step of the ELF execution, returning whether to continue execution and optional syscall return.
type Instrument f m = Core.Input f -> Core.State f -> Core.Output f -> Int -> m (Bool, Maybe (f Types.Word))

runElf :: forall f m. (MonadMemory m) => Instrument f m -> CircuitSim m (Core.Input f) (Core.State f) (Core.Output f) -> m ()
runElf instr c = watchWithStep (0 :: Int) c
  where
    watchWithStep stepCount sim@(CircuitSim i s step next) = do
      (s', o) <- step i s
      mi' <- next o
      (cont, mRet) <- instr i s' o stepCount
      case mi' of
        Just i' | cont -> do
          let i'' = case mRet of
                      Just ret -> i' { Core.inputMem = ret }
                      Nothing -> i'
          watchWithStep (stepCount + 1) $ sim {circuitInput = i'', circuitState = s'}
        _ -> pure ()