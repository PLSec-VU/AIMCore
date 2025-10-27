module ElfLoader (loadElf, readElf, startAddr, Instrument, runElf, readStringFromMemory) where

import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Word
import Data.Elf
import Data.Elf.Headers
import Data.Elf.Constants
import Control.Monad.Catch
import "uc-risc-v" Types
import Util
import Control.Monad.IO.Class
import qualified Core
import Control.Monad (when)
import Numeric (showHex)
import Data.Monoid (First(getFirst))
import Clash.Explicit.Prelude (Unsigned, bitCoerce)
import Clash.Explicit.Prelude.Safe ((.&.))
import Data.Char (chr)
import Control.Exception (throwIO)

type LoadFunc m = Address -> BSL.ByteString -> m ()

loadableSegments :: ElfListXX a -> [ElfXX 'Segment a]
loadableSegments (ElfListCons v@(ElfSegment{..}) l) =
    if epType == PT_LOAD
        then v : loadableSegments l
        else loadableSegments l
loadableSegments (ElfListCons _ l) = loadableSegments l
loadableSegments ElfListNull = []

copyData :: (Monad m, SingElfClassI a) => ElfListXX a -> Int64 -> LoadFunc m -> m ()
copyData ElfListNull _ _ = pure ()
copyData (ElfListCons (ElfSection{esData = ElfSectionData textData, ..}) xs) zeros f = do
    f (fromIntegral esAddr) $ BSL.append textData (BSL.replicate zeros 0)
    copyData xs zeros f
copyData (ElfListCons _ xs) zeros f = copyData xs zeros f

loadSegment :: (Monad m, SingElfClassI a) => LoadFunc m -> ElfXX 'Segment a -> m ()
loadSegment loadFunc ElfSegment{..} =
    copyData epData (fromIntegral epAddMemSize) loadFunc

loadElf :: (Monad m) => Elf -> LoadFunc m -> m ()
loadElf (Elf classS elfs) loadFunc = withSingElfClassI classS $ do
    let loadable = loadableSegments elfs
    mapM_ (loadSegment loadFunc) loadable

readElf :: FilePath -> IO Elf
readElf path = BSL.readFile path >>= parseElf

startAddr :: (MonadCatch m) => Elf -> m Word32
startAddr (Elf SELFCLASS32 elfs) = ehEntry <$> elfFindHeader elfs

-- | Read a string from memory starting at the given address for count bytes
readStringFromMemory :: (MonadMemory m) => Unsigned 32 -> Unsigned 32 -> m String
readStringFromMemory addr count = do
  bytes <- mapM (\i -> do
    byte <- ramRead (addr + i)
    pure $ fromIntegral (byte .&. 0xFF)) [0..count-1]
  pure $ map chr bytes

type Instrument m = Core.Input -> Core.State -> Core.Output -> Int -> m Bool

runElf :: forall m. (MonadMemory m) => Instrument m -> CircuitSim m Core.Input Core.State Core.Output -> m ()
runElf instr c = watchWithStep (0 :: Int) c
  where
    watchWithStep step sim = do
      (s', o, mi') <- run1 sim
      cont <- instr (circuitInput sim) s' o step
      case mi' of
        Just i' | cont -> do
          watchWithStep (step + 1) $ sim {circuitInput = i', circuitState = s'}
        _ -> pure ()