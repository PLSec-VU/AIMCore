module Elf.ElfLoader
  ( loadElf
  , readElf
  , startAddr
  , baseAddr
  , Instrument
  , runElf
  , readStringFromMemory
  ) where

import Clash.Explicit.Prelude (Unsigned, bitCoerce)
import Clash.Explicit.Prelude.Safe ((.&.))
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Core
import qualified Data.ByteString.Lazy as BSL
import Data.Char (chr)
import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Int
import Data.Monoid (First (getFirst))
import Data.Word
import Numeric (showHex)
import Types
import Util

type LoadFunc m = Address -> BSL.ByteString -> m ()

loadableSegments :: ElfListXX a -> [ElfXX 'Segment a]
loadableSegments (ElfListCons v@(ElfSegment {..}) l) =
  if epType == PT_LOAD
    then v : loadableSegments l
    else loadableSegments l
loadableSegments (ElfListCons _ l) = loadableSegments l
loadableSegments ElfListNull = []

copyData :: (Monad m, SingElfClassI a) => ElfListXX a -> Int64 -> LoadFunc m -> m ()
copyData ElfListNull _ _ = pure ()
copyData (ElfListCons (ElfSection {esData = ElfSectionData textData, ..}) xs) zeros f = do
  f (fromIntegral esAddr) $ BSL.append textData (BSL.replicate zeros 0)
  copyData xs zeros f
copyData (ElfListCons _ xs) zeros f = copyData xs zeros f

loadSegment :: (Monad m, SingElfClassI a) => LoadFunc m -> ElfXX 'Segment a -> m ()
loadSegment loadFunc ElfSegment {..} =
  copyData epData (fromIntegral epAddMemSize) loadFunc

loadElf :: (Monad m) => Elf -> LoadFunc m -> m ()
loadElf (Elf classS elfs) loadFunc = withSingElfClassI classS $ do
  let loadable = loadableSegments elfs
  mapM_ (loadSegment loadFunc) loadable

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
    go (ElfListCons v@(ElfSegment {..}) l) = pure $ fromIntegral epVirtAddr
    go (ElfListCons _ l) = go l
    go ElfListNull = pure 0

-- | Read a string from memory starting at the given address for count bytes
readStringFromMemory :: (MonadMemory m) => Unsigned 32 -> Unsigned 32 -> m String
readStringFromMemory addr count = do
  bytes <-
    mapM
      ( \i -> do
          byte <- ramRead (addr + i)
          pure $ fromIntegral (byte .&. 0xFF)
      )
      [0 .. count - 1]
  pure $ map chr $ takeWhile (/= 0) bytes

-- | Called on each step of the ELF execution, returning whether to continue execution.
type Instrument m = Core.Input -> Core.State -> Core.Output -> Int -> m Bool

runElf :: forall m. (MonadMemory m) => Instrument m -> CircuitSim m Core.Input Core.State Core.Output -> m ()
runElf instr c = watchWithStep (0 :: Int) c
  where
    watchWithStep stepCount sim@(CircuitSim i s step next) = do
      (s', o) <- step i s
      mi' <- next o
      cont <- instr i s' o stepCount
      case mi' of
        Just i' | cont -> do
          watchWithStep (stepCount + 1) $ sim {circuitInput = i', circuitState = s'}
        _ -> pure ()