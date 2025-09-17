module RegFile
  ( RegFile,
    initRF,
    lookupRF,
    modifyRF,
    reserveRF,
    releaseRF,
    RegEntry (..),
    StationId,
  )
where

import Clash.Prelude hiding (Word, zip)
import Pretty
import Types
import Prelude hiding (Word, repeat, undefined, (!!), (++))

type StationId = BitVector 4  -- Support up to 16 reservation stations

-- | An entry in the register file.
data RegEntry
  = Ready Word           -- Register has actual value
  | Pending StationId    -- Register waiting for result from this station
  deriving (Eq, Show, Generic, NFDataX)

instance Pretty RegEntry where
  pretty (Ready w) = pretty w
  pretty (Pending s) = "P" <> pretty s

-- | Register file used in this core.
newtype RegFile = RegFile (Vec ((2 ^ 5) - 1) RegEntry)
  deriving (Eq, Show, Generic, NFDataX)

instance Pretty RegFile where
  pretty (RegFile rf) = vcat $ uncurry line <$> zip [0 :: Int ..] rf'
    where
      line idx word = "%r" <> pretty idx <+> "=" <+> pretty word
      rf' = toList $ singleton (Ready 0) ++ rf

initRF :: RegFile
initRF = RegFile $ repeat (Ready 0)

-- | Lookup a register from the register file.
lookupRF :: RegIdx -> RegFile -> RegEntry
lookupRF idx (RegFile rf) = (singleton (Ready 0) ++ rf) !! idx

-- | Reserve a register for a reservation station
reserveRF :: RegIdx -> StationId -> RegFile -> RegFile
reserveRF idx stationId (RegFile rf) = case idx of
  0 -> RegFile rf  -- Can't reserve r0
  _ -> RegFile $ replace (idx - 1) (Pending stationId) rf

-- | Release a register reservation and set its value
releaseRF :: RegIdx -> Word -> RegFile -> RegFile
releaseRF = modifyRF

-- | Modify a register in the register file.
modifyRF :: RegIdx -> Word -> RegFile -> RegFile
modifyRF idx val (RegFile rf) = case idx of
  0 -> RegFile rf
  _ -> RegFile $ replace (idx - 1) (Ready val) rf

