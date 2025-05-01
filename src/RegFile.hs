module RegFile
  ( RegFile,
    initRF,
    lookupRF,
    modifyRF,
  )
where

import Clash.Prelude hiding (Word, zip)
import Pretty
import Types
import Prelude hiding (Word, repeat, undefined, (!!), (++))

-- | Register file used in this core.
newtype RegFile = RegFile (Vec ((2 ^ 5) - 1) Word)
  deriving (Eq, Show, Generic, NFDataX)

instance Pretty RegFile where
  pretty (RegFile rf) = vcat $ uncurry line <$> zip [0 :: Int ..] rf'
    where
      line idx word = "%r" <> pretty idx <+> "=" <+> pretty word
      rf' = toList $ singleton 0 ++ rf

initRF :: RegFile
initRF = RegFile $ repeat 0

-- | Lookup a register from the register file.
lookupRF :: RegIdx -> RegFile -> Word
lookupRF idx (RegFile rf) = (singleton 0 ++ rf) !! idx

-- | Modify a register in the register file.
modifyRF :: RegIdx -> Word -> RegFile -> RegFile
modifyRF idx val (RegFile rf) = case idx of
  0 -> RegFile rf
  _ -> RegFile $ replace (idx - 1) val rf
