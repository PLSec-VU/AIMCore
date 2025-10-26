{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module RegFile
  ( RegFile,
    initRF,
    lookupRF,
    modifyRF,
  )
where

import Access
import Clash.Prelude hiding (Word, zip)
import Data.List (intercalate)
import Numeric (showHex)
import Pretty
import Types
import Prelude hiding (Word, repeat, undefined, (!!), (++))

-- | Register file used in this core.
newtype RegFile f = RegFile (Vec ((2 ^ 5) - 1) (f Word))

deriving instance (Eq (f Word)) => Eq (RegFile f)

deriving instance (Generic (f Word)) => Generic (RegFile f)

deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (RegFile f)

instance (Access f, Show (f String)) => Show (RegFile f) where
  show (RegFile rf) =
    intercalate ", " $
      Prelude.zipWith
        (\i x -> "%r" <> show @Int i <> "=" <> x)
        [0 ..]
        ((show . fmap (flip showHex "")) <$> toList rf)

instance (Access f, Pretty (f Word)) => Pretty (RegFile f) where
  pretty (RegFile rf) = vcat $ uncurry line <$> zip [0 :: Int ..] rf'
    where
      line idx word = "%r" <> pretty idx <+> "=" <+> pretty word
      rf' = toList $ singleton (pure 0) ++ rf

initRF :: (Access f) => RegFile f
initRF = RegFile $ repeat $ pure 0

-- | Lookup a register from the register file.
lookupRF :: (Access f) => RegIdx -> RegFile f -> (f Word)
lookupRF idx (RegFile rf) = (singleton (pure 0) ++ rf) !! idx

-- | Modify a register in the register file.
modifyRF :: RegIdx -> f Word -> RegFile f -> RegFile f
modifyRF idx val (RegFile rf) = case idx of
  0 -> RegFile rf
  _ -> RegFile $ replace (idx - 1) val rf
