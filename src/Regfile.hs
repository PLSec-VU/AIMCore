module Regfile
  ( Regfile
  , initRF
  , lookupRF
  , modifyRF
  ) where

import Prelude hiding (Word, (!!), (++), repeat, undefined)
import Clash.Prelude hiding (Word, zip)
import Types
import Pretty

-- | Register file used in this core.
newtype Regfile = Regfile (Vec ((2 ^ 5) - 1) Word)
  deriving Show

instance Pretty Regfile where
  pretty (Regfile rf) = vcat $ uncurry line <$> zip [0 :: Int ..] rf'
    where
      line idx word = "%r" <> pretty idx <+> "=" <+> pretty word
      rf' = toList $ singleton 0 ++ rf

initRF :: Regfile
initRF = Regfile $ repeat 0

-- | Lookup a register from the register file.
lookupRF :: RegIdx -> Regfile -> Word
lookupRF idx (Regfile rf) = (singleton 0 ++ rf) !! idx

-- | Modify a register in the register file.
modifyRF :: RegIdx -> Word -> Regfile -> Regfile
modifyRF idx val (Regfile rf) = case idx of
  0 -> Regfile rf
  _ -> Regfile $ replace (idx - 1) val rf
