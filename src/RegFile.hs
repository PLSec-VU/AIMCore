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
import qualified Prelude as P
import Numeric (showHex)
import Data.List (intercalate)

-- | Register file used in this core.
newtype RegFile = RegFile (Vec ((2 ^ 5) - 1) Word)
  deriving (Eq, Generic, NFDataX)

regIdxName :: RegIdx -> String
regIdxName idx =
  ["zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"]
    P.!! (fromIntegral idx)

instance Show RegFile where
  show (RegFile rf) = intercalate ", " $
    Prelude.zipWith (\i x -> "%" <> regIdxName i <> "=0x" <> x)
      [1..] (flip showHex "" <$> toList rf)

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
