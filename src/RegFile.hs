{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module RegFile
  ( RegFile (..),
    initRF,
    lookupRF,
    modifyRF,
    censorRF,
    RegFileOps (..),
    RegFn (..),
  )
where

import Access
import Clash.Prelude hiding (Word, zip)
import Pretty
import Types
import Prelude hiding (Ordering (..), Word, repeat, undefined, (!!), (++))
import qualified Prelude as P
import Numeric (showHex)
import Data.List (intercalate)

-- | Register file used in this core.
newtype RegFile f = RegFile (Vec ((2 ^ 5) - 1) (f Word))

deriving instance (Generic (f Word), NFDataX (f Word)) => Generic (RegFile f)
deriving instance (Generic (f Word), NFDataX (f Word)) => NFDataX (RegFile f)
deriving instance (Eq (f Word)) => Eq (RegFile f)

regIdxName :: RegIdx -> String
regIdxName idx =
  ["zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"]
    P.!! (fromIntegral idx)

instance (Show (f Word)) => Show (RegFile f) where
  show (RegFile rf) = intercalate ", " $
    Prelude.zipWith (\i x -> "%" <> regIdxName i <> "=" <> show x)
      [1..] (toList rf)

instance (Access f, Pretty (f Word)) => Pretty (RegFile f) where
  pretty (RegFile rf) = vcat $ uncurry line <$> P.zip [0 :: Int ..] rf'
    where
      line idx word = "%r" <> pretty idx <+> "=" <+> pretty word
      rf' = toList $ singleton (pure 0) ++ rf

initRF :: (Access f) => RegFile f
initRF = RegFile $ repeat (pure 0)

-- | Lookup a register from the register file.
lookupRF :: (Access f) => RegIdx -> RegFile f -> f Word
lookupRF 0 _ = pure 0
lookupRF idx (RegFile rf) = rf !! (idx - 1)

-- | Modify a register in the register file.
modifyRF :: (Access f) => RegIdx -> f Word -> RegFile f -> RegFile f
modifyRF idx val (RegFile rf) = case idx of
  0 -> RegFile rf
  _ -> RegFile $ replace (idx - 1) val rf

-- | Censor all registers in the register file.
censorRF :: (Access f) => RegFile f -> RegFile f
censorRF _ = initRF

-- | The operations the pipeline needs from a register file.
--
-- 'Core.StateG' is parameterised over this so that the same pipeline can run on
-- two representations: the synthesisable 'RegFile' (a 'Vec'), and 'RegFn' (a
-- function) which is what symbolic execution can actually handle. See
-- "Induction" for why the 'Vec' one cannot be symbolically executed.
class RegFileOps r where
  lookupRFg :: (Access f) => RegIdx -> r f -> f Word
  modifyRFg :: (Access f) => RegIdx -> f Word -> r f -> r f
  initRFg :: (Access f) => r f

instance RegFileOps RegFile where
  lookupRFg = lookupRF
  modifyRFg = modifyRF
  initRFg = initRF

-- | Verification-only register file: a function rather than a container.
--
-- Not synthesisable -- Clash cannot turn a function into hardware -- so this
-- must never reach 'Core.topEntity'. It exists purely so that reads become
-- applications and writes become lambdas, both of which the symbolic executor
-- handles natively.
newtype RegFn f = RegFn (RegIdx -> f Word)

instance RegFileOps RegFn where
  lookupRFg idx (RegFn g) = if idx == 0 then pure 0 else g idx
  modifyRFg idx v rf@(RegFn g) =
    if idx == 0 then rf else RegFn (\j -> if j == idx then v else g j)
  initRFg = RegFn (const (pure 0))
