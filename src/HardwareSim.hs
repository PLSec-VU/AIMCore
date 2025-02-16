{-# LANGUAGE TupleSections #-}

module HardwareSim where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Debug.Trace as DB
import Instruction hiding (decode)
import Pipe
import Regfile
import qualified Simulate
import Text.Read (readMaybe)
import Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))
import qualified Prelude

simN :: (KnownNat m) => Vec m Word -> Int -> [Output]
simN prog n = sampleN @System n $ system prog

cpu :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
cpu = mealy pipe initPipe

system :: (HiddenClockResetEnable dom) => Vec m Word -> Signal dom Output
system prog = out
  where
    out = cpu input
    regfile = mkRegfile $ mkRegAccess <$> out
    ram = mkRAM prog (mkMemRead <$> out) (mkMemWrite <$> out)
    input = (\mread (rs1, rs2) -> Input mread rs1 rs2) <$> ram <*> regfile
    mkMemRead = maybe 0 memAddress . getFirst . outMem
    mkMemWrite = ((\ma -> (memAddress ma,) <$> memVal ma) =<<) . getFirst . outMem
    mkRegAccess (Output _ mr1 mr2 mrd) =
      RegAccess
        { regRs1 = fromMaybe 0 $ getFirst mr1,
          regRs2 = fromMaybe 0 $ getFirst mr2,
          regRd = fromMaybe (0, 0) $ getFirst mrd
        }

data RegAccess = RegAccess
  { regRs1 :: RegIdx,
    regRs2 :: RegIdx,
    regRd :: (RegIdx, Word)
  }
  deriving (Show, Generic, NFDataX)

mkRegfile :: (HiddenClockResetEnable dom) => Signal dom RegAccess -> Signal dom (Word, Word)
mkRegfile input = mkOutput <$> reg_output <*> input
  where
    reg_output = register initRF reg_update
    reg_update = ((uncurry modifyRF . regRd) <$> input) <*> reg_output
    mkOutput rf (RegAccess rs1 rs2 _) = (lookupRF rs1 rf, lookupRF rs2 rf)

mkRAM ::
  (HiddenClockResetEnable dom) =>
  Vec m Word ->
  Signal dom Address ->
  Signal dom (Maybe (Address, Word)) ->
  Signal dom Word
mkRAM = blockRam . Simulate.mkRAM

prog1 =
  map encode $
    -- r2 := r0 + 5
    IType (Arith ADD) 2 0 5
      :>
      -- mem[0 + r0] := r2
      SType Word 0 0 2
      :> halt
      :> Nil
