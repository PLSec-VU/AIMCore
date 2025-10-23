{-# LANGUAGE TupleSections #-}

module HardwareSim (topEntity) where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Core hiding (topEntity)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified GHC.TypeNats
import Instruction hiding (decode)
import RegFile
import Types
import Util (PROG_SIZE, MemSizeFrom, RAM_SIZE)
import qualified Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, take, undefined, (!!), (&&), (++), (||))

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Output
topEntity = exposeClockResetEnable $ system prog3

cpu :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
cpu = mealy circuit init

system :: forall dom. (HiddenClockResetEnable dom) => Vec PROG_SIZE Word -> Signal dom Output
system prog = cpuOut
  where
    cpuInput :: Signal dom Input
    cpuInput = register initInput input
    cpuOut = cpu cpuInput
    regfile = mkRegFile $ mkRegAccess <$> cpuOut
    ram = mkRAM @PROG_SIZE @RAM_SIZE prog ((fromMaybe (MemAccess False 0 Word Nothing) . getFirst . outMem) <$> cpuOut)
    input =
      ( \o mread (rs1, rs2) ->
          Input
            ( fromMaybe False $ memIsInstr <$> getFirst (outMem o)
            )
            mread
            rs1
            rs2
      )
        <$> cpuOut
        <*> ram
        <*> regfile
    mkRegAccess (Output _ mr1 mr2 mrd _) =
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

mkRegFile :: (HiddenClockResetEnable dom) => Signal dom RegAccess -> Signal dom (Word, Word)
mkRegFile input = mkOutput <$> reg_update <*> input
  where
    reg_output = register initRF reg_update
    reg_update = ((uncurry modifyRF . regRd) <$> input) <*> reg_output
    mkOutput rf (RegAccess rs1 rs2 _) = (lookupRF rs1 rf, lookupRF rs2 rf)

mkRAM :: forall progSize ramSize dom. 
  (HiddenClockResetEnable dom, KnownNat ((GHC.TypeNats.*) ramSize 4), KnownNat (progSize + ramSize), KnownNat ((GHC.TypeNats.*) (progSize + ramSize) 4)) =>
  Vec progSize Word ->
  Signal dom MemAccess ->
  Signal dom Word
mkRAM prog memAccessM =
  combine <$> ram0 <*> ram1 <*> ram2 <*> ram3
  where
    addr = memAddress <$> memAccessM

    mkWrite n =
      fmap $ \memAccess -> do
        w <- memVal memAccess
        pure (memAddress memAccess, extractByte n w)

    extractByte 0 w = slice d7 d0 w
    extractByte 1 w = slice d15 d8 w
    extractByte 2 w = slice d23 d16 w
    extractByte 3 w = slice d31 d24 w
    extractByte _ _ = error "Invalid byte index"

    vecRam :: Vec (((GHC.TypeNats.*) (progSize + ramSize) 4)) Byte
    vecRam = Util.mkRAM @progSize @((GHC.TypeNats.*) ramSize 4) prog

    every4th ::
      forall n offset a.
      (KnownNat n, KnownNat ((GHC.TypeNats.*) n 4)) =>
      SNat offset ->
      Vec ((GHC.TypeNats.*) n 4) a ->
      Vec n a
    every4th offset v =
      imap (\(i :: Index n) _ -> v !! idx i) (repeat undefined)
      where
        off :: Int
        off = fromIntegral (snatToNum offset)

        idx :: Index n -> Index ((GHC.TypeNats.*) n 4)
        idx i = fromInteger (toInteger off + toInteger i * 4)

    ram0 = blockRam (every4th d0 vecRam) addr (mkWrite 0 memAccessM)
    ram1 = blockRam (every4th d1 vecRam) addr (mkWrite 1 memAccessM)
    ram2 = blockRam (every4th d2 vecRam) addr (mkWrite 2 memAccessM)
    ram3 = blockRam (every4th d3 vecRam) addr (mkWrite 3 memAccessM)

    combine :: Byte -> Byte -> Byte -> Byte -> Word
    combine b0 b1 b2 b3 =
      b3 ++# b2 ++# b1 ++# b0

prog3 :: Vec PROG_SIZE Word
prog3 =
  Util.mkProg $
    ( -- r2 := r0 + 3
      IType (Arith ADD) 2 0 3
        :>
        -- r3 := r0 + r2
        RType ADD 3 0 2
        :>
        -- r2 == r3 ? jump pc + 8
        BType EQ 8 2 3
        :>
        -- mem[0 + r0] := r2
        SType Word 0 0 2
        :>
        -- mem[1 + r0] := r2
        SType Word 4 0 2
        :> Instruction.break
        :> Nil
    )
