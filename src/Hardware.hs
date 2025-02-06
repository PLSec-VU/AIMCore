module Hardware where

import Clash.Prelude hiding (Ordering (..), Word, def, init)
import Clash.Prelude.BlockRam
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import qualified Debug.Trace as DB
import Instruction hiding (decode)
import Pipe
import Regfile
import Types
import Prelude hiding (Ordering (..), Word, init, map, not, repeat, replicate, undefined, (!!), (&&), (++), (||))

simulate :: (KnownNat m) => Vec m Word -> Int -> [Output]
simulate prog n = sampleN @System n $ system prog

cpu :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
cpu = mealy pipe initPipe
  where
    initPipe =
      Pipe
        { rf = initRF,
          fePc = 25,
          dePc = 0,
          exPc = 0,
          exIr = nop,
          meIr = nop,
          meRe = 0,
          wbIr = nop,
          wbRe = 0
        }

system :: (KnownNat m, HiddenClockResetEnable dom) => Vec m Word -> Signal dom Output
system prog = out
  where
    memOut = mkRAM d10 prog outAddr outW
    out = cpu (Input <$> memOut)
    outAddr = (fromMaybe 0 . getLast . outAddress) <$> out
    outW =
      ( \(Output a w) ->
          (,) <$> getLast a <*> getLast w
      )
        <$> out

mkRAM ::
  (KnownNat n, KnownNat m, HiddenClockResetEnable dom) =>
  SNat n ->
  Vec m Word ->
  Signal dom Address ->
  Signal dom (Maybe (Address, Word)) ->
  Signal dom Word
mkRAM size prog =
  blockRam $ replicate size 0 ++ prog ++ replicate size 0

prog1 :: Vec 2 Word
prog1 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2
      ]
