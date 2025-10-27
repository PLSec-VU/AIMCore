module Leak.PC.ISA (leak) where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Core (Input)
import qualified Core
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import qualified Instruction as Core
import Interp
import qualified Leak.PC.Leak as Leak
import RegFile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

leak :: (Access f) => Vec PROG_SIZE (f Word) -> [Leak.Out]
leak = Prelude.map (\(_, o, _) -> o) . runSimulator watch

simulator ::
  forall m f.
  ( Access f,
    MonadState ((Core.State f, Core.Output f), Simulate.Mem f MEM_SIZE_BYTES) m
  ) =>
  CircuitSim m (Input f) (Leak.State f) Leak.Out
simulator =
  CircuitSim
    { circuitInput = Core.initInput,
      circuitState = Leak.init,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: Input f -> Leak.State f -> m (Leak.State f, Leak.Out)
    step i s = do
      ((s_sim, _), mem) <- get
      let (res_sim@(_, o_sim), mem') = runState (circuitStep Simulate.simulator i s_sim) mem
      put (res_sim, mem')
      let (s', leakInstr) = Leak.circuit s i
      pure (s', leakInstr)

    next :: Leak.Out -> m (Maybe (Input f))
    next _out = do
      ((_, o_sim), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_sim) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  (Access f) =>
  ( CircuitSim
      (State ((Core.State f, Core.Output f), Simulate.Mem f MEM_SIZE_BYTES))
      (Input f)
      (Leak.State f)
      Leak.Out ->
    State ((Core.State f, Core.Output f), Simulate.Mem f MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE (f Word) ->
  a
runSimulator f prog = evalState (f Leak.PC.ISA.simulator) s
  where
    s = ((Core.init, mempty), Simulate.Mem (mkRAM prog) initRF)
