module Leak.PC.ISA (leak) where

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

leak :: Vec PROG_SIZE Word -> [Leak.Out]
leak = Prelude.map (\(_, o, _) -> o) . runSimulator watch

simulator ::
  forall m.
  ( MonadState ((Core.State, Core.Output), Simulate.Mem MEM_SIZE_BYTES) m
  ) =>
  CircuitSim m Input Leak.State Leak.Out
simulator =
  CircuitSim
    { circuitInput = Core.initInput,
      circuitState = Leak.init,
      circuitStep = step,
      circuitNext = next
    }
  where
    step :: Input -> Leak.State -> m (Leak.State, Leak.Out)
    step i s = do
      ((s_sim, _), mem) <- get
      let (res_sim@(_, o_sim), mem') = runState (circuitStep Simulate.simulator i s_sim) mem
      put (res_sim, mem')
      let (s', leakInstr) = Leak.circuit s i
      pure (s', leakInstr)

    next :: Leak.Out -> m (Maybe Input)
    next _out = do
      ((_, o_sim), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator o_sim) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  ( CircuitSim
      (State ((Core.State, Core.Output), Simulate.Mem MEM_SIZE_BYTES))
      Input
      Leak.State
      Leak.Out ->
    State ((Core.State, Core.Output), Simulate.Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f prog = evalState (f Leak.PC.ISA.simulator) s
  where
    s = ((Core.init, mempty), Simulate.Mem (mkRAM prog) initRF)

prog1 :: Vec PROG_SIZE Word
prog1 =
  mkProg $
    -- r2 := r0 + 5
    Core.IType (Core.Arith Core.ADD) 2 0 5
      :>
      -- mem[0 + r0] := r2
      Core.SType Word 0 0 2
      :> Core.break
      :> Nil
