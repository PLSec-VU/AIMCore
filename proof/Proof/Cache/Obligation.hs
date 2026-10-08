-- | What lets results about the core behind ideal memory carry over to the core
-- behind a cache.
--
-- Behind ideal memory ('Proof.Machine.stepSys') every request is answered in the
-- next cycle. Behind a cache ('Proof.Machine.stepCachedOut') a read that misses
-- is answered only after the miss penalty, and in the meantime the bus is not
-- ready and the core stalls. 'stallObligation' says such a stall is a stutter
-- step, so the cached run is the ideal run with stall cycles inserted, provided
-- the cache answers every read with the data ideal memory would give.
module Proof.Cache.Obligation
  ( stallObligation,
    waitObligation,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Data.Maybe (isNothing)
import Data.Monoid (getFirst)
import Instruction (isLoad)
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | A stall cycle is a stutter step.
--
-- While a load waits in writeback for a bus that is not ready, a cycle makes no
-- request and changes nothing in the core but its control lines, which
-- 'Core.withCtrlReset' overwrites before any stage reads them. The register file
-- is compared at one witness register, as in
-- 'Proof.Functional.Invariant.invAtFree'.
stallObligation :: (RegFileOps r) => RegIdx -> StateG r Identity -> Input Identity -> Bool
stallObligation wr st inp = not stalled || (noRequest && unchanged)
  where
    stalled = stateLoadInFlight st && isLoad (stateWbInstr st) && not (inputMemReady inp)
    (st', o) = circuit st inp
    noRequest = isNothing (getFirst (outMem o))
    unchanged =
      stateFePc st' == stateFePc st
        && stateDePc st' == stateDePc st
        && stateExPc st' == stateExPc st
        && stateExInstr st' == stateExInstr st
        && stateMeInstr st' == stateMeInstr st
        && runIdentity (stateMeRes st') == runIdentity (stateMeRes st)
        && stateMeAddr st' == stateMeAddr st
        && stateWbInstr st' == stateWbInstr st
        && runIdentity (stateWbRes st') == runIdentity (stateWbRes st)
        && stateHalt st' == stateHalt st
        && stateHaltNextPc st' == stateHaltNextPc st
        && stateLoadInFlight st' == stateLoadInFlight st
        && runIdentity (lookupRFg wr (stateRegFile st')) == runIdentity (lookupRFg wr (stateRegFile st))

-- | After a data read, the core waits: the second half of patience.
--
-- A data read is issued by a load in the memory stage, which moves to writeback
-- with a load in flight in the same cycle, since writeback runs before the
-- memory stage. There 'stallObligation' holds the core still until the response
-- is ready. The cache answers late only to data reads
-- ('Proof.Cache.Server.stallsOnReads'), so the core waits after every request the
-- cache can stall on.
waitObligation :: (RegFileOps r) => StateG r Identity -> Input Identity -> Bool
waitObligation st inp = case getFirst (outMem o) of
  Just (MemAccess False _ _ Nothing) -> stateLoadInFlight st' && isLoad (stateWbInstr st')
  _ -> True
  where
    (st', o) = circuit st inp

