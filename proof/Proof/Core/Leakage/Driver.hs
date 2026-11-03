-- | The simulator's driver: how many cycles the simulator takes for an
-- instruction, computed from its own state and the instruction's leakage. The
-- counterpart of 'Proof.Core.Functional.Driver.driver', case for case; the
-- leakage proof's first step is that the two agree.
module Proof.Core.Leakage.Driver
  ( simDriver,
    simDriverSpec,
    simDriverMatchesSpecInit,
    simDriverMatchesSpec,
    simIsBubble,
    simStoreHazard,
    simLoadHazard
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core.Instruction
import Core.RegFile (RegFileOps)
import Memory.Types (MemOps)
import Proof.Core.Functional.Invariant (sysInvAt)
import Proof.Core.Functional.Machine (SysG)
import Proof.Core.Leakage.Invariant (simInv)
import Proof.Core.Leakage.Model
import Proof.Core.Leakage.Simulator
import Types (Address, RegIdx)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The store-hazard condition of 'Core.Pipeline.decode': the instruction word
-- at the decode-stage PC overlaps a store either in the execute stage or in the
-- memory stage.
simStoreHazard :: SimInput -> SimState -> Bool
simStoreHazard inp st =
  maybe False (Core.Instruction.storeHazard dePc) exStore
    || maybe False (Core.Instruction.storeHazard dePc) meStore
  where
    dePc = simStateDePc st
    exStore = case inp of
      Just (LStore size addr) -> Just (addr, size)
      _ -> Nothing
    meStore = case simStateMeInstr st of
      LStore size addr -> Just (addr, size)
      _ -> Nothing

-- | The load-hazard condition of 'Core.Pipeline.decode', computed by the ISA as
-- part of the leakage.
simLoadHazard :: SimInput -> SimState -> Bool
simLoadHazard (Just (LLoad _ _ haz)) st = simStateDeExpInstr st && haz
simLoadHazard _ _ = False

simDriver :: SimInput -> SimState -> Int
simDriver inp st
  | simStateExInstrType st == FirstCycle = 1
  | simStateExInstrType st == Halted = 0
  | maybe False isEnvLeak inp = 2
  | maybe False isJumpLeak inp = 2
  | simStoreHazard inp st = if maybe False isMemLeak inp then 3 else 2
  | simLoadHazard inp st = 3
  | simStateDeExpInstr st = 0
  | not (isMemLeak (simStateMeInstr st)) = 1
  | not (maybe False isMemLeak inp) = 2
  | otherwise = 3

-- | Is the simulator's execute stage a pipeline bubble, as opposed to an
-- instruction? The counterpart of 'Proof.Core.Functional.Driver.isBubble': a
-- decoded instruction is not, and neither is a halted simulator; the stall
-- reasons are.
simIsBubble :: SimState -> Bool
simIsBubble sim = case simStateExInstrType sim of
  DecodedInstr -> False
  Halted -> False
  _ -> True

-- | The operational reading of 'simDriver', as
-- 'Proof.Core.Functional.Driver.driverSpec' is of the core's: step on the
-- instruction's leakage, then with no input until the execute stage is no
-- longer a bubble ('simIsBubble'), and count the steps after the first.
-- 'Nothing' if @fuel@ ran out.
simDriverSpec :: Int -> SimInput -> SimState -> Maybe Int
simDriverSpec fuel inp sim0 = go 0 (fst (simCircuit sim0 inp))
  where
    go n sim
      | not (simIsBubble sim) = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (fst (simCircuit sim Nothing))

-- | On the initial simulator its driver agrees with its operational reading:
-- the first hop, from the start PC @pc@, which starts with no input
-- ('simInit'). The invariant does not cover the initial simulator, so
-- 'simDriverMatchesSpec' does not either.
simDriverMatchesSpecInit :: Address -> Bool
simDriverMatchesSpecInit pc =
  simDriverSpec 3 Nothing sim == Just (simDriver Nothing sim)
  where
    sim = simInit {simStateFePc = pc}

-- | On every other hop the simulator's driver agrees with its operational
-- reading as well, as 'Proof.Core.Functional.Driver.driverMatchesSpec' for the
-- core: the simulator is related to a system ('simInv') that the core's
-- invariant covers ('Proof.Core.Functional.Invariant.sysInvAt'), and the input
-- is the leakage of any instruction -- every hop but the first starts with one.
-- The core's invariant is needed for the
-- pipeline's shape: without it, the fetch and decode PCs, which 'simInv' copies
-- from the core, are not related, and a store hazard between the input's store
-- and the fetch PC would go unseen by the driver. Three cycles of fuel cover
-- the longest hop.
simDriverMatchesSpec ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> Leak -> SimState -> SysG r m -> Bool
simDriverMatchesSpec wr wa leak sim sys =
  not premises || conclusion
  where
    premises = sysInvAt wr wa sys && simInv sim sys

    conclusion =
      simDriverSpec 3 (Just leak) sim == Just (simDriver (Just leak) sim)
