-- | The simulator's driver: how many cycles the simulator takes for an
-- instruction, computed from its own state and the instruction's leakage. The
-- counterpart of 'Proof.Functional.Driver.driver', case for case; the leakage
-- proof's first step is that the two agree.
module Proof.Leakage.Driver
  ( simDriver,
    simStoreHazard,
    simLoadHazard
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Instruction
import Proof.Leakage.Model
import Proof.Leakage.Simulator
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The store-hazard condition of 'Core.decode': the instruction word at the
-- decode-stage PC overlaps a store either in the execute stage or in the memory
-- stage.
simStoreHazard :: SimInput -> SimState -> Bool
simStoreHazard inp st =
  maybe False (Instruction.storeHazard dePc) exStore
    || maybe False (Instruction.storeHazard dePc) meStore
  where
    dePc = simStateDePc st
    exStore = case inp of
      Just (LStore size addr) -> Just (addr, size)
      _ -> Nothing
    meStore = case simStateMeInstr st of
      LStore size addr -> Just (addr, size)
      _ -> Nothing

-- | The load-hazard condition of 'Core.decode', computed by the ISA as part of
-- the leakage.
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
