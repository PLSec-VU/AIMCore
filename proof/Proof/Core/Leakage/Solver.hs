-- | The leakage proof, checked symbolically.
--
-- 'baseCaseLeak' plus one property per driver delay: on a hop of that length,
-- the corresponding obligation in "Proof.Core.Leakage.Obligation" holds. Split by
-- @k@ because the number of unrolled cycles has to be concrete -- Pantomime
-- cannot recurse on a symbolic count -- and each property pins @driver == k@ so
-- it carries one unrolling rather than four.
--
-- Together with "Proof.Core.Functional.Solver" these are the leakage theorem: the
-- functional proof establishes that the invariant holds wherever the driver
-- lands, which each step here assumes as a premise.
--
-- Each query runs both the core and the simulator over the hop and compares
-- their per-cycle bus observations, the two drivers' hop lengths and the
-- simulator invariant, so they are larger than the corresponding functional
-- ones. The statements live in "Proof.Core.Leakage.Obligation"; this module only
-- instantiates them.
--
-- The system state is passed as a 'KState' plus SMT-array register file and
-- memory (see "Proof.Core.Functional.System"); the simulator state is passed directly,
-- since it holds no register file or memory.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Core.Leakage.Solver
  ( baseCaseLeakP,
    indStepLeak0P,
    indStepLeak1P,
    indStepLeak2P,
    indStepLeak3P,
    results,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Core.Leakage.Obligation
import Proof.Core.Leakage.Simulator
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Proof.Core.Functional.System
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The base case ----------------------------------------------------------------

-- | The first hop from the initial state, for any start PC, register file and
-- memory.
{-# ANN baseCaseLeakP (Theory arrayAxioms) #-}
baseCaseLeakP :: Address -> RegArr -> MemArr -> Pantomime.Bool
baseCaseLeakP pc ra ma =
  Pantomime.boolean $ baseCaseLeak pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStepLeak0P (Theory arrayAxioms) #-}
indStepLeak0P :: RegIdx -> Address -> Address -> KState -> Core.Input -> RegArr -> MemArr -> SimState -> Pantomime.Bool
indStepLeak0P wr wa ipc ss i ra ma sim =
  Pantomime.boolean $ indStepLeak0 wr wa ipc (sysOf ss i ra ma) sim

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStepLeak1P (Theory arrayAxioms) #-}
indStepLeak1P :: RegIdx -> Address -> Address -> KState -> Core.Input -> RegArr -> MemArr -> SimState -> Pantomime.Bool
indStepLeak1P wr wa ipc ss i ra ma sim =
  Pantomime.boolean $ indStepLeak1 wr wa ipc (sysOf ss i ra ma) sim

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStepLeak2P (Theory arrayAxioms) #-}
indStepLeak2P :: RegIdx -> Address -> Address -> KState -> Core.Input -> RegArr -> MemArr -> SimState -> Pantomime.Bool
indStepLeak2P wr wa ipc ss i ra ma sim =
  Pantomime.boolean $ indStepLeak2 wr wa ipc (sysOf ss i ra ma) sim

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStepLeak3P (Theory arrayAxioms) #-}
indStepLeak3P :: RegIdx -> Address -> Address -> KState -> Core.Input -> RegArr -> MemArr -> SimState -> Pantomime.Bool
indStepLeak3P wr wa ipc ss i ra ma sim =
  Pantomime.boolean $ indStepLeak3 wr wa ipc (sysOf ss i ra ma) sim

results :: [(String, Maybe String)]
results =
  [ ("baseCaseLeakP", $(pantomime 'baseCaseLeakP)),
    ("indStepLeak0P", $(pantomime 'indStepLeak0P)),
    ("indStepLeak1P", $(pantomime 'indStepLeak1P)),
    ("indStepLeak2P", $(pantomime 'indStepLeak2P)),
    ("indStepLeak3P", $(pantomime 'indStepLeak3P))
  ]
