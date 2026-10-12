-- | The leakage proof, checked symbolically.
--
-- 'baseCaseLeak' plus one property per driver delay: on a hop of that length,
-- the corresponding obligation in "Proof.Leakage.Obligation" holds. Split by
-- @k@ because the number of unrolled cycles has to be concrete -- Pantomime
-- cannot recurse on a symbolic count -- and each property pins @driver == k@ so
-- it carries one unrolling rather than four.
--
-- Together with "Proof.Functional.Induction" these are the leakage theorem: the
-- functional proof establishes that the invariant holds wherever the driver
-- lands, which each step here assumes as a premise.
--
-- Each query runs both the core and the simulator over the hop and compares
-- their per-cycle bus observations, the two drivers' hop lengths and the
-- simulator invariant, so they are larger than the corresponding functional
-- ones. The statements live in "Proof.Leakage.Obligation"; this module only
-- instantiates them.
--
-- The system state is passed as a 'KState' plus SMT-array register file and
-- memory (see "Proof.SMT.System"); the simulator state is passed directly,
-- since it holds no register file or memory.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Leakage.Induction
  ( baseCaseLeak,
    indStepLeak,
    indStepLeak1,
    indStepLeak2,
    indStepLeak3,
    results,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Leakage.Obligation
import Proof.Leakage.Simulator
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Proof.SMT.System
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The base case ----------------------------------------------------------------

-- | The first hop from the initial state, for any start PC, register file and
-- memory.
{-# ANN baseCaseLeak (Theory arrayAxioms) #-}
baseCaseLeak :: RegArr -> MemArr -> Address -> Pantomime.Bool
baseCaseLeak ra ma pc =
  Pantomime.boolean $ baseCaseLeakObligation (RegArrF ra) ma pc

-- The inductive steps ----------------------------------------------------------

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStepLeak (Theory arrayAxioms) #-}
indStepLeak :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> SimState -> Pantomime.Bool
indStepLeak ss i ra ma wr wa ipc sim =
  Pantomime.boolean $ indStepLeakObligation wr wa ipc (sysOf ss i ra ma) sim

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStepLeak1 (Theory arrayAxioms) #-}
indStepLeak1 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> SimState -> Pantomime.Bool
indStepLeak1 ss i ra ma wr wa ipc sim =
  Pantomime.boolean $ indStepLeakObligation1 wr wa ipc (sysOf ss i ra ma) sim

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStepLeak2 (Theory arrayAxioms) #-}
indStepLeak2 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> SimState -> Pantomime.Bool
indStepLeak2 ss i ra ma wr wa ipc sim =
  Pantomime.boolean $ indStepLeakObligation2 wr wa ipc (sysOf ss i ra ma) sim

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStepLeak3 (Theory arrayAxioms) #-}
indStepLeak3 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Address -> SimState -> Pantomime.Bool
indStepLeak3 ss i ra ma wr wa ipc sim =
  Pantomime.boolean $ indStepLeakObligation3 wr wa ipc (sysOf ss i ra ma) sim

results :: [(String, Maybe String)]
results =
  [ ("baseCaseLeak", $(pantomime 'baseCaseLeak)),
    ("indStepLeak", $(pantomime 'indStepLeak)),
    ("indStepLeak1", $(pantomime 'indStepLeak1)),
    ("indStepLeak2", $(pantomime 'indStepLeak2)),
    ("indStepLeak3", $(pantomime 'indStepLeak3))
  ]
