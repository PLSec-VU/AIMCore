-- | The leakage proof, checked symbolically.
--
-- 'baseCaseLeak' plus one property per driver delay: on a hop of that length,
-- the corresponding obligation in "Proof.Leakage.Obligation" holds. Split by
-- @k@ because the number of unrolled cycles has to be concrete -- the plugin
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
-- The system state is a 'KState' plus SMT-array register file and memory (see
-- "Proof.SMT.System"); the simulator state is a fresh value of its own type,
-- since it holds no register file or memory.
module Proof.Leakage.Induction
  ( baseCaseLeak,
    indStepLeak,
    indStepLeak1,
    indStepLeak2,
    indStepLeak3,
  )
where

import Proof.Leakage.Obligation
import Proof.Leakage.Simulator (SimState)
import Proof.Machine (SysG)
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayEmbeddings)
import Proof.SMT.Symbolic (expectValid)
import Proof.SMT.System
import SymFC (SymFC (..))
import qualified SymFC.BuiltIn as SymFC
import Types
import Prelude hiding (Word)

-- The base case ----------------------------------------------------------------

-- | The first hop from the initial state, for any start PC, register file and
-- memory.
{-# ANN baseCaseLeak (SymFC ['arrayEmbeddings]) #-}
baseCaseLeak :: SymFC.Sym ()
baseCaseLeak = do
  ra <- freshRA
  ma <- freshMA
  pc <- SymFC.fresh
  expectValid $ baseCaseLeakObligation (RegArrF ra) ma pc

-- The inductive steps ----------------------------------------------------------

type HopLeakObligation = RegIdx -> Address -> Address -> SysG RegArrF MemArr -> SimState -> Bool

holdsOnEveryHop :: HopLeakObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  sys <- freshSys
  sim <- SymFC.fresh
  wr <- SymFC.fresh
  wa <- SymFC.fresh
  ipc <- SymFC.fresh
  expectValid $ obligation wr wa ipc sys sim

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStepLeak (SymFC ['arrayEmbeddings]) #-}
indStepLeak :: SymFC.Sym ()
indStepLeak = holdsOnEveryHop indStepLeakObligation

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStepLeak1 (SymFC ['arrayEmbeddings]) #-}
indStepLeak1 :: SymFC.Sym ()
indStepLeak1 = holdsOnEveryHop indStepLeakObligation1

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStepLeak2 (SymFC ['arrayEmbeddings]) #-}
indStepLeak2 :: SymFC.Sym ()
indStepLeak2 = holdsOnEveryHop indStepLeakObligation2

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStepLeak3 (SymFC ['arrayEmbeddings]) #-}
indStepLeak3 :: SymFC.Sym ()
indStepLeak3 = holdsOnEveryHop indStepLeakObligation3
