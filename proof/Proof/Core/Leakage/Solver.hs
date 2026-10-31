-- | The leakage proof, checked symbolically.
--
-- 'baseCaseLeakP' plus one property per driver delay: on a hop of that length,
-- the corresponding obligation in "Proof.Core.Leakage.Obligation" holds. Split
-- by @k@ because the number of unrolled cycles has to be concrete -- the plugin
-- cannot recurse on a symbolic count -- and each property pins @driver == k@ so
-- it carries one unrolling rather than four.
--
-- Together with "Proof.Core.Functional.Solver" these are the leakage theorem:
-- the functional proof establishes that the invariant holds wherever the driver
-- lands, which each step here assumes as a premise.
--
-- Each query runs both the core and the simulator over the hop and compares
-- their per-cycle bus observations, the two drivers' hop lengths and the
-- simulator invariant, so they are larger than the corresponding functional
-- ones. The statements live in "Proof.Core.Leakage.Obligation"; this module
-- only instantiates them.
--
-- The system state is a 'KState' plus SMT-array register file and memory (see
-- "Proof.Core.Functional.System"); the simulator state is a fresh value of its
-- own type, since it holds no register file or memory.
module Proof.Core.Leakage.Solver
  ( baseCaseLeakP,
    indStepLeak0P,
    indStepLeak1P,
    indStepLeak2P,
    indStepLeak3P,
  )
where

import Proof.Core.Functional.Machine (SysG)
import Proof.Core.Functional.System
import Proof.Core.Leakage.Obligation
import Proof.Core.Leakage.Simulator (SimState)
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayEmbeddings)
import Proof.SMT.Symbolic (expectValid)
import SymFC (SymFC (..))
import qualified SymFC.BuiltIn as SymFC
import Types
import Prelude hiding (Word)

-- The base case ----------------------------------------------------------------

-- | The first hop from the initial state, for any start PC, register file and
-- memory.
{-# ANN baseCaseLeakP (SymFC ['arrayEmbeddings]) #-}
baseCaseLeakP :: SymFC.Sym ()
baseCaseLeakP = do
  pc <- SymFC.fresh
  ra <- freshRA
  ma <- freshMA
  expectValid $ baseCaseLeak pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------

type HopLeakObligation = RegIdx -> Address -> Address -> SysG RegArrF MemArr -> SimState -> Bool

holdsOnEveryHop :: HopLeakObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  wr <- SymFC.fresh
  wa <- SymFC.fresh
  ipc <- SymFC.fresh
  sys <- freshSys
  sim <- SymFC.fresh
  expectValid $ obligation wr wa ipc sys sim

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStepLeak0P (SymFC ['arrayEmbeddings]) #-}
indStepLeak0P :: SymFC.Sym ()
indStepLeak0P = holdsOnEveryHop indStepLeak0

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStepLeak1P (SymFC ['arrayEmbeddings]) #-}
indStepLeak1P :: SymFC.Sym ()
indStepLeak1P = holdsOnEveryHop indStepLeak1

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStepLeak2P (SymFC ['arrayEmbeddings]) #-}
indStepLeak2P :: SymFC.Sym ()
indStepLeak2P = holdsOnEveryHop indStepLeak2

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStepLeak3P (SymFC ['arrayEmbeddings]) #-}
indStepLeak3P :: SymFC.Sym ()
indStepLeak3P = holdsOnEveryHop indStepLeak3
