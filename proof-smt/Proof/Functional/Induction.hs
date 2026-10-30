-- | The inductive steps of the refinement proof, checked symbolically.
--
-- 'baseCase' says the invariant holds once the reset state has taken its first
-- hop. Then one property per driver
-- delay: if the invariant relates @(isa, sys)@ and the driver says the hop takes
-- @k + 1@ cycles, then after those cycles (and one ISA step, where the hop
-- retires an instruction) the invariant relates them again. Base case plus the
-- four steps is the whole refinement theorem.
--
-- Checking one @k@ at a time keeps the number of unrolled cycles concrete,
-- which sidesteps the plugin's termination check: @stepSysN (driver sys + 1)@
-- would recurse on a symbolic count.
--
-- Each property is checked by the plugin at compile time, and an invalid one
-- fails the build. The statements themselves live in
-- "Proof.Functional.Obligation", shared with the QuickCheck harness so the two
-- cannot drift.
--
-- The pipeline state is a 'KState' plus SMT-array register file and memory;
-- see "Proof.SMT.System".
module Proof.Functional.Induction
  ( arrRoundTrip,
    shiftsSane,
    baseCase,
    indStep,
    indStep1,
    indStep2,
    indStep3,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Proof.Functional.Obligation
import Proof.Machine (SysG)
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayEmbeddings)
import Proof.SMT.Symbolic (expectValid)
import Proof.SMT.System
import SymFC (SymFC (..))
import qualified SymFC.BuiltIn as SymFC
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Sanity probes for the trusted embeddings -------------------------------------
--
-- The term axioms in "Proof.SMT.Axioms" replace Haskell functions by hand-written SMT
-- counterparts, so they are trusted, not proved. These two probes check each
-- embedding against facts a broken one would get wrong.

-- | The register-file array embedding: a read after a write at the same index
-- gives the written value.
{-# ANN arrRoundTrip (SymFC ['arrayEmbeddings]) #-}
arrRoundTrip :: SymFC.Sym ()
arrRoundTrip = do
  a <- freshRA
  i <- SymFC.fresh
  v <- SymFC.fresh
  expectValid $ loadRA (storeRA a i v) i == v

-- | The shift embeddings: identities that would fail if the three shifts were
-- mixed up, the zero-extension of the amount were wrong, or the arithmetic
-- shift lost its sign.
{-# ANN shiftsSane (SymFC ['arrayEmbeddings]) #-}
shiftsSane :: SymFC.Sym ()
shiftsSane = do
  x <- SymFC.fresh @Word
  let sign = slice d31 d31 x
  expectValid $
    Core.sllWord x 0 == x
      && Core.srlWord x 0 == x
      && Core.sraWord x 0 == x
      && Core.sllWord x 1 == x + x
      && Core.srlWord x 31 == (if sign == 1 then 1 else 0)
      && Core.sraWord x 31 == (if sign == 1 then 0xFFFFFFFF else 0)

-- The base case ----------------------------------------------------------------

{-# ANN baseCase (SymFC ['arrayEmbeddings]) #-}
baseCase :: SymFC.Sym ()
baseCase = do
  ra <- freshRA
  ma <- freshMA
  wr <- SymFC.fresh
  wa <- SymFC.fresh
  pc <- SymFC.fresh
  expectValid $ baseCaseObligation (RegArrF ra) ma wr wa pc

-- The inductive steps ----------------------------------------------------------

type HopObligation = RegIdx -> Address -> Address -> SysG RegArrF MemArr -> Bool

holdsOnEveryHop :: HopObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  sys <- freshSys
  wr <- SymFC.fresh
  wa <- SymFC.fresh
  ipc <- SymFC.fresh
  expectValid $ obligation wr wa ipc sys

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStep (SymFC ['arrayEmbeddings]) #-}
indStep :: SymFC.Sym ()
indStep = holdsOnEveryHop indStepObligation

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStep1 (SymFC ['arrayEmbeddings]) #-}
indStep1 :: SymFC.Sym ()
indStep1 = holdsOnEveryHop indStepObligation1

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStep2 (SymFC ['arrayEmbeddings]) #-}
indStep2 :: SymFC.Sym ()
indStep2 = holdsOnEveryHop indStepObligation2

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStep3 (SymFC ['arrayEmbeddings]) #-}
indStep3 :: SymFC.Sym ()
indStep3 = holdsOnEveryHop indStepObligation3
