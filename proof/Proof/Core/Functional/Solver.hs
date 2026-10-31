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
-- "Proof.Core.Functional.Obligation", shared with the QuickCheck harness so the
-- two cannot drift.
--
-- The pipeline state is a 'KState' plus SMT-array register file and memory;
-- see "Proof.Core.Functional.System".
module Proof.Core.Functional.Solver
  ( arrRoundTripP,
    shiftsSaneP,
    baseCaseP,
    indStep0P,
    indStep1P,
    indStep2P,
    indStep3P,
    regFileUnderInvP,
    memUnderInvP,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Core.ISA (IsaStateG (..))
import Proof.Core.Functional.Machine (SysG)
import Proof.Core.Functional.Obligation
import Proof.Core.Functional.System
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayEmbeddings)
import Proof.SMT.Symbolic (expectValid)
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
{-# ANN arrRoundTripP (SymFC ['arrayEmbeddings]) #-}
arrRoundTripP :: SymFC.Sym ()
arrRoundTripP = do
  a <- freshRA
  i <- SymFC.fresh
  v <- SymFC.fresh
  expectValid $ loadRA (storeRA a i v) i == v

-- | The shift embeddings: identities that would fail if the three shifts were
-- mixed up, the zero-extension of the amount were wrong, or the arithmetic
-- shift lost its sign.
{-# ANN shiftsSaneP (SymFC ['arrayEmbeddings]) #-}
shiftsSaneP :: SymFC.Sym ()
shiftsSaneP = do
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

{-# ANN baseCaseP (SymFC ['arrayEmbeddings]) #-}
baseCaseP :: SymFC.Sym ()
baseCaseP = do
  wr <- SymFC.fresh
  wa <- SymFC.fresh
  pc <- SymFC.fresh
  ra <- freshRA
  ma <- freshMA
  expectValid $ baseCase wr wa pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------

type HopObligation = RegIdx -> Address -> Address -> SysG RegArrF MemArr -> Bool

holdsOnEveryHop :: HopObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  wr <- SymFC.fresh
  wa <- SymFC.fresh
  ipc <- SymFC.fresh
  sys <- freshSys
  expectValid $ obligation wr wa ipc sys

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStep0P (SymFC ['arrayEmbeddings]) #-}
indStep0P :: SymFC.Sym ()
indStep0P = holdsOnEveryHop indStep0

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStep1P (SymFC ['arrayEmbeddings]) #-}
indStep1P :: SymFC.Sym ()
indStep1P = holdsOnEveryHop indStep1

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStep2P (SymFC ['arrayEmbeddings]) #-}
indStep2P :: SymFC.Sym ()
indStep2P = holdsOnEveryHop indStep2

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStep3P (SymFC ['arrayEmbeddings]) #-}
indStep3P :: SymFC.Sym ()
indStep3P = holdsOnEveryHop indStep3

-- The invariant and the flush ---------------------------------------------------

type FlushObligation = RegIdx -> Address -> IsaStateG RegArrF MemArr -> SysG RegArrF MemArr -> Bool

-- | Checked for any ISA state, not only one derived from the core.
holdsUnderInv :: FlushObligation -> SymFC.Sym ()
holdsUnderInv obligation = do
  wr <- SymFC.fresh
  wa <- SymFC.fresh
  ipc <- SymFC.fresh
  ira <- freshRA
  ima <- freshMA
  sys <- freshSys
  expectValid $ obligation wr wa (IsaState ipc (RegArrF ira) ima) sys

-- | The invariant relating the core to the ISA implies that the ISA's register
-- file is the core's, flushed.
{-# ANN regFileUnderInvP (SymFC ['arrayEmbeddings]) #-}
regFileUnderInvP :: SymFC.Sym ()
regFileUnderInvP = holdsUnderInv regFileUnderInv

-- | The invariant relating the core to the ISA implies that the ISA's memory is
-- the core's, flushed.
{-# ANN memUnderInvP (SymFC ['arrayEmbeddings]) #-}
memUnderInvP :: SymFC.Sym ()
memUnderInvP = holdsUnderInv memUnderInv
