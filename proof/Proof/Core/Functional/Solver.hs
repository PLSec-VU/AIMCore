-- | The inductive steps of the refinement proof, checked symbolically.
--
-- 'baseCase' says the invariant holds once the reset state has taken its first
-- hop. Then one property per driver delay: if the invariant relates @(isa,
-- sys)@ and the driver says the hop takes @k + 1@ cycles, then after those
-- cycles (and one ISA step, where the hop retires an instruction) the invariant
-- relates them again. Base case plus the four steps is the whole refinement
-- theorem.
--
-- Checking one @k@ at a time keeps the number of unrolled cycles concrete,
-- which sidesteps Pantomime's termination check: @stepSysN (driver sys + 1)@
-- would recurse on a symbolic count.
--
-- Each property is checked by the plugin at compile time and spliced into
-- 'results': 'Nothing' when valid, @'Just' counterexample@ when not. The
-- statements themselves live in "Proof.Core.Functional.Obligation", shared with
-- the QuickCheck harness so the two cannot drift.
--
-- The pipeline state is passed as a 'KState' plus SMT-array register file and
-- memory; see "Proof.Core.Functional.System".
module Proof.Core.Functional.Solver
  ( arrRoundTripP,
    shiftsSaneP,
    driverMatchesSpecP,
    driverMatchesSpecInitP,
    driverValuesP,
    baseCaseP,
    indStep0P,
    indStep1P,
    indStep2P,
    indStep3P,
    pcUnderInvP,
    regFileUnderInvP,
    memUnderInvP,
    invFunctionalP,
    results,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Core.ISA (IsaStateG (..))
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Core.Functional.Driver
  ( driverMatchesSpec,
    driverMatchesSpecInit,
    driverValues,
  )
import Proof.Core.Functional.Invariant
  ( invFunctional,
    memUnderInv,
    pcUnderInv,
    regFileUnderInv,
  )
import Proof.Core.Functional.Obligation
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Proof.Core.Functional.System
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Sanity probes for the trusted embeddings -------------------------------------
--
-- The term axioms in "Proof.SMT.Axioms" replace Haskell functions by
-- hand-written SMT counterparts, so they are trusted, not proved. These two
-- probes check each embedding against facts a broken one would get wrong.

-- | The register-file array embedding: a read after a write at the same index
-- gives the written value.
{-# ANN arrRoundTripP (Theory arrayAxioms) #-}
arrRoundTripP :: RegArr -> RegIdx -> Word -> Pantomime.Bool
arrRoundTripP a i v = Pantomime.boolean $ loadRA (storeRA a i v) i == v

-- | The shift embeddings: identities that would fail if the three shifts were
-- mixed up, the zero-extension of the amount were wrong, or the arithmetic
-- shift lost its sign.
{-# ANN shiftsSaneP (Theory arrayAxioms) #-}
shiftsSaneP :: Word -> Pantomime.Bool
shiftsSaneP x =
  Pantomime.boolean $
    Core.sllWord x 0 == x
      && Core.srlWord x 0 == x
      && Core.sraWord x 0 == x
      && Core.sllWord x 1 == x + x
      && Core.srlWord x 31 == (if sign == 1 then 1 else 0)
      && Core.sraWord x 31 == (if sign == 1 then 0xFFFFFFFF else 0)
  where
    sign = slice d31 d31 x

-- The driver -------------------------------------------------------------------

-- | The driver is 0 to 3, on any system.
{-# ANN driverValuesP (Theory arrayAxioms) #-}
driverValuesP :: KState -> Core.Input -> RegArr -> MemArr -> Pantomime.Bool
driverValuesP ss i ra ma =
  Pantomime.boolean $ driverValues (sysOf ss i ra ma)

-- | On states the invariant relates to a free ISA state, the driver agrees
-- with its operational reading.
{-# ANN driverMatchesSpecP (Theory arrayAxioms) #-}
driverMatchesSpecP ::
  RegIdx -> Address -> Address -> RegArr -> MemArr ->
  KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
driverMatchesSpecP wr wa ipc ira ima ss i ra ma =
  Pantomime.boolean $
    driverMatchesSpec wr wa (IsaState ipc (RegArrF ira) ima) (sysOf ss i ra ma)

-- | On the initial system, the driver agrees with its operational reading, for
-- any start PC, register file and memory.
{-# ANN driverMatchesSpecInitP (Theory arrayAxioms) #-}
driverMatchesSpecInitP :: Address -> RegArr -> MemArr -> Pantomime.Bool
driverMatchesSpecInitP pc ra ma =
  Pantomime.boolean $ driverMatchesSpecInit pc (RegArrF ra) ma

-- The invariant and the architectural state -------------------------------------

-- | The invariant relating the core to the ISA implies that the ISA's PC is
-- the core's architectural one. The ISA state is free, not derived from the
-- core.
{-# ANN pcUnderInvP (Theory arrayAxioms) #-}
pcUnderInvP ::
  RegIdx -> Address -> Address -> RegArr -> MemArr ->
  KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
pcUnderInvP wr wa ipc ira ima ss i ra ma =
  Pantomime.boolean $
    pcUnderInv wr wa (IsaState ipc (RegArrF ira) ima) (sysOf ss i ra ma)

-- | The invariant relating the core to the ISA implies that the ISA's register
-- file is the core's architectural one. The ISA state is free, not derived from
-- the core.
{-# ANN regFileUnderInvP (Theory arrayAxioms) #-}
regFileUnderInvP ::
  RegIdx -> Address -> Address -> RegArr -> MemArr ->
  KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
regFileUnderInvP wr wa ipc ira ima ss i ra ma =
  Pantomime.boolean $
    regFileUnderInv wr wa (IsaState ipc (RegArrF ira) ima) (sysOf ss i ra ma)

-- | The invariant relating the core to the ISA implies that the ISA's memory is
-- the core's architectural one.
{-# ANN memUnderInvP (Theory arrayAxioms) #-}
memUnderInvP ::
  RegIdx -> Address -> Address -> RegArr -> MemArr ->
  KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
memUnderInvP wr wa ipc ira ima ss i ra ma =
  Pantomime.boolean $
    memUnderInv wr wa (IsaState ipc (RegArrF ira) ima) (sysOf ss i ra ma)

-- | The invariant is functional: two ISA states, both free, related to the
-- same system are equal.
{-# ANN invFunctionalP (Theory arrayAxioms) #-}
invFunctionalP ::
  RegIdx -> Address ->
  Address -> RegArr -> MemArr ->
  Address -> RegArr -> MemArr ->
  KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
invFunctionalP wr wa ipc1 ira1 ima1 ipc2 ira2 ima2 ss i ra ma =
  Pantomime.boolean $
    invFunctional
      wr
      wa
      (IsaState ipc1 (RegArrF ira1) ima1)
      (IsaState ipc2 (RegArrF ira2) ima2)
      (sysOf ss i ra ma)

-- The base case ----------------------------------------------------------------

{-# ANN baseCaseP (Theory arrayAxioms) #-}
baseCaseP :: RegIdx -> Address -> Address -> RegArr -> MemArr -> Pantomime.Bool
baseCaseP wr wa pc ra ma =
  Pantomime.boolean $ baseCase wr wa pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStep0P (Theory arrayAxioms) #-}
indStep0P ::
  RegIdx -> Address -> KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
indStep0P wr wa ss i ra ma =
  Pantomime.boolean $ indStep0 wr wa (sysOf ss i ra ma)

-- | @k = 1@: the two-cycle hop (a memory instruction in writeback).
{-# ANN indStep1P (Theory arrayAxioms) #-}
indStep1P ::
  RegIdx -> Address -> KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
indStep1P wr wa ss i ra ma =
  Pantomime.boolean $ indStep1 wr wa (sysOf ss i ra ma)

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The hop on which the ISA enters a halt; a halted state then sits at k = 0.
{-# ANN indStep2P (Theory arrayAxioms) #-}
indStep2P ::
  RegIdx -> Address -> KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
indStep2P wr wa ss i ra ma =
  Pantomime.boolean $ indStep2 wr wa (sysOf ss i ra ma)

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStep3P (Theory arrayAxioms) #-}
indStep3P ::
  RegIdx -> Address -> KState -> Core.Input -> RegArr -> MemArr ->
  Pantomime.Bool
indStep3P wr wa ss i ra ma =
  Pantomime.boolean $ indStep3 wr wa (sysOf ss i ra ma)

results :: [(String, Maybe String)]
results =
  [ ("arrRoundTripP", $(pantomime 'arrRoundTripP)),
    ("shiftsSaneP", $(pantomime 'shiftsSaneP)),
    ("driverMatchesSpecP", $(pantomime 'driverMatchesSpecP)),
    ("driverMatchesSpecInitP", $(pantomime 'driverMatchesSpecInitP)),
    ("driverValuesP", $(pantomime 'driverValuesP)),
    ("baseCaseP", $(pantomime 'baseCaseP)),
    ("indStep0P", $(pantomime 'indStep0P)),
    ("indStep1P", $(pantomime 'indStep1P)),
    ("indStep2P", $(pantomime 'indStep2P)),
    ("indStep3P", $(pantomime 'indStep3P)),
    ("pcUnderInvP", $(pantomime 'pcUnderInvP)),
    ("regFileUnderInvP", $(pantomime 'regFileUnderInvP)),
    ("memUnderInvP", $(pantomime 'memUnderInvP)),
    ("invFunctionalP", $(pantomime 'invFunctionalP))
  ]
