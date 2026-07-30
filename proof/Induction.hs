-- | The inductive steps of the refinement proof, checked symbolically.
--
-- One property per driver delay: if the invariant relates @(isa, sys)@ and the
-- driver says the hop takes @k + 1@ cycles, then after those cycles (and one
-- ISA step, where the hop retires an instruction) the invariant relates them
-- again. Together with the base case -- 'Core.init' satisfies the startup case
-- by construction -- these four properties are the whole refinement theorem.
--
-- Checking one @k@ at a time keeps the number of unrolled cycles concrete,
-- which sidesteps Pantomime's termination check: @stepSysN (driver sys + 1)@
-- would recurse on a symbolic count.
--
-- Each property is checked by the plugin at compile time and spliced into
-- 'results': 'Nothing' when valid, @'Just' counterexample@ when not. The
-- statements themselves live in "Obligation", shared with the QuickCheck
-- harness so the two cannot drift.
--
-- The pipeline state is passed as an ADT of scalars ('KState') plus SMT-array
-- register file and memory: an ADT of scalars can be a fresh symbolic
-- argument, a record containing a function cannot, and the Clash @Vec@ API is
-- opaque to the plugin (see "ArrayRF").
module Induction
  ( arrRoundTrip,
    shiftsSane,
    indStep0,
    indStep1,
    indStep2,
    indStep3,
    results,
  )
where

import ArrayRF
import Axioms (arrayAxioms)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Instruction
import LoggedPantomime (pantomime)
import Machine
import Obligation
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The pipeline registers, as plain scalars.
data KState = KState
  { kFePc :: Address,
    kDePc :: Address,
    kExPc :: Address,
    kExIr :: Instruction,
    kMeIr :: Instruction,
    kMeRes :: Word,
    kMeAddr :: Address,
    kWbIr :: Instruction,
    kWbRes :: Word,
    kCtrl :: Core.Control Identity,
    kHalt :: Maybe Core.HaltState,
    kHaltPending :: Maybe Core.HaltState
  }

-- | Assemble a system state from the symbolic pieces.
sysOf :: KState -> Core.Input Identity -> RegArr -> MemArr -> SysG RegArrF MemArr
sysOf ss i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = kFePc ss,
            Core.stateDePc = kDePc ss,
            Core.stateExPc = kExPc ss,
            Core.stateExInstr = kExIr ss,
            Core.stateMeInstr = kMeIr ss,
            Core.stateMeRes = Identity (kMeRes ss),
            Core.stateMeAddr = kMeAddr ss,
            Core.stateWbInstr = kWbIr ss,
            Core.stateWbRes = Identity (kWbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = kCtrl ss,
            Core.stateHalt = kHalt ss,
            Core.stateHaltPending = kHaltPending ss
          },
      sysInput = i,
      sysMem = ma
    }

-- Sanity probes for the trusted embeddings -------------------------------------
--
-- The term axioms in "Axioms" replace Haskell functions by hand-written SMT
-- counterparts, so they are trusted, not proved. These two probes check each
-- embedding against facts a broken one would get wrong.

-- | The register-file array embedding: a read after a write at the same index
-- gives the written value.
{-# ANN arrRoundTrip (Theory arrayAxioms) #-}
arrRoundTrip :: RegArr -> RegIdx -> Word -> Pantomime.Bool
arrRoundTrip a i v = Pantomime.boolean $ loadRA (storeRA a i v) i == v

-- | The shift embeddings: identities that would fail if the three shifts were
-- mixed up, the zero-extension of the amount were wrong, or the arithmetic
-- shift lost its sign.
{-# ANN shiftsSane (Theory arrayAxioms) #-}
shiftsSane :: Word -> Pantomime.Bool
shiftsSane x =
  Pantomime.boolean $
    Core.sllWord x 0 == x
      && Core.srlWord x 0 == x
      && Core.sraWord x 0 == x
      && Core.sllWord x 1 == x + x
      && Core.srlWord x 31 == (if sign == 1 then 1 else 0)
      && Core.sraWord x 31 == (if sign == 1 then 0xFFFFFFFF else 0)
  where
    sign = slice d31 d31 x

-- The inductive steps ----------------------------------------------------------

-- | @k = 0@: the one-cycle hop (steady, writeback non-memory).
{-# ANN indStep0 (Theory arrayAxioms) #-}
indStep0 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
indStep0 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation wr wa (sysOf ss i ra ma)

-- | @k = 1@: the two-cycle hop (startup, or a memory instruction in writeback).
{-# ANN indStep1 (Theory arrayAxioms) #-}
indStep1 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
indStep1 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation1 wr wa (sysOf ss i ra ma)

-- | @k = 2@: the three-cycle hop (environment, taken jump, store hazard with a
-- non-memory execute instruction, or memory instructions in both older stages).
-- The only hop on which the ISA can halt.
{-# ANN indStep2 (Theory arrayAxioms) #-}
indStep2 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
indStep2 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation2 wr wa (sysOf ss i ra ma)

-- | @k = 3@: the four-cycle hop (store hazard with a memory execute
-- instruction, load hazard, or all three stages holding memory instructions).
{-# ANN indStep3 (Theory arrayAxioms) #-}
indStep3 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
indStep3 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation3 wr wa (sysOf ss i ra ma)

results :: [(String, Maybe String)]
results =
  [ ("arrRoundTrip", $(pantomime 'arrRoundTrip)),
    ("shiftsSane", $(pantomime 'shiftsSane)),
    ("indStep0", $(pantomime 'indStep0)),
    ("indStep1", $(pantomime 'indStep1)),
    ("indStep2", $(pantomime 'indStep2)),
    ("indStep3", $(pantomime 'indStep3))
  ]
