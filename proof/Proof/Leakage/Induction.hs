-- | The leakage refinement, checked symbolically.
--
-- One property per driver delay: on a hop of that length,
-- 'Proof.Leakage.Obligation.leakObligation' holds. Split by @k@ because the
-- number of unrolled cycles has to be concrete -- Pantomime cannot recurse on a
-- symbolic count -- and each property pins @driver == k@ so it carries one
-- unrolling rather than four.
--
-- These four plus 'Proof.Functional.Induction.baseCase' are the leakage
-- theorem: the functional half establishes that the invariant holds wherever
-- the driver lands, which is what each property here assumes.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/opt/homebrew/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
--
-- Z3 does not finish these in useful time; under Bitwuzla each solve is seconds
-- and symbolisation dominates. The whole module is well under an hour.
--
-- Each query runs the pipeline twice over -- once on the real state, once on
-- the censored one -- and compares two states plus two observation traces, so
-- they are larger than the corresponding 'Proof.Functional.Induction' ones.
-- They are stated in the form the QuickCheck harness (@test/LeakageSpec.hs@)
-- checks, so a counterexample to one is a counterexample to the other; when a
-- query comes back @sat@, splitting the premise or the conclusion localises it
-- faster than decoding the model (see @proof/notes/leakage.txt@).
module Proof.Leakage.Induction
  ( leakStep0,
    leakStep1,
    leakStep2,
    leakStep3,
    leakStep3a,
    leakStep3b,
    results,
  )
where

import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Proof.Driver (driver, loadHazardD)
import Instruction
import Proof.Leakage.Obligation (leakObligation)
import Proof.SMT.Logged (pantomime)
import Proof.Machine
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import RegFile (RegFileOps)
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The pipeline registers, as plain scalars.
--
-- An ADT of scalars can be a fresh symbolic argument; a record containing a
-- function cannot, and the Clash @Vec@ API is opaque to the plugin. Same shape
-- and same reason as 'Proof.Functional.Induction'\'s.
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

-- | 'Proof.Leakage.Obligation.leakObligation', restricted to hops of length
-- @k + 1@.
leakAt ::
  (RegFileOps r, MemOps m) =>
  Int -> RegIdx -> Address -> SysG r m -> Bool
leakAt k wr wa sys = driver sys /= k || leakObligation wr wa sys

-- | @k = 0@: the one-cycle hop.
{-# ANN leakStep0 (Theory arrayAxioms) #-}
leakStep0 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep0 ss i ra ma wr wa =
  Pantomime.boolean $ leakAt 0 wr wa (sysOf ss i ra ma)

-- | @k = 1@: the two-cycle hop (startup, or a memory instruction in writeback).
{-# ANN leakStep1 (Theory arrayAxioms) #-}
leakStep1 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep1 ss i ra ma wr wa =
  Pantomime.boolean $ leakAt 1 wr wa (sysOf ss i ra ma)

-- | @k = 2@: the three-cycle hop -- an environment instruction, or a taken
-- jump. The store-hazard route into this case is unreachable under the
-- strengthened 'Proof.Functional.Invariant.noStoreAlias'.
{-# ANN leakStep2 (Theory arrayAxioms) #-}
leakStep2 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep2 ss i ra ma wr wa =
  Pantomime.boolean $ leakAt 2 wr wa (sysOf ss i ra ma)

-- | @k = 3@: the four-cycle hop -- a load-use hazard, or memory instructions in
-- all three older stages.
{-# ANN leakStep3 (Theory arrayAxioms) #-}
leakStep3 :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep3 ss i ra ma wr wa =
  Pantomime.boolean $ leakAt 3 wr wa (sysOf ss i ra ma)

-- | @k = 3@ restricted to the load-use-hazard route.
--
-- Kept, with its annotation off, as a worked example of localising a @sat@
-- result: splitting @leakStep3@ on 'Proof.Driver.loadHazardD' says which of the
-- two shapes that reach a four-cycle hop is at fault, which is quicker than
-- decoding an array-valued model.
-- {-# ANN leakStep3a (Theory arrayAxioms) #-}
leakStep3a :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep3a ss i ra ma wr wa =
  Pantomime.boolean $
    let sys = sysOf ss i ra ma
     in not (loadHazardD sys) || leakAt 3 wr wa sys

-- | @k = 3@ restricted to the steady all-memory route.
-- {-# ANN leakStep3b (Theory arrayAxioms) #-}
leakStep3b :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
leakStep3b ss i ra ma wr wa =
  Pantomime.boolean $
    let sys = sysOf ss i ra ma
     in loadHazardD sys || leakAt 3 wr wa sys

-- | Verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid, @'Just' counterexample@ when it is not.
results :: [(String, Maybe String)]
results =
  [ ("leakStep0", $(pantomime 'leakStep0)),
    ("leakStep1", $(pantomime 'leakStep1)),
    ("leakStep2", $(pantomime 'leakStep2)),
    ("leakStep3", $(pantomime 'leakStep3))
  ]
