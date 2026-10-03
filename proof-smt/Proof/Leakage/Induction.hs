-- | The constant-time leakage refinement, checked symbolically.
--
-- One property per driver delay: on a hop of that length,
-- 'Proof.Leakage.Obligation.leakObligation' holds -- the projection commutes
-- with the hop and the two machines make the same observations. Each property
-- lives in its own module ("Proof.Leakage.Induction.Step0" to
-- "Proof.Leakage.Induction.Step3"); this one collects their verdicts.
--
-- These four plus 'Proof.Functional.Induction.baseCase' are the leakage
-- theorem: the functional half establishes that the invariant holds wherever
-- the driver lands, which is what each property here assumes. They quantify
-- over the same symbolic states as the functional steps
-- ('Proof.Functional.Induction.sysOf').
--
-- Run them under Bitwuzla, not Z3:
--
-- > SBV_Z3=/opt/homebrew/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack test
--
-- They are stated in the form the QuickCheck harness (@test/LeakageSpec.hs@)
-- checks, so a counterexample to one is a counterexample to the other; when a
-- query comes back @sat@, the harness's arbitrary-state properties are the
-- quickest way to a readable counterexample (see @proof/notes/leakage.txt@).
module Proof.Leakage.Induction
  ( results,
  )
where

import qualified Proof.Leakage.Induction.Step0 as Step0
import qualified Proof.Leakage.Induction.Step1 as Step1
import qualified Proof.Leakage.Induction.Step2 as Step2
import qualified Proof.Leakage.Induction.Step3 as Step3

-- | The verdicts of the four steps: 'Nothing' when valid.
results :: [(String, Maybe String)]
results = Step0.results <> Step1.results <> Step2.results <> Step3.results
