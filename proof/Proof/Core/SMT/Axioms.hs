-- | The axioms for the properties about the core: those every proof builds on
-- ("Proof.SMT.Axioms"), plus the core's shift wrappers and its register file as
-- an SMT array ("Proof.Core.SMT.Array").
--
-- This lives in its own module because GHC's stage restriction requires a value
-- mentioned in an @ANN@ pragma to be imported rather than defined locally.
module Proof.Core.SMT.Axioms (arrayAxioms) where

import Core.Alu (sllWord, sraWord, srlWord)
import qualified Data.Map as Map
import Pantomime (PluginAxioms (..))
import Proof.Core.SMT.Array
import Proof.SMT.Axioms (memAxioms)

-- | The ALU shifts, mapped to the SMT bitvector shifts.
--
-- They are part of the core's semantics, not of the container encoding: any
-- property that runs the ALU needs them. Without them 'Core.Alu.sllWord' and
-- friends are OPAQUE names with no unfolding, and symbolic execution stops
-- with \"Unbound variable in symbolise\". See 'Core.Alu.sllWord' for why the
-- wrappers exist: they keep the shift amount a bitvector, so a query stays in
-- the bitvector-and-array fragment that Bitwuzla and Yices can read.
shiftAxioms :: PluginAxioms
shiftAxioms =
  PluginAxioms
    { typeAxioms = Map.empty,
      termAxioms =
        [ ('sllWord, 'sllWordE),
          ('srlWord, 'srlWordE),
          ('sraWord, 'sraWordE)
        ]
    }

-- | The register-file-as-SMT-array embedding and the shifts, on top of memory
-- and the base set.
--
-- Monomorphic by necessity: an array needs concrete index and element sorts.
arrayAxioms :: PluginAxioms
arrayAxioms =
  memAxioms
    <> shiftAxioms
    <> PluginAxioms
      { typeAxioms = Map.fromList [(''RegArr, ''RegArrSMT)],
        termAxioms =
          [ ('loadRA, 'loadRAE),
            ('zeroRA, 'zeroRAE),
            ('storeRA, 'storeRAE),
            ('eqRA, 'eqRAE)
          ]
      }
