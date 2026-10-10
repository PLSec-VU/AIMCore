-- | The axiom set used by the proof properties.
--
-- This lives in its own module because GHC's stage restriction requires a value
-- mentioned in an @ANN@ pragma to be imported rather than defined locally.
module Proof.SMT.Axioms (axioms, arrayAxioms) where

import Proof.SMT.Array
import Core (sllWord, sraWord, srlWord)
import qualified Data.Map as Map
import Pantomime (PluginAxioms (..))
import qualified Pantomime.Base as Base
import qualified Pantomime.Clash as Clash

-- | 'base' embeddings plus the Clash numeric types ('BitVector', 'Unsigned',
-- 'Signed', 'Bit'), which is what this core's types are built from, plus this
-- core's own shift wrappers.
--
-- The shift axioms belong here rather than in 'arrayAxioms' because they are
-- part of the core's semantics, not of the container encoding: any property that
-- runs the ALU needs them. Without them 'Core.sllWord' and friends are OPAQUE
-- names with no unfolding, and symbolic execution stops with \"Unbound variable
-- in symbolise\".
axioms :: PluginAxioms
axioms = Base.axioms <> Clash.axioms <> shiftAxioms

-- | The ALU shifts, mapped to the SMT bitvector shifts.
--
-- See 'Core.sllWord' for why the wrappers exist: they keep the shift amount a
-- bitvector, so a query stays in the bitvector-and-array fragment that Bitwuzla
-- and Yices can read.
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

-- | The register-file-as-SMT-array embedding, on top of the base set.
--
-- Monomorphic by necessity: an array needs concrete index and element sorts.
arrayAxioms :: PluginAxioms
arrayAxioms =
  axioms
    <> PluginAxioms
      { typeAxioms =
          Map.fromList
            [ (''RegArr, ''RegArrSMT),
              (''MemArr, ''MemArrSMT)
            ],
        termAxioms =
          [ ('loadRA, 'loadRAE),
            ('zeroRA, 'zeroRAE),
            ('storeRA, 'storeRAE),
            ('loadM, 'loadME),
            ('storeM, 'storeME)
          ]
      }
