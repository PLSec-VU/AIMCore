-- | The axiom set used by the proof properties.
--
-- This lives in its own module because GHC's stage restriction requires a value
-- mentioned in an @ANN@ pragma to be imported rather than defined locally.
module Axioms (axioms, arrayAxioms) where

import ArrayRF
import qualified Data.Map as Map
import Pantomime (PluginAxioms (..))
import qualified Pantomime.Base as Base
import qualified Pantomime.Clash as Clash

-- | 'base' embeddings plus the Clash numeric types ('BitVector', 'Unsigned',
-- 'Signed', 'Bit'), which is what this core's types are built from.
axioms :: PluginAxioms
axioms = Base.axioms <> Clash.axioms

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
            ('storeRA, 'storeRAE),
            ('loadM, 'loadME),
            ('storeM, 'storeME)
          ]
      }
