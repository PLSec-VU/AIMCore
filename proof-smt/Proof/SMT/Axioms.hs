-- | The axiom set used by the proof properties.
--
-- This lives in its own module because GHC's stage restriction requires a value
-- mentioned in an @ANN@ pragma to be imported rather than defined locally.
module Proof.SMT.Axioms (embeddings, arrayEmbeddings) where

import Proof.SMT.Array
import Stepper.Concrete (baseEmbeddings)
import SymFC (Embeddings (..))

-- | 'base' embeddings plus the Clash numeric types ('BitVector', 'Unsigned',
-- 'Signed', 'Bit'), which is what this core's types are built from, plus this
-- core's own shift wrappers. The stepper uses the same set.
embeddings :: Embeddings
embeddings = baseEmbeddings

-- | The register-file-as-SMT-array embedding, on top of the base set.
--
-- Monomorphic by necessity: an array needs concrete index and element sorts.
arrayEmbeddings :: Embeddings
arrayEmbeddings =
  embeddings
    <> Embeddings
      { typeEmbeddings =
        [ (''RegArr, ''RegArrSMT),
          (''MemArr, ''MemArrSMT)
        ],
        termEmbeddings =
          [ ('freshRA, 'freshRAE),
            ('freshMA, 'freshMAE),
            ('loadRA, 'loadRAE),
            ('zeroRA, 'zeroRAE),
            ('storeRA, 'storeRAE),
            ('loadM, 'loadME),
            ('storeM, 'storeME)
          ]
      }
