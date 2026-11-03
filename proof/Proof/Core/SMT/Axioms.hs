-- | The embeddings for the properties about the core: those every proof builds
-- on ("Proof.SMT.Axioms"), plus the core's register file as an SMT array
-- ("Proof.Core.SMT.Array").
--
-- This lives in its own module because GHC's stage restriction requires a value
-- mentioned in an @ANN@ pragma to be imported rather than defined locally.
module Proof.Core.SMT.Axioms (arrayEmbeddings) where

import Proof.Core.SMT.Array
import Proof.SMT.Axioms (memEmbeddings)
import SymFC (Embeddings (..))

-- | The register-file-as-SMT-array embedding, on top of memory and the base
-- set.
--
-- Monomorphic by necessity: an array needs concrete index and element sorts.
arrayEmbeddings :: Embeddings
arrayEmbeddings =
  memEmbeddings
    <> Embeddings
      { typeEmbeddings = [(''RegArr, ''RegArrSMT)],
        termEmbeddings =
          [ ('freshRA, 'freshRAE),
            ('loadRA, 'loadRAE),
            ('zeroRA, 'zeroRAE),
            ('storeRA, 'storeRAE),
            ('eqRA, 'eqRAE)
          ]
      }
