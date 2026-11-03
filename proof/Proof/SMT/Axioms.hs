{-# LANGUAGE MagicHash #-}

-- | The embedding sets every proof builds on: the embeddings of 'base', of the
-- Clash numeric types and of the ALU shifts, and memory as an SMT array
-- ("Proof.SMT.Array"). The core's register file adds its own in
-- "Proof.Core.SMT.Axioms".
--
-- This lives in its own module because GHC's stage restriction requires a value
-- mentioned in an @ANN@ pragma to be imported rather than defined locally.
module Proof.SMT.Axioms (embeddings, memEmbeddings) where

import qualified Clash.Sized.Internal.Unsigned as Unsigned
import Proof.SMT.Array
import Stepper.Concrete (baseEmbeddings)
import SymFC (Embeddings (..))

-- | The stepper's base set ('base', the Clash numeric types and the ALU
-- shifts), plus the conversion of 'Unsigned' to 'Integer'.
embeddings :: Embeddings
embeddings = baseEmbeddings <> unsignedEmbeddings

-- | What symfc-clash leaves out for 'Unsigned': its conversion to 'Integer',
-- which 'fromIntegral' goes through.
unsignedEmbeddings :: Embeddings
unsignedEmbeddings =
  Embeddings
    { typeEmbeddings = [],
      termEmbeddings = [('Unsigned.toInteger#, 'unsignedToIntegerE)]
    }

-- | The memory-as-SMT-array embedding, on top of the base set.
--
-- Monomorphic by necessity: an array needs concrete index and element sorts.
memEmbeddings :: Embeddings
memEmbeddings =
  embeddings
    <> Embeddings
      { typeEmbeddings = [(''MemArr, ''MemArrSMT)],
        termEmbeddings =
          [ ('freshMA, 'freshMAE),
            ('loadM, 'loadME),
            ('storeM, 'storeME),
            ('eqM, 'eqME)
          ]
      }
