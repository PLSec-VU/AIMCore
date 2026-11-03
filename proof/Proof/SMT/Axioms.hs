{-# LANGUAGE MagicHash #-}

-- | The axiom sets every proof builds on: the embeddings of 'base' and of the
-- Clash numeric types, and memory as an SMT array ("Proof.SMT.Array"). The
-- core's register file and shifts add theirs in "Proof.Core.SMT.Axioms".
--
-- This lives in its own module because GHC's stage restriction requires a value
-- mentioned in an @ANN@ pragma to be imported rather than defined locally.
module Proof.SMT.Axioms (axioms, memAxioms) where

import Proof.SMT.Array
import qualified Data.Map as Map
import Pantomime (PluginAxioms (..))
import qualified Pantomime.Base as Base
import qualified Pantomime.Clash as Clash
import qualified Clash.Sized.Internal.Unsigned as Unsigned

-- | 'base' embeddings plus the Clash numeric types ('BitVector', 'Unsigned',
-- 'Signed', 'Bit'), which is what this core's types are built from.
axioms :: PluginAxioms
axioms = Base.axioms <> Clash.axioms <> unsignedAxioms

-- | What pantomime-clash leaves out for 'Unsigned': its conversion to
-- 'Integer', which 'fromIntegral' goes through.
unsignedAxioms :: PluginAxioms
unsignedAxioms =
  PluginAxioms
    { typeAxioms = Map.empty,
      termAxioms = [('Unsigned.toInteger#, 'unsignedToIntegerE)]
    }

-- | The memory-as-SMT-array embedding, on top of the base set.
--
-- Monomorphic by necessity: an array needs concrete index and element sorts.
memAxioms :: PluginAxioms
memAxioms =
  axioms
    <> PluginAxioms
      { typeAxioms = Map.fromList [(''MemArr, ''MemArrSMT)],
        termAxioms =
          [ ('loadM, 'loadME),
            ('storeM, 'storeME),
            ('eqM, 'eqME)
          ]
      }
