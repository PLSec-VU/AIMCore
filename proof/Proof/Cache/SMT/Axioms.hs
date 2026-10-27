-- | The axioms for the properties about "Memory.Cache4K": those of the register
-- file and memory, and the two cache arrays. In its own module for the same
-- reason as "Proof.SMT.Axioms".
module Proof.Cache.SMT.Axioms (cacheAxioms, coherentAxioms) where

import qualified Data.Map as Map
import Memory.Cache4K (DataArr, TagArr, dataRead, dataWrite, dataZero, tagEmpty, tagRead, tagWrite)
import Pantomime (PluginAxioms (..))
import Proof.SMT.Axioms (arrayAxioms)
import Proof.Cache.SMT.Array
import Proof.Cache.Functional.Obligation (mkDataMem)
import Proof.Cache.SMT.Coherent
import Proof.Cache.SMT.Raw

cacheAxioms :: PluginAxioms
cacheAxioms =
  arrayAxioms
    <> PluginAxioms
      { typeAxioms =
          Map.fromList
            [ (''TagArr, ''TagArrSMT),
              (''DataArr, ''DataArrSMT)
            ],
        termAxioms =
          [ ('tagRead, 'tagReadE),
            ('tagWrite, 'tagWriteE),
            ('tagEmpty, 'tagEmptyE),
            ('dataRead, 'dataReadE),
            ('dataWrite, 'dataWriteE),
            ('dataZero, 'dataZeroE)
          ]
      }

-- | The axioms for the cache's proof: as 'cacheAxioms', but the data array is
-- embedded as 'Proof.Cache.SMT.Coherent.DataC', so that
-- 'Proof.Cache.Functional.Obligation.mkData' builds a cache coherent by
-- construction (through its 'MemArr' instance,
-- 'Proof.Cache.Functional.Obligation.mkDataMem'). The raw-array and tag axioms
-- come first: an embedding that calls another wrapper ('dataReadC' calls
-- 'rawRead' and 'tagRead') only sees the axioms listed before its own.
coherentAxioms :: PluginAxioms
coherentAxioms =
  arrayAxioms
    <> PluginAxioms
      { typeAxioms =
          Map.fromList
            [ (''TagArr, ''TagArrSMT),
              (''DataArr, ''DataC),
              (''RawData, ''DataArrSMT)
            ],
        termAxioms =
          [ ('rawRead, 'dataReadE),
            ('rawWrite, 'dataWriteE),
            ('rawZero, 'dataZeroE),
            ('tagRead, 'tagReadE),
            ('tagWrite, 'tagWriteE),
            ('tagEmpty, 'tagEmptyE),
            ('dataRead, 'dataReadC),
            ('dataWrite, 'dataWriteC),
            ('mkDataMem, 'mkDataC)
          ]
      }
