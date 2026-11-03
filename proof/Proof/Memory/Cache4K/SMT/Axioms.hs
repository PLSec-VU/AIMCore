-- | The axioms for the properties about "Memory.Cache4K": those of the register
-- file and memory, and the two cache arrays. In its own module for the same
-- reason as "Proof.SMT.Axioms".
module Proof.Memory.Cache4K.SMT.Axioms (cacheAxioms, coherentAxioms) where

import qualified Data.Map as Map
import Memory.Cache4K
  ( DataArr,
    TagArr,
    dataEq,
    dataRead,
    dataWrite,
    dataZero,
    tagEmpty,
    tagEq,
    tagRead,
    tagWrite,
  )
import Pantomime (PluginAxioms (..))
import Proof.Core.SMT.Axioms (arrayAxioms)
import Proof.Memory.Cache4K.SMT.Array
import Proof.Memory.Cache4K.Server.Functional.Obligation (mkDataMem)
import Proof.Memory.Cache4K.SMT.Coherent
import Proof.Memory.Cache4K.SMT.Raw

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
            ('tagEq, 'tagEqE),
            ('dataRead, 'dataReadE),
            ('dataWrite, 'dataWriteE),
            ('dataZero, 'dataZeroE),
            ('dataEq, 'dataEqE)
          ]
      }

-- | The axioms for the cache's proof: as 'cacheAxioms', but the data array is
-- embedded as 'Proof.Memory.Cache4K.SMT.Coherent.DataC', so that
-- 'Proof.Memory.Cache4K.Server.Functional.Obligation.mkData' builds a cache
-- coherent by construction (through its 'MemArr' instance,
-- 'Proof.Memory.Cache4K.Server.Functional.Obligation.mkDataMem'). The raw-array
-- and tag axioms come first: an embedding that calls another wrapper
-- ('dataReadC' calls 'rawRead' and 'tagRead') only sees the axioms listed
-- before its own.
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
