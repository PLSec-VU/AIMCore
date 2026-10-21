-- | The axioms for the properties about "Memory.Cache4K": those of the register
-- file and memory, and the two cache arrays. In its own module for the same
-- reason as "Proof.SMT.Axioms".
module Proof.SMT.CacheAxioms (cacheAxioms) where

import qualified Data.Map as Map
import Memory.Cache4K (DataArr, TagArr, dataRead, dataWrite, dataZero, tagEmpty, tagRead, tagWrite)
import Pantomime (PluginAxioms (..))
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.CacheArray

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
