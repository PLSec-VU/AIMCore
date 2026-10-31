-- | The embeddings for the properties about "Memory.Cache4K": those of the
-- register file and memory, and the two cache arrays. In its own module for the
-- same reason as "Proof.SMT.Axioms".
module Proof.Cache.SMT.Axioms (cacheEmbeddings, coherentEmbeddings) where

import Memory.Cache4K (DataArr, TagArr, dataRead, dataWrite, dataZero, tagEmpty, tagRead, tagWrite)
import Proof.Cache.Functional.Obligation (mkDataMem)
import Proof.Cache.SMT.Array
import Proof.Cache.SMT.Coherent
import Proof.Cache.SMT.Raw
import Proof.SMT.Axioms (arrayEmbeddings)
import SymFC (Embeddings (..))

cacheEmbeddings :: Embeddings
cacheEmbeddings =
  arrayEmbeddings
    <> Embeddings
      { typeEmbeddings =
          [ (''TagArr, ''TagArrSMT),
            (''DataArr, ''DataArrSMT)
          ],
        termEmbeddings =
          [ ('freshTag, 'freshTagE),
            ('freshData, 'freshDataE),
            ('tagRead, 'tagReadE),
            ('tagWrite, 'tagWriteE),
            ('tagEmpty, 'tagEmptyE),
            ('dataRead, 'dataReadE),
            ('dataWrite, 'dataWriteE),
            ('dataZero, 'dataZeroE)
          ]
      }

-- | The embeddings for the cache's proof: as 'cacheEmbeddings', but the data
-- array is embedded as 'Proof.Cache.SMT.Coherent.DataC', so that
-- 'Proof.Cache.Functional.Obligation.mkData' builds a cache coherent by
-- construction (through its 'MemArr' instance,
-- 'Proof.Cache.Functional.Obligation.mkDataMem'). The raw-array and tag
-- embeddings come first: an embedding that calls another wrapper ('dataReadC'
-- calls 'rawRead' and 'tagRead') only sees the embeddings listed before its own.
coherentEmbeddings :: Embeddings
coherentEmbeddings =
  arrayEmbeddings
    <> Embeddings
      { typeEmbeddings =
          [ (''TagArr, ''TagArrSMT),
            (''DataArr, ''DataC),
            (''RawData, ''DataArrSMT)
          ],
        termEmbeddings =
          [ ('freshRaw, 'freshDataE),
            ('rawRead, 'dataReadE),
            ('rawWrite, 'dataWriteE),
            ('rawZero, 'dataZeroE),
            ('freshTag, 'freshTagE),
            ('tagRead, 'tagReadE),
            ('tagWrite, 'tagWriteE),
            ('tagEmpty, 'tagEmptyE),
            ('freshData, 'freshDataC),
            ('dataRead, 'dataReadC),
            ('dataWrite, 'dataWriteC),
            ('mkDataMem, 'mkDataC)
          ]
      }
