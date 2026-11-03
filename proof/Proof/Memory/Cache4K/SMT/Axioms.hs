-- | The embeddings for the properties about "Memory.Cache4K": those of the
-- register file and memory, and the two cache arrays. In its own module for the
-- same reason as "Proof.SMT.Axioms".
module Proof.Memory.Cache4K.SMT.Axioms (cacheEmbeddings, coherentEmbeddings) where

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
import Proof.Core.SMT.Axioms (arrayEmbeddings)
import Proof.Memory.Cache4K.SMT.Array
import Proof.Memory.Cache4K.Server.Functional.Obligation (mkDataMem)
import Proof.Memory.Cache4K.SMT.Coherent
import Proof.Memory.Cache4K.SMT.Raw
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
            ('tagEq, 'tagEqE),
            ('dataRead, 'dataReadE),
            ('dataWrite, 'dataWriteE),
            ('dataZero, 'dataZeroE),
            ('dataEq, 'dataEqE)
          ]
      }

-- | The embeddings for the cache's proof: as 'cacheEmbeddings', but the data
-- array is embedded as 'Proof.Memory.Cache4K.SMT.Coherent.DataC', so that
-- 'Proof.Memory.Cache4K.Server.Functional.Obligation.mkData' builds a cache coherent by
-- construction (through its 'MemArr' instance,
-- 'Proof.Memory.Cache4K.Server.Functional.Obligation.mkDataMem'). The raw-array and tag
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
