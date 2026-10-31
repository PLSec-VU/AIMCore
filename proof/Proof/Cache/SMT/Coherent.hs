-- | The SMT side of a cache that is coherent by construction
-- ('Proof.Cache.Functional.Obligation.cachedOf'): the term axioms in
-- "Proof.Cache.SMT.Axioms" ('Proof.Cache.SMT.Axioms.coherentAxioms') embed
-- 'Memory.Cache4K.DataArr' as 'DataC', which carries only arrays:
--
--   * the words written since construction, and which slots were written;
--   * the memory and tags the construction reads from, and the raw contents.
--
-- A read takes a written word if there is one, else memory's word if the line
-- is valid, else the raw word. Everything stays an array, so the solver can
-- share it across the cache's branches; a Haskell function in its place makes
-- the longest hop exhaust memory before the query is even built.
module Proof.Cache.SMT.Coherent
  ( DataC (..),
    freshDataC,
    dataReadC,
    dataWriteC,
    mkDataC,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Data.Coerce (coerce)
import Memory.Cache4K
import Memory.Types (MemOps (..))
import Proof.Cache.SMT.Array (freshRaw, freshTag)
import Proof.Cache.SMT.Raw
import Proof.SMT.Array (MemArr, freshMA)
import qualified SymFC.BuiltIn as P
import SymFC.BuiltIn (Embeddable, Embedding (..), embedding)
import Types (Word)
import Prelude hiding (Ordering (..), Word, init, log, not, repeat, undefined, (!!), (&&), (++), (||))

-- The data array ------------------------------------------------------------------

-- | 'Memory.Cache4K.DataArr' on the proof side: the words written since
-- construction, which slots were written (nonzero = written), and the memory,
-- tags and raw contents the construction reads from.
data DataC = DataC RawData RawData MemArr TagArr RawData

-- | The embedding of 'Memory.Cache4K.dataRead'.
dataReadC :: forall arr. Embeddable arr DataC => BitVector 10 -> arr -> Word
dataReadC i a
  | Embedding <- embedding @arr = readC (coerce a)
  where
    readC (DataC ws written mem tags raw)
      | rawRead i written /= 0 = rawRead i ws
      | entryValid e = memReadWord (wordAddr (entryTag e) si (slice d1 d0 i) 0) mem
      | otherwise = rawRead i raw
      where
        e = tagRead si tags
    si = slice d9 d2 i

-- | The embedding of 'Memory.Cache4K.dataWrite'.
dataWriteC :: forall arr. Embeddable arr DataC => BitVector 10 -> Word -> arr -> arr
dataWriteC i v a
  | Embedding <- embedding @arr = coerce (writeC (coerce a))
  where
    writeC (DataC ws written mem tags raw) = DataC (rawWrite i v ws) (rawWrite i 1 written) mem tags raw

-- | The embedding of 'Proof.Cache.Functional.Obligation.mkDataMem': nothing
-- written yet.
mkDataC :: forall arr. Embeddable arr DataC => MemArr -> TagArr -> RawData -> arr
mkDataC mem t raw
  | Embedding <- embedding @arr = coerce (DataC rawZero rawZero mem t raw)

-- | The embedding of 'Proof.Cache.SMT.Array.freshData': any coherent-cache
-- data array, writes included.
freshDataC :: forall arr. Embeddable arr DataC => P.Sym arr
freshDataC
  | Embedding <- embedding @arr =
      coerce <$> (DataC <$> freshRaw <*> freshRaw <*> freshMA <*> freshTag <*> freshRaw)
