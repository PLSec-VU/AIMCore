-- | The cache's functional proof, checked symbolically: the cache's counterpart
-- of "Proof.Core.Functional.Induction".
--
-- 'baseCase' plus one property per hop length: on a hop of that length, the
-- corresponding obligation in "Proof.Cache.Functional.Obligation" holds. The
-- statements live there; this module only instantiates them, over every tag
-- array, data array, memory, request and witness.
--
-- The server's state is symbolic too: every state is covered, not only the
-- ready one the invariant requires.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Cache.Functional.Induction
  ( cacheArrays,
    coherentData,
    coherentWrite,
    coherentLayout,
    baseCase,
    indStep0,
    indStep4,
    indStep8,
    results,
  )
where

import Clash.Prelude (BitVector, d0, d1, d2, d9, not, slice, (++#), (||))
import qualified Core
import Memory.Cache4K
import Memory.Cache4KServer (CacheSrvState)
import Memory.Types (MemOps (..))
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.SMT.Axioms (cacheAxioms, coherentAxioms)
import Proof.Cache.SMT.Raw (RawData, rawRead)
import Proof.Cache.Functional.Obligation
import Proof.Core.Functional.System (KState, sysOf)
import Proof.SMT.Array (MemArr, RegArr, RegArrF (..))
import Proof.SMT.Logged (pantomime)
import Types
import Prelude hiding (Word, not, (||))

-- Sanity probes for the trusted embeddings -------------------------------------

-- | The tag and data array embeddings, which are trusted, against facts a
-- broken one would get wrong.
{-# ANN cacheArrays (Theory cacheAxioms) #-}
cacheArrays ::
  TagArr -> DataArr -> SetIdx -> BitVector 21 -> BitVector 10 -> Word -> Pantomime.Bool
cacheArrays t d s e i w =
  Pantomime.boolean $
    tagRead s (tagWrite s e t) == e
      && dataRead i (dataWrite i w d) == w
      && tagRead s tagEmpty == 0
      && dataRead i dataZero == 0

-- | The embedding of the coherent cache ('mkData' as
-- 'Proof.Cache.SMT.Coherent.DataC'), which is trusted, against what 'mkData'
-- defines: a read of the constructed array is memory's word if the line is
-- valid, else the raw word. Writes are checked by 'coherentWrite'.
{-# ANN coherentData (Theory coherentAxioms) #-}
coherentData :: MemArr -> TagArr -> RawData -> BitVector 10 -> Pantomime.Bool
coherentData ma t raw i =
  Pantomime.boolean $ dataRead i (mkData ma t raw) == constructed i
  where
    constructed k
      | entryValid e = memReadWord (wordAddr (entryTag e) si (slice d1 d0 k) 0) ma
      | otherwise = rawRead k raw
      where
        si = slice d9 d2 k
        e = tagRead si t

-- | A write on any data array of the coherent cache's embedding, including one
-- that already holds writes: a read after it gives the written word at its
-- index and leaves the other slots as they were. Since it holds for every
-- array, it covers any sequence of writes.
{-# ANN coherentWrite (Theory coherentAxioms) #-}
coherentWrite :: DataArr -> BitVector 10 -> BitVector 10 -> Word -> Pantomime.Bool
coherentWrite d i j w =
  Pantomime.boolean $
    dataRead i (dataWrite i w d) == w
      && (i == j || dataRead j (dataWrite i w d) == dataRead j d)

-- | The coherent cache's index layout agrees with the cache's addressing: at
-- any address whose line is valid and holds its block, the cache's word (index
-- set, then word, as 'Memory.Cache4K' composes it) is memory's word there.
{-# ANN coherentLayout (Theory coherentAxioms) #-}
coherentLayout :: MemArr -> TagArr -> RawData -> Address -> Pantomime.Bool
coherentLayout ma t raw a =
  Pantomime.boolean $
    not (entryValid e && entryTag e == tagOf a)
      || dataRead (setOf a ++# wordOf a) (mkData ma t raw)
        == memReadWord (wordAddr (tagOf a) (setOf a) (wordOf a) 0) ma
  where
    e = tagRead (setOf a) t

-- The base case ----------------------------------------------------------------

-- | Under 'cacheAxioms': the initial cache is the empty one ('Memory.Cache4K.dataZero'),
-- not a constructed one.
{-# ANN baseCase (Theory cacheAxioms) #-}
baseCase ::
  Address -> RegArr -> MemArr -> RegIdx -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
baseCase pc ra ma wr wa wsi wwi =
  Pantomime.boolean $ baseCaseObligation pc (RegArrF ra) ma wr wa wsi wwi

-- The inductive steps ----------------------------------------------------------
--
-- The ideal system is built from symbolic parts as in the core's proof
-- ('Proof.Core.Functional.System.sysOf'); the cache adds its tags, the
-- contents of its invalid lines and its state.

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0 (Theory coherentAxioms) #-}
indStep0 ::
  KState -> Core.Input -> RegArr -> MemArr -> TagArr -> RawData -> CacheSrvState ->
  RegIdx -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
indStep0 ss i ra ma t raw st wr wa wsi wwi =
  Pantomime.boolean $ indStepObligation0 wr wa wsi wwi (sysOf ss i ra ma) t raw st

-- | The hop of a read that misses one line.
{-# ANN indStep4 (Theory coherentAxioms) #-}
indStep4 ::
  KState -> Core.Input -> RegArr -> MemArr -> TagArr -> RawData -> CacheSrvState ->
  RegIdx -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
indStep4 ss i ra ma t raw st wr wa wsi wwi =
  Pantomime.boolean $ indStepObligation4 wr wa wsi wwi (sysOf ss i ra ma) t raw st

-- | The hop of a read that misses both its lines.
{-# ANN indStep8 (Theory coherentAxioms) #-}
indStep8 ::
  KState -> Core.Input -> RegArr -> MemArr -> TagArr -> RawData -> CacheSrvState ->
  RegIdx -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
indStep8 ss i ra ma t raw st wr wa wsi wwi =
  Pantomime.boolean $ indStepObligation8 wr wa wsi wwi (sysOf ss i ra ma) t raw st

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("cacheArrays", $(pantomime 'cacheArrays)),
    ("coherentData", $(pantomime 'coherentData)),
    ("coherentWrite", $(pantomime 'coherentWrite)),
    ("coherentLayout", $(pantomime 'coherentLayout)),
    ("baseCase", $(pantomime 'baseCase)),
    ("indStep0", $(pantomime 'indStep0)),
    ("indStep4", $(pantomime 'indStep4)),
    ("indStep8", $(pantomime 'indStep8))
  ]
