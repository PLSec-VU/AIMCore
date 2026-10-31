-- | The cache's functional proof, checked symbolically: the cache's counterpart
-- of "Proof.Core.Functional.Solver".
--
-- 'baseCase' plus one property per hop length: on a hop of that length, the
-- corresponding obligation in "Proof.Cache.Functional.Obligation" holds. The
-- statements live there; this module only instantiates them, over every tag
-- array, data array, memory, request and witness.
--
-- Each step starts from a ready server whose cache is coherent by
-- construction ('mkData').
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Cache.Functional.Solver
  ( cacheArraysP,
    coherentDataP,
    coherentWriteP,
    coherentLayoutP,
    baseCaseP,
    indStep0P,
    indStep4P,
    indStep8P,
    readyUnderInvP,
    readyIdleNoWaitP,
    waitsAfterRequestP,
    results,
  )
where

import Clash.Prelude (BitVector, d0, d1, d2, d9, not, slice, (++#), (||))
import Core (Request)
import qualified Core
import Memory.Cache4KServer (CacheSrv (..), CacheSrvState)
import Memory.Cache4K
import Memory.Types (MemOps (..))
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.SMT.Axioms (cacheAxioms, coherentAxioms)
import Proof.Cache.SMT.Raw (RawData, rawRead)
import Proof.Cache.Functional.Contract (readyIdleNoWait, readyUnderInv, waitsAfterRequest)
import Proof.Cache.Functional.Obligation
import Proof.Core.Functional.Machine (SysG (..))
import Proof.Core.Functional.System (KState, sysOf)
import Proof.SMT.Array (MemArr, RegArr)
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Types
import Prelude hiding (Word, not, (||))

-- Sanity probes for the trusted embeddings -------------------------------------

-- | The tag and data array embeddings, which are trusted, against facts a
-- broken one would get wrong.
{-# ANN cacheArraysP (Theory cacheAxioms) #-}
cacheArraysP ::
  TagArr -> DataArr -> SetIdx -> BitVector 21 -> BitVector 10 -> Word -> Pantomime.Bool
cacheArraysP t d s e i w =
  Pantomime.boolean $
    tagRead s (tagWrite s e t) == e
      && dataRead i (dataWrite i w d) == w
      && tagRead s tagEmpty == 0
      && dataRead i dataZero == 0

-- | The embedding of the coherent cache ('mkData' as
-- 'Proof.Cache.SMT.Coherent.DataC'), which is trusted, against what 'mkData'
-- defines: a read of the constructed array is memory's word if the line is
-- valid, else the raw word. Writes are checked by 'coherentWrite'.
{-# ANN coherentDataP (Theory coherentAxioms) #-}
coherentDataP :: MemArr -> TagArr -> RawData -> BitVector 10 -> Pantomime.Bool
coherentDataP ma t raw i =
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
{-# ANN coherentWriteP (Theory coherentAxioms) #-}
coherentWriteP :: DataArr -> BitVector 10 -> BitVector 10 -> Word -> Pantomime.Bool
coherentWriteP d i j w =
  Pantomime.boolean $
    dataRead i (dataWrite i w d) == w
      && (i == j || dataRead j (dataWrite i w d) == dataRead j d)

-- | The coherent cache's index layout agrees with the cache's addressing: at
-- any address whose line is valid and holds its block, the cache's word (index
-- set, then word, as 'Memory.Cache4K' composes it) is memory's word there.
{-# ANN coherentLayoutP (Theory coherentAxioms) #-}
coherentLayoutP :: MemArr -> TagArr -> RawData -> Address -> Pantomime.Bool
coherentLayoutP ma t raw a =
  Pantomime.boolean $
    not (entryValid e && entryTag e == tagOf a)
      || dataRead (setOf a ++# wordOf a) (mkData ma t raw)
        == memReadWord (wordAddr (tagOf a) (setOf a) (wordOf a) 0) ma
  where
    e = tagRead (setOf a) t

-- The base case ----------------------------------------------------------------

-- | Under 'cacheAxioms': the initial cache is the empty one ('Memory.Cache4K.dataZero'),
-- not a constructed one.
{-# ANN baseCaseP (Theory cacheAxioms) #-}
baseCaseP ::
  Address -> SetIdx -> WordIdx -> MemArr -> Pantomime.Bool
baseCaseP wa wsi wwi ma =
  Pantomime.boolean $ baseCase wa wsi wwi ma

-- The inductive steps ----------------------------------------------------------

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0P (Theory coherentAxioms) #-}
indStep0P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request -> Pantomime.Bool
indStep0P wa wsi wwi ma t raw req =
  Pantomime.boolean $ indStep0 wa wsi wwi ma t raw req

-- | The hop of a read that misses one line.
{-# ANN indStep4P (Theory coherentAxioms) #-}
indStep4P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request -> Pantomime.Bool
indStep4P wa wsi wwi ma t raw req =
  Pantomime.boolean $ indStep4 wa wsi wwi ma t raw req

-- | The hop of a read that misses both its lines.
{-# ANN indStep8P (Theory coherentAxioms) #-}
indStep8P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request -> Pantomime.Bool
indStep8P wa wsi wwi ma t raw req =
  Pantomime.boolean $ indStep8 wa wsi wwi ma t raw req

-- The contract -----------------------------------------------------------------
--
-- The properties of "Proof.Cache.Functional.Contract": the cache's over a
-- server with arbitrary tags, data, memory and state, the core's over a core
-- state built from symbolic parts as in the core's proof
-- ('Proof.Core.Functional.System.sysOf').

-- | A server related to ideal memory by the invariant is ready.
{-# ANN readyUnderInvP (Theory cacheAxioms) #-}
readyUnderInvP ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> DataArr -> MemArr -> CacheSrvState -> Pantomime.Bool
readyUnderInvP wa wsi wwi ideal t d ma st =
  Pantomime.boolean $ readyUnderInv wa wsi wwi ideal (CacheSrv (Cache4K t d) ma st)

-- | A ready server that is asked nothing answers within the cycle.
{-# ANN readyIdleNoWaitP (Theory cacheAxioms) #-}
readyIdleNoWaitP :: TagArr -> DataArr -> MemArr -> CacheSrvState -> Pantomime.Bool
readyIdleNoWaitP t d ma st = Pantomime.boolean $ readyIdleNoWait (CacheSrv (Cache4K t d) ma st)

-- | The core waits for what it asked for.
{-# ANN waitsAfterRequestP (Theory arrayAxioms) #-}
waitsAfterRequestP :: RegIdx -> KState -> Core.Input -> RegArr -> MemArr -> Pantomime.Bool
waitsAfterRequestP wr ss i ra ma =
  Pantomime.boolean $ waitsAfterRequest wr (sysCore (sysOf ss i ra ma)) i

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("cacheArraysP", $(pantomime 'cacheArraysP)),
    ("coherentDataP", $(pantomime 'coherentDataP)),
    ("coherentWriteP", $(pantomime 'coherentWriteP)),
    ("coherentLayoutP", $(pantomime 'coherentLayoutP)),
    ("baseCaseP", $(pantomime 'baseCaseP)),
    ("indStep0P", $(pantomime 'indStep0P)),
    ("indStep4P", $(pantomime 'indStep4P)),
    ("indStep8P", $(pantomime 'indStep8P)),
    ("readyUnderInvP", $(pantomime 'readyUnderInvP)),
    ("readyIdleNoWaitP", $(pantomime 'readyIdleNoWaitP)),
    ("waitsAfterRequestP", $(pantomime 'waitsAfterRequestP))
  ]
