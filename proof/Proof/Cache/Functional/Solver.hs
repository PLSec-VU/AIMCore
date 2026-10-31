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
  )
where

import Clash.Prelude (BitVector, d0, d1, d2, d9, not, slice, (++#), (||))
import Core (Request)
import Memory.Cache4KServer (CacheSrv (..))
import Memory.Cache4K
import Memory.Types (MemOps (..))
import Proof.Cache.Functional.Contract (readyIdleNoWait, readyUnderInv, waitsAfterRequest)
import Proof.Cache.Functional.Obligation
import Proof.Cache.SMT.Array (freshData, freshRaw, freshTag)
import Proof.Cache.SMT.Axioms (cacheEmbeddings, coherentEmbeddings)
import Proof.Cache.SMT.Raw (RawData, rawRead)
import Proof.Core.Functional.Machine (SysG (..))
import Proof.Core.Functional.System (freshSys)
import Proof.SMT.Array (MemArr, freshMA)
import Proof.SMT.Axioms (arrayEmbeddings)
import Proof.SMT.Symbolic (expectValid)
import SymFC (SymFC (..))
import qualified SymFC.BuiltIn as SymFC
import Types
import Prelude hiding (Word, not, (||))

-- Sanity probes for the trusted embeddings -------------------------------------

-- | The tag and data array embeddings, which are trusted, against facts a
-- broken one would get wrong.
{-# ANN cacheArraysP (SymFC ['cacheEmbeddings]) #-}
cacheArraysP :: SymFC.Sym ()
cacheArraysP = do
  t <- freshTag
  d <- freshData
  s <- SymFC.fresh @SetIdx
  e <- SymFC.fresh @(BitVector 21)
  i <- SymFC.fresh @(BitVector 10)
  w <- SymFC.fresh @Word
  expectValid $
    tagRead s (tagWrite s e t) == e
      && dataRead i (dataWrite i w d) == w
      && tagRead s tagEmpty == 0
      && dataRead i dataZero == 0

-- | The embedding of the coherent cache ('mkData' as
-- 'Proof.Cache.SMT.Coherent.DataC'), which is trusted, against what 'mkData'
-- defines: a read of the constructed array is memory's word if the line is
-- valid, else the raw word. Writes are checked by 'coherentWriteP'.
{-# ANN coherentDataP (SymFC ['coherentEmbeddings]) #-}
coherentDataP :: SymFC.Sym ()
coherentDataP = do
  ma <- freshMA
  t <- freshTag
  raw <- freshRaw
  i <- SymFC.fresh @(BitVector 10)
  let si = slice d9 d2 i
      e = tagRead si t
      constructed
        | entryValid e = memReadWord (wordAddr (entryTag e) si (slice d1 d0 i) 0) ma
        | otherwise = rawRead i raw
  expectValid $ dataRead i (mkData ma t raw) == constructed

-- | A write on any data array of the coherent cache's embedding, including one
-- that already holds writes: a read after it gives the written word at its
-- index and leaves the other slots as they were. Since it holds for every
-- array, it covers any sequence of writes.
{-# ANN coherentWriteP (SymFC ['coherentEmbeddings]) #-}
coherentWriteP :: SymFC.Sym ()
coherentWriteP = do
  d <- freshData
  i <- SymFC.fresh @(BitVector 10)
  j <- SymFC.fresh @(BitVector 10)
  w <- SymFC.fresh @Word
  expectValid $
    dataRead i (dataWrite i w d) == w
      && (i == j || dataRead j (dataWrite i w d) == dataRead j d)

-- | The coherent cache's index layout agrees with the cache's addressing: at
-- any address whose line is valid and holds its block, the cache's word (index
-- set, then word, as 'Memory.Cache4K' composes it) is memory's word there.
{-# ANN coherentLayoutP (SymFC ['coherentEmbeddings]) #-}
coherentLayoutP :: SymFC.Sym ()
coherentLayoutP = do
  ma <- freshMA
  t <- freshTag
  raw <- freshRaw
  a <- SymFC.fresh @Address
  let e = tagRead (setOf a) t
  expectValid $
    not (entryValid e && entryTag e == tagOf a)
      || dataRead (setOf a ++# wordOf a) (mkData ma t raw)
        == memReadWord (wordAddr (tagOf a) (setOf a) (wordOf a) 0) ma

-- The base case ----------------------------------------------------------------

-- | Under 'cacheEmbeddings': the initial cache is the empty one
-- ('Memory.Cache4K.dataZero'), not a constructed one.
{-# ANN baseCaseP (SymFC ['cacheEmbeddings]) #-}
baseCaseP :: SymFC.Sym ()
baseCaseP = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  ma <- freshMA
  expectValid $ baseCase wa wsi wwi ma

-- The inductive steps ----------------------------------------------------------

type HopObligation = Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request -> Bool

holdsOnEveryHop :: HopObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  ma <- freshMA
  t <- freshTag
  raw <- freshRaw
  req <- SymFC.fresh
  expectValid $ obligation wa wsi wwi ma t raw req

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0P (SymFC ['coherentEmbeddings]) #-}
indStep0P :: SymFC.Sym ()
indStep0P = holdsOnEveryHop indStep0

-- | The hop of a read that misses one line.
{-# ANN indStep4P (SymFC ['coherentEmbeddings]) #-}
indStep4P :: SymFC.Sym ()
indStep4P = holdsOnEveryHop indStep4

-- | The hop of a read that misses both its lines.
{-# ANN indStep8P (SymFC ['coherentEmbeddings]) #-}
indStep8P :: SymFC.Sym ()
indStep8P = holdsOnEveryHop indStep8

-- The contract -----------------------------------------------------------------
--
-- The properties of "Proof.Cache.Functional.Contract": the cache's over a
-- server with arbitrary tags, data, memory and state, the core's over a fresh
-- system state as in the core's proof ('Proof.Core.Functional.System.freshSys').

freshServer :: SymFC.Sym (CacheSrv MemArr)
freshServer = do
  t <- freshTag
  d <- freshData
  ma <- freshMA
  st <- SymFC.fresh
  pure (CacheSrv (Cache4K t d) ma st)

-- | A server related to ideal memory by the invariant is ready.
{-# ANN readyUnderInvP (SymFC ['cacheEmbeddings]) #-}
readyUnderInvP :: SymFC.Sym ()
readyUnderInvP = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  ideal <- freshMA
  server <- freshServer
  expectValid $ readyUnderInv wa wsi wwi ideal server

-- | A ready server that is asked nothing answers within the cycle.
{-# ANN readyIdleNoWaitP (SymFC ['cacheEmbeddings]) #-}
readyIdleNoWaitP :: SymFC.Sym ()
readyIdleNoWaitP = expectValid . readyIdleNoWait =<< freshServer

-- | The core waits for what it asked for.
{-# ANN waitsAfterRequestP (SymFC ['arrayEmbeddings]) #-}
waitsAfterRequestP :: SymFC.Sym ()
waitsAfterRequestP = do
  wr <- SymFC.fresh
  sys <- freshSys
  expectValid $ waitsAfterRequest wr (sysCore sys) (sysInput sys)
