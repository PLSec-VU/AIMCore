-- | The cache's functional proof, checked symbolically: the cache's counterpart
-- of "Proof.Core.Functional.Solver".
--
-- The statements live in "Proof.Memory.Cache4K.Server.Functional.Obligation",
-- "Proof.Memory.Cache4K.Server.Functional.Driver" and
-- "Proof.Memory.Cache4K.Server.Functional.Contract"; this module only
-- instantiates them, over every tag array, data array, memory, request and
-- witness. Each property is named after the statement it checks, with a @P@:
--
--   * sanity probes for the trusted array embeddings;
--   * the driver against its operational reading ('srvDriverMatchesSpecP'), and
--     its values, the hop lengths of the steps ('srvDriverValuesP');
--   * the base case, and one step per hop length (0, 'penalty', 2 * 'penalty'),
--     each starting from a ready server whose cache is coherent by construction
--     ('mkData'); for the hops that stall, a sanity check that the answers
--     before the last are 'Nothing' ('indStepWaits4P', 'indStepWaits8P');
--   * the contract between core and cache.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Memory.Cache4K.Server.Functional.Solver
  ( cacheArraysP,
    coherentDataP,
    coherentWriteP,
    coherentLayoutP,
    srvDriverMatchesSpecP,
    srvDriverValuesP,
    srvInvFunctionalP,
    baseCaseP,
    indStep0P,
    indStep4P,
    indStepWaits4P,
    indStep8P,
    indStepWaits8P,
    srvReadyIffAnsweredP,
    srvReadyUnderInvP,
    corePatientP,
  )
where

import Clash.Prelude (BitVector, d0, d1, d2, d9, not, slice, (++#), (||))
import Core.Pipeline as Core (Request)
import Memory.Cache4KServer (CacheSrv (..))
import Memory.Cache4K
import Memory.Types (MemOps (..))
import Proof.Core.Functional.Machine (SysG (..))
import Proof.Core.Functional.System (freshSys)
import Proof.Core.SMT.Axioms (arrayEmbeddings)
import Proof.Memory.Cache4K.SMT.Array (freshData, freshRaw, freshTag)
import Proof.Memory.Cache4K.SMT.Axioms (cacheEmbeddings, coherentEmbeddings)
import Proof.Memory.Cache4K.SMT.Raw (RawData, rawRead)
import Proof.Memory.Cache4K.Server.Functional.Contract
  ( srvReadyIffAnswered,
    srvReadyUnderInv,
    corePatient,
    srvDriverMatchesSpec,
  )
import Proof.Memory.Cache4K.Server.Functional.Driver (srvDriverValues)
import Proof.Memory.Cache4K.Server.Functional.Invariant (srvInvFunctional)
import Proof.Memory.Cache4K.Server.Functional.Obligation
import Proof.SMT.Array (MemArr, freshMA)
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
  tags <- freshTag
  dat <- freshData
  s <- SymFC.fresh @SetIdx
  e <- SymFC.fresh @(BitVector 21)
  i <- SymFC.fresh @(BitVector 10)
  w <- SymFC.fresh @Word
  expectValid $
    tagRead s (tagWrite s e tags) == e
      && dataRead i (dataWrite i w dat) == w
      && tagRead s tagEmpty == 0
      && dataRead i dataZero == 0

-- | The embedding of the coherent cache ('mkData' as
-- 'Proof.Memory.Cache4K.SMT.Coherent.DataC'), which is trusted, against what
-- 'mkData' defines: a read of the constructed array is memory's word if the
-- line is valid, else the raw word. Writes are checked by 'coherentWriteP'.
{-# ANN coherentDataP (SymFC ['coherentEmbeddings]) #-}
coherentDataP :: SymFC.Sym ()
coherentDataP = do
  ma <- freshMA
  tags <- freshTag
  raw <- freshRaw
  i <- SymFC.fresh @(BitVector 10)
  let si = slice d9 d2 i
      e = tagRead si tags
      constructed
        | entryValid e =
            memReadWord (wordAddr (entryTag e) si (slice d1 d0 i) 0) ma
        | otherwise = rawRead i raw
  expectValid $ dataRead i (mkData ma tags raw) == constructed

-- | A write on any data array of the coherent cache's embedding, including one
-- that already holds writes: a read after it gives the written word at its
-- index and leaves the other slots as they were. Since it holds for every
-- array, it covers any sequence of writes.
{-# ANN coherentWriteP (SymFC ['coherentEmbeddings]) #-}
coherentWriteP :: SymFC.Sym ()
coherentWriteP = do
  dat <- freshData
  i <- SymFC.fresh @(BitVector 10)
  j <- SymFC.fresh @(BitVector 10)
  w <- SymFC.fresh @Word
  expectValid $
    dataRead i (dataWrite i w dat) == w
      && (i == j || dataRead j (dataWrite i w dat) == dataRead j dat)

-- | The coherent cache's index layout agrees with the cache's addressing: at
-- any address whose line is valid and holds its block, the cache's word (index
-- set, then word, as 'Memory.Cache4K' composes it) is memory's word there.
{-# ANN coherentLayoutP (SymFC ['coherentEmbeddings]) #-}
coherentLayoutP :: SymFC.Sym ()
coherentLayoutP = do
  ma <- freshMA
  tags <- freshTag
  raw <- freshRaw
  a <- SymFC.fresh @Address
  let e = tagRead (setOf a) tags
  expectValid $
    not (entryValid e && entryTag e == tagOf a)
      || dataRead (setOf a ++# wordOf a) (mkData ma tags raw)
        == memReadWord (wordAddr (tagOf a) (setOf a) (wordOf a) 0) ma

-- Servers -----------------------------------------------------------------------

-- | A server with arbitrary tags, data, memory and state.
freshServer :: SymFC.Sym (CacheSrv MemArr)
freshServer = do
  tags <- freshTag
  dat <- freshData
  ma <- freshMA
  st <- SymFC.fresh
  pure (CacheSrv (Cache4K tags dat) ma st)

type ServerObligation = CacheSrv MemArr -> Request -> Bool

holdsOnEveryRequest :: ServerObligation -> SymFC.Sym ()
holdsOnEveryRequest obligation = do
  srv <- freshServer
  req <- SymFC.fresh
  expectValid $ obligation srv req

-- The driver -------------------------------------------------------------------

-- | The driver is 0, 'penalty' or twice 'penalty', on any server.
{-# ANN srvDriverValuesP (SymFC ['cacheEmbeddings]) #-}
srvDriverValuesP :: SymFC.Sym ()
srvDriverValuesP = holdsOnEveryRequest srvDriverValues

-- | From a ready server, the driver agrees with its operational reading.
{-# ANN srvDriverMatchesSpecP (SymFC ['cacheEmbeddings]) #-}
srvDriverMatchesSpecP :: SymFC.Sym ()
srvDriverMatchesSpecP = holdsOnEveryRequest srvDriverMatchesSpec

-- The invariant ----------------------------------------------------------------

-- | The invariant is functional: two ideal memories related to the same server
-- are equal.
{-# ANN srvInvFunctionalP (SymFC ['cacheEmbeddings]) #-}
srvInvFunctionalP :: SymFC.Sym ()
srvInvFunctionalP = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  ideal1 <- freshMA
  ideal2 <- freshMA
  srv <- freshServer
  expectValid $ srvInvFunctional wa wsi wwi ideal1 ideal2 srv

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

type HopObligation =
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request -> Bool

holdsOnEveryHop :: HopObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  ma <- freshMA
  tags <- freshTag
  raw <- freshRaw
  req <- SymFC.fresh
  expectValid $ obligation wa wsi wwi ma tags raw req

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0P (SymFC ['coherentEmbeddings]) #-}
indStep0P :: SymFC.Sym ()
indStep0P = holdsOnEveryHop indStep0

-- | The hop of a read that misses one line: 'penalty' + 1 cycles.
{-# ANN indStep4P (SymFC ['coherentEmbeddings]) #-}
indStep4P :: SymFC.Sym ()
indStep4P = holdsOnEveryHop indStep4

-- | Sanity check: during that hop, the answers before the last are 'Nothing'.
{-# ANN indStepWaits4P (SymFC ['coherentEmbeddings]) #-}
indStepWaits4P :: SymFC.Sym ()
indStepWaits4P = holdsOnEveryHop indStepWaits4

-- | The hop of a read that misses both its lines: 2 * 'penalty' + 1 cycles.
{-# ANN indStep8P (SymFC ['coherentEmbeddings]) #-}
indStep8P :: SymFC.Sym ()
indStep8P = holdsOnEveryHop indStep8

-- | Sanity check: during that hop, the answers before the last are 'Nothing'.
{-# ANN indStepWaits8P (SymFC ['coherentEmbeddings]) #-}
indStepWaits8P :: SymFC.Sym ()
indStepWaits8P = holdsOnEveryHop indStepWaits8

-- The contract -----------------------------------------------------------------
--
-- The properties of "Proof.Memory.Cache4K.Server.Functional.Contract": the
-- cache's over a server with arbitrary tags, data, memory and state, the core's
-- over a fresh system state as in the core's proof
-- ('Proof.Core.Functional.System.freshSys').

-- | After a step, the server is ready exactly when it has answered.
{-# ANN srvReadyIffAnsweredP (SymFC ['cacheEmbeddings]) #-}
srvReadyIffAnsweredP :: SymFC.Sym ()
srvReadyIffAnsweredP = holdsOnEveryRequest srvReadyIffAnswered

-- | A server related to ideal memory by the invariant is ready.
{-# ANN srvReadyUnderInvP (SymFC ['cacheEmbeddings]) #-}
srvReadyUnderInvP :: SymFC.Sym ()
srvReadyUnderInvP = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  ideal <- freshMA
  srv <- freshServer
  expectValid $ srvReadyUnderInv wa wsi wwi ideal srv

-- | The core waits for what it asked for.
{-# ANN corePatientP (SymFC ['arrayEmbeddings]) #-}
corePatientP :: SymFC.Sym ()
corePatientP = do
  sys <- freshSys
  expectValid $ corePatient (sysCore sys) (sysInput sys)
