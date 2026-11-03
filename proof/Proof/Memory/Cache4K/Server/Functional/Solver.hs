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
    results,
  )
where

import Clash.Prelude (BitVector, d0, d1, d2, d9, not, slice, (++#), (||))
import Core.Pipeline as Core (Request)
import qualified Core.Pipeline as Core
import Memory.Cache4KServer (CacheSrv (..), CacheSrvState (..))
import Memory.Cache4K
import Memory.Types (MemOps (..))
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Memory.Cache4K.SMT.Axioms (cacheAxioms, coherentAxioms)
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
import Proof.Core.Functional.Machine (SysG (..))
import Proof.Core.Functional.System (KState, sysOf)
import Proof.Core.SMT.Array (RegArr)
import Proof.SMT.Array (MemArr)
import Proof.Core.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Types
import Prelude hiding (Word, not, (||))

-- Sanity probes for the trusted embeddings -------------------------------------

-- | The tag and data array embeddings, which are trusted, against facts a
-- broken one would get wrong.
{-# ANN cacheArraysP (Theory cacheAxioms) #-}
cacheArraysP ::
  TagArr -> DataArr -> SetIdx -> BitVector 21 -> BitVector 10 -> Word ->
  Pantomime.Bool
cacheArraysP tags dat s e i w =
  Pantomime.boolean $
    tagRead s (tagWrite s e tags) == e
      && dataRead i (dataWrite i w dat) == w
      && tagRead s tagEmpty == 0
      && dataRead i dataZero == 0

-- | The embedding of the coherent cache ('mkData' as
-- 'Proof.Memory.Cache4K.SMT.Coherent.DataC'), which is trusted, against what
-- 'mkData' defines: a read of the constructed array is memory's word if the
-- line is valid, else the raw word. Writes are checked by 'coherentWriteP'.
{-# ANN coherentDataP (Theory coherentAxioms) #-}
coherentDataP :: MemArr -> TagArr -> RawData -> BitVector 10 -> Pantomime.Bool
coherentDataP ma tags raw i =
  Pantomime.boolean $ dataRead i (mkData ma tags raw) == constructed i
  where
    constructed k
      | entryValid e =
          memReadWord (wordAddr (entryTag e) si (slice d1 d0 k) 0) ma
      | otherwise = rawRead k raw
      where
        si = slice d9 d2 k
        e = tagRead si tags

-- | A write on any data array of the coherent cache's embedding, including one
-- that already holds writes: a read after it gives the written word at its
-- index and leaves the other slots as they were. Since it holds for every
-- array, it covers any sequence of writes.
{-# ANN coherentWriteP (Theory coherentAxioms) #-}
coherentWriteP ::
  DataArr -> BitVector 10 -> BitVector 10 -> Word -> Pantomime.Bool
coherentWriteP dat i j w =
  Pantomime.boolean $
    dataRead i (dataWrite i w dat) == w
      && (i == j || dataRead j (dataWrite i w dat) == dataRead j dat)

-- | The coherent cache's index layout agrees with the cache's addressing: at
-- any address whose line is valid and holds its block, the cache's word (index
-- set, then word, as 'Memory.Cache4K' composes it) is memory's word there.
{-# ANN coherentLayoutP (Theory coherentAxioms) #-}
coherentLayoutP :: MemArr -> TagArr -> RawData -> Address -> Pantomime.Bool
coherentLayoutP ma tags raw a =
  Pantomime.boolean $
    not (entryValid e && entryTag e == tagOf a)
      || dataRead (setOf a ++# wordOf a) (mkData ma tags raw)
        == memReadWord (wordAddr (tagOf a) (setOf a) (wordOf a) 0) ma
  where
    e = tagRead (setOf a) tags

-- The driver -------------------------------------------------------------------

-- | The driver is 0, 'penalty' or twice 'penalty', on any server.
{-# ANN srvDriverValuesP (Theory cacheAxioms) #-}
srvDriverValuesP ::
  TagArr -> DataArr -> MemArr -> CacheSrvState -> Request -> Pantomime.Bool
srvDriverValuesP tags dat ma st req =
  Pantomime.boolean $ srvDriverValues (CacheSrv (Cache4K tags dat) ma st) req

-- | From a ready server, the driver agrees with its operational reading.
{-# ANN srvDriverMatchesSpecP (Theory cacheAxioms) #-}
srvDriverMatchesSpecP ::
  TagArr -> DataArr -> MemArr -> CacheSrvState -> Request -> Pantomime.Bool
srvDriverMatchesSpecP tags dat ma st req =
  Pantomime.boolean $
    srvDriverMatchesSpec (CacheSrv (Cache4K tags dat) ma st) req

-- The invariant ----------------------------------------------------------------

-- | The invariant is functional: two ideal memories related to the same server
-- are equal.
{-# ANN srvInvFunctionalP (Theory cacheAxioms) #-}
srvInvFunctionalP ::
  Address -> SetIdx -> WordIdx -> MemArr -> MemArr ->
  TagArr -> DataArr -> MemArr -> CacheSrvState -> Pantomime.Bool
srvInvFunctionalP wa wsi wwi ideal1 ideal2 tags dat ma st =
  Pantomime.boolean $
    srvInvFunctional wa wsi wwi ideal1 ideal2 srv
  where
    srv = CacheSrv (Cache4K tags dat) ma st

-- The base case ----------------------------------------------------------------

-- | Under 'cacheAxioms': the initial cache is the empty one
-- ('Memory.Cache4K.dataZero'), not a constructed one.
{-# ANN baseCaseP (Theory cacheAxioms) #-}
baseCaseP ::
  Address -> SetIdx -> WordIdx -> MemArr -> Pantomime.Bool
baseCaseP wa wsi wwi ma =
  Pantomime.boolean $ baseCase wa wsi wwi ma

-- The inductive steps ----------------------------------------------------------

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0P (Theory coherentAxioms) #-}
indStep0P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request ->
  Pantomime.Bool
indStep0P wa wsi wwi ma tags raw req =
  Pantomime.boolean $ indStep0 wa wsi wwi ma tags raw req

-- | The hop of a read that misses one line: 'penalty' + 1 cycles.
{-# ANN indStep4P (Theory coherentAxioms) #-}
indStep4P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request ->
  Pantomime.Bool
indStep4P wa wsi wwi ma tags raw req =
  Pantomime.boolean $ indStep4 wa wsi wwi ma tags raw req

-- | Sanity check: during that hop, the answers before the last are 'Nothing'.
{-# ANN indStepWaits4P (Theory coherentAxioms) #-}
indStepWaits4P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request ->
  Pantomime.Bool
indStepWaits4P wa wsi wwi ma tags raw req =
  Pantomime.boolean $ indStepWaits4 wa wsi wwi ma tags raw req

-- | The hop of a read that misses both its lines: 2 * 'penalty' + 1 cycles.
{-# ANN indStep8P (Theory coherentAxioms) #-}
indStep8P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request ->
  Pantomime.Bool
indStep8P wa wsi wwi ma tags raw req =
  Pantomime.boolean $ indStep8 wa wsi wwi ma tags raw req

-- | Sanity check: during that hop, the answers before the last are 'Nothing'.
{-# ANN indStepWaits8P (Theory coherentAxioms) #-}
indStepWaits8P ::
  Address -> SetIdx -> WordIdx -> MemArr -> TagArr -> RawData -> Request ->
  Pantomime.Bool
indStepWaits8P wa wsi wwi ma tags raw req =
  Pantomime.boolean $ indStepWaits8 wa wsi wwi ma tags raw req

-- The contract -----------------------------------------------------------------
--
-- The properties of "Proof.Memory.Cache4K.Server.Functional.Contract": the
-- cache's over a server with arbitrary tags, data, memory and state, the core's
-- over a core state built from symbolic parts as in the core's proof
-- ('Proof.Core.Functional.System.sysOf').

-- | After a step, the server is ready exactly when it has answered.
{-# ANN srvReadyIffAnsweredP (Theory cacheAxioms) #-}
srvReadyIffAnsweredP ::
  TagArr -> DataArr -> MemArr -> CacheSrvState -> Request -> Pantomime.Bool
srvReadyIffAnsweredP tags dat ma st req =
  Pantomime.boolean $
    srvReadyIffAnswered (CacheSrv (Cache4K tags dat) ma st) req

-- | A server related to ideal memory by the invariant is ready.
{-# ANN srvReadyUnderInvP (Theory cacheAxioms) #-}
srvReadyUnderInvP ::
  Address -> SetIdx -> WordIdx -> MemArr ->
  TagArr -> DataArr -> MemArr -> CacheSrvState -> Pantomime.Bool
srvReadyUnderInvP wa wsi wwi ideal tags dat ma st =
  Pantomime.boolean $
    srvReadyUnderInv wa wsi wwi ideal (CacheSrv (Cache4K tags dat) ma st)

-- | The core waits for what it asked for.
{-# ANN corePatientP (Theory arrayAxioms) #-}
corePatientP :: KState -> Core.Input -> RegArr -> MemArr -> Pantomime.Bool
corePatientP ss i ra ma =
  Pantomime.boolean $ corePatient (sysCore (sysOf ss i ra ma)) i

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("cacheArraysP", $(pantomime 'cacheArraysP)),
    ("coherentDataP", $(pantomime 'coherentDataP)),
    ("coherentWriteP", $(pantomime 'coherentWriteP)),
    ("coherentLayoutP", $(pantomime 'coherentLayoutP)),
    ("srvDriverMatchesSpecP", $(pantomime 'srvDriverMatchesSpecP)),
    ("srvDriverValuesP", $(pantomime 'srvDriverValuesP)),
    ("srvInvFunctionalP", $(pantomime 'srvInvFunctionalP)),
    ("baseCaseP", $(pantomime 'baseCaseP)),
    ("indStep0P", $(pantomime 'indStep0P)),
    ("indStep4P", $(pantomime 'indStep4P)),
    ("indStepWaits4P", $(pantomime 'indStepWaits4P)),
    ("indStep8P", $(pantomime 'indStep8P)),
    ("indStepWaits8P", $(pantomime 'indStepWaits8P)),
    ("srvReadyIffAnsweredP", $(pantomime 'srvReadyIffAnsweredP)),
    ("srvReadyUnderInvP", $(pantomime 'srvReadyUnderInvP)),
    ("corePatientP", $(pantomime 'corePatientP))
  ]
