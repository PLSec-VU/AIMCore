-- | The obligations of link 2, in one place: the core behind the cache refines
-- the core behind ideal memory -- the counterpart of
-- "Proof.Core.Functional.Obligation".
--
-- One cycle of the core behind ideal memory ("Memory.Ideal") corresponds to
-- 'Proof.Cache.CachedCore.Functional.Driver.cachedDriver' + 1 cycles of the
-- core behind the cache. As the core's obligations build the ISA state from the
-- system ('Proof.Core.Functional.Invariant.isaOf'), these build the cached
-- system from the ideal one ('cachedOf'), with a cache that is coherent by
-- construction; the conclusion compares the two after the hop.
--
-- From a system that satisfies the invariant the driver is 0, 'penalty' or
-- twice 'penalty' (no, one or two lines missed;
-- 'Proof.Cache.CachedCore.Functional.Driver.cachedDriverValues'), so there is
-- one step per hop length, named after it for @'penalty' = 4@. Split by length
-- because Pantomime unrolls recursion only when its count is concrete.
--
-- == Why they hold
--
-- The base case and the steps follow from the standalone cache's proof
-- ("Proof.Cache.Server.Functional") and the contract between core and cache
-- ("Proof.Cache.Server.Functional.Contract"); they are checked here only as a
-- sanity check of that argument.
--
--   * Base case: the server at start satisfies the cache's invariant against
--     its memory ('Proof.Cache.Server.Functional.Obligation.baseCase'), and the
--     cores and inputs are the same by construction.
--
--   * Step of length @k@: the invariant ('cachedInvAt') gives the same core,
--     the same input, and the cache's invariant against ideal memory. In the
--     first cycle both cores therefore make the same request. The server is
--     ready ('Proof.Cache.Server.Functional.Contract.srvReadyUnderInv') and
--     answers it @k@ cycles later
--     ('Proof.Cache.Server.Functional.Contract.srvDriverMatchesSpec'), with
--     ideal memory's answer and its invariant again
--     ('Proof.Cache.Server.Functional.Obligation.indStep0',
--     'Proof.Cache.Server.Functional.Obligation.indStep4',
--     'Proof.Cache.Server.Functional.Obligation.indStep8'). In the @k@ cycles
--     in between it answers 'Nothing', and the core, given 'Nothing' after a
--     request, makes no request and stays the same
--     ('Proof.Cache.Server.Functional.Contract.corePatient', once per cycle),
--     so the server is served no request, as its own steps assume. After the
--     hop the cores and inputs are those of the ideal system after one step,
--     and the cache's invariant holds: 'cachedInvAt' again.
module Proof.Cache.CachedCore.Functional.Obligation
  ( cachedOf,
    baseCase,
    indStep0,
    indStep4,
    indStep8,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.RegFile (RegFileOps)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Proof.Cache.Server.Functional.Obligation (CoherentOps (..))
import Proof.Cache.SMT.Raw (RawData (..))
import Proof.Cache.CachedCore.Functional.Driver (cachedDriver)
import Proof.Cache.CachedCore.Functional.Invariant (cachedInvAt)
import Proof.Cache.CachedCore.Functional.Machine
  ( CachedSysG (..),
    initCachedSys,
    stepCached,
  )
import Proof.Core.Functional.Machine (SysG (..), initSys, stepSys)
import Types (Address)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The starting state -----------------------------------------------------------

-- | The cached system for the ideal system @sys@: the same core and input, and
-- in front of its memory a ready cache with the tag array @tags@ that is
-- coherent by construction -- valid lines hold memory's words, invalid lines
-- those of @raw@. The steps start from it, as the cache's own steps do
-- ('Proof.Cache.Server.Functional.Obligation.indStep0'), so that coherence
-- holds by construction instead of being assumed; it shows what the cache adds
-- to the ideal system: its tags and the contents of its invalid lines.
cachedOf :: (CoherentOps m) => SysG r m -> TagArr -> RawData -> CachedSysG r m
cachedOf (Sys core inp mem) tags raw =
  CachedSys core inp (CacheSrv cache mem CacheSrvReady)
  where
    cache = Cache4K tags (mkData mem tags raw)

-- The base case ----------------------------------------------------------------

-- | The base case: the core at start behind the cache at start, in front of
-- any memory, and the same core behind ideal memory satisfy the invariant. A
-- sanity check: it follows from the cache's base case (see the module header).
baseCase ::
  (Eq r, MemOps m) =>
  Address -> SetIdx -> WordIdx -> Address -> r -> m -> Bool
baseCase wa wsi wwi pc rf mem =
  cachedInvAt wa wsi wwi sys csys
  where
    sys = initSys pc rf mem
    csys = initCachedSys pc rf mem

-- The inductive steps ----------------------------------------------------------

-- | The one-cycle hop: a request the cache answers at once. A sanity check:
-- it follows from the cache's step and the contract (see the module header).
indStep0 ::
  (RegFileOps r, Eq r, CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> Bool
indStep0 wa wsi wwi sys tags raw =
  not premises || conclusion
  where
    sys' = stepSys sys

    csys = cachedOf sys tags raw
    csys' = stepCached csys

    premises =
      cachedInvAt wa wsi wwi sys csys
        && cachedDriver csys == 0

    conclusion = cachedInvAt wa wsi wwi sys' csys'

-- | The hop of a read that misses one line: 'penalty' + 1 cycle. A sanity
-- check: it follows from the cache's step and the contract (see the module
-- header).
indStep4 ::
  (RegFileOps r, Eq r, CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> Bool
indStep4 wa wsi wwi sys tags raw =
  not premises || conclusion
  where
    sys' = stepSys sys

    csys = cachedOf sys tags raw
    csys' = stepCachedN (fromIntegral penalty) csys

    premises =
      cachedInvAt wa wsi wwi sys csys
        && cachedDriver csys == fromIntegral penalty

    conclusion = cachedInvAt wa wsi wwi sys' csys'

-- | The hop of a read that misses both its lines: 2 * 'penalty' + 1 cycle. A
-- sanity check: it follows from the cache's step and the contract (see the
-- module header).
indStep8 ::
  (RegFileOps r, Eq r, CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> Bool
indStep8 wa wsi wwi sys tags raw =
  not premises || conclusion
  where
    sys' = stepSys sys

    csys = cachedOf sys tags raw
    csys' = stepCachedN (2 * fromIntegral penalty) csys

    premises =
      cachedInvAt wa wsi wwi sys csys
        && cachedDriver csys == 2 * fromIntegral penalty

    conclusion = cachedInvAt wa wsi wwi sys' csys'

-- Helpers ----------------------------------------------------------------------

-- | A hop of the cached system: @n + 1@ steps, the request's cycle and the @n@
-- cycles the driver counts after it. Recursive on @n@, which each step fixes
-- to a constant, so that the solver can unroll it.
stepCachedN ::
  (RegFileOps r, MemOps m) => Int -> CachedSysG r m -> CachedSysG r m
stepCachedN n csys
  | n == 0 = stepCached csys
  | otherwise = stepCachedN (n - 1) (stepCached csys)
