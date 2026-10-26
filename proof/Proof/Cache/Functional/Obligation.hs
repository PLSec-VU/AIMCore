{-# LANGUAGE InstanceSigs #-}
-- | The cache's proof obligations, in one place: the cache's counterpart of
-- "Proof.Core.Functional.Obligation".
--
-- They relate the cached memory to ideal memory ("Memory.Ideal") one request at
-- a time: ideal memory answers in one cycle, the cached memory in
-- 'Proof.Cache.Functional.Driver.driver' + 1 cycles. As the core's obligations
-- build the ISA state from the system ('Proof.Core.Functional.Obligation.isaAt'),
-- these take ideal memory to be the server's memory before the hop; the
-- conclusion compares the memory ideal memory steps to with the server's after
-- the hop.
--
-- From a server that satisfies the invariant the driver is 0, 'penalty' or
-- twice 'penalty' (no, one or two lines missed), so there is one step
-- obligation per hop length, named after it for @'penalty' = 4@. Split by
-- length because Pantomime unrolls recursion only when its count is concrete.
module Proof.Cache.Functional.Obligation
  ( CoherentOps (..),
    mkDataMem,
    cachedOf,
    baseCaseObligation,
    indStepObligation0,
    indStepObligation4,
    indStepObligation8,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Proof.Cache.Functional.Driver (driver)
import Proof.Cache.Functional.Invariant (cacheInvAt)
import Proof.Cache.Functional.Machine (CachedSysG (..), stepCached)
import Proof.Cache.SMT.Raw (RawData (..))
import Proof.Core.Functional.Machine (SysG (..), stepSys)
import Proof.SMT.Array (MemArr)
import Core.RegFile (RegFileOps)
import Types (Address, RegIdx, Word)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Memories a coherent cache can be built from. A class, as memory's
-- operations are ('MemOps'), so that the obligations stay polymorphic in memory
-- and the solver only meets the monomorphic 'mkDataMem'.
class (MemOps m) => CoherentOps m where
  -- | The data array of a coherent cache with tags @t@ in front of @mem@: a
  -- valid line holds memory's words at the address its tag gives, an invalid
  -- line the words of @raw@.
  mkData :: m -> TagArr -> RawData -> DataArr

instance CoherentOps MemArr where
  mkData :: MemArr -> TagArr -> RawData -> DataArr
  mkData = mkDataMem

-- | 'mkData' on the solver's memory. The solver does not run this body:
-- 'Proof.Cache.SMT.Axioms.coherentAxioms' replaces it by
-- 'Proof.Cache.SMT.Coherent.mkDataC'.
{-# OPAQUE mkDataMem #-}
mkDataMem :: MemArr -> TagArr -> RawData -> DataArr
mkDataMem mem t (RawData raw) = DataArr (imap word raw)
  where
    word :: Index 1024 -> Word -> Word
    word i w
      | entryValid e = memReadWord (wordAddr (entryTag e) si wi 0) mem
      | otherwise = w
      where
        idx = fromIntegral i :: BitVector 10
        si = slice d9 d2 idx
        wi = slice d1 d0 idx
        e = tagRead si t

-- | The cached system for the ideal system @sys@: the same core and input, and
-- in front of its memory a cache in state @st@ with tags @t@ that is coherent
-- by construction -- valid lines hold memory's words, invalid lines those of
-- @raw@. The obligations start from it, so that coherence holds by
-- construction instead of being assumed, and show what the cache adds to the
-- ideal system: its tags, the contents of its invalid lines, and its state.
cachedOf :: (CoherentOps m) => SysG r m -> TagArr -> RawData -> CacheSrvState -> CachedSysG r m
cachedOf (Sys core inp mem) t raw st =
  CachedSys core inp (CacheSrv (Cache4K t (mkData mem t raw)) mem st)

-- | A hop of the cached system: @n + 1@ steps, the request's cycle and the @n@
-- cycles the driver counts after it. Recursive on @n@, which each obligation
-- fixes to a constant, so that the solver can unroll it.
stepCachedSysN :: (RegFileOps r, MemOps m) => Cycles -> CachedSysG r m -> CachedSysG r m
stepCachedSysN n csys
  | n == 0 = stepCached csys
  | otherwise = stepCachedSysN (n - 1) (stepCached csys)

-- | The base case: the server at start, in front of any memory, satisfies the
-- invariant against that memory.
baseCaseObligation ::
  (RegFileOps r, MemOps m) => Address -> r -> m -> RegIdx -> Address -> SetIdx -> WordIdx -> Bool
baseCaseObligation pc rf mem wr wa wsi wwi =
  cacheInvAt wr wa wsi wwi sys csys
  where
    core = Core.initAt pc rf
    inp = Core.initInput
    sys = Sys core inp mem
    csys = CachedSys core inp (initCacheSrv mem)

-- | The one-cycle hop: a request the cache answers at once.
indStepObligation0 ::
  (RegFileOps r, CoherentOps m) =>
  RegIdx -> Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> CacheSrvState -> Bool
indStepObligation0 wr wa wsi wwi sys t raw st =
  not premises || conclusion
  where
    sys' = stepSys sys

    csys = cachedOf sys t raw st
    csys' = stepCachedSysN 0 csys

    premises =
      cacheInvAt wr wa wsi wwi sys csys
        && driver csys == 0

    conclusion = cacheInvAt wr wa wsi wwi sys' csys'

-- | The hop of a read that misses one line: 'penalty' + 1 cycle.
indStepObligation4 ::
  (RegFileOps r, CoherentOps m) =>
  RegIdx -> Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> CacheSrvState -> Bool
indStepObligation4 wr wa wsi wwi sys t raw st =
  not premises || conclusion
  where
    sys' = stepSys sys

    csys = cachedOf sys t raw st
    csys' = stepCachedSysN penalty csys

    premises =
      cacheInvAt wr wa wsi wwi sys csys
        && driver csys == penalty

    conclusion = cacheInvAt wr wa wsi wwi sys' csys'

-- | The hop of a read that misses both its lines: 2 * 'penalty' + 1 cycle.
indStepObligation8 ::
  (RegFileOps r, CoherentOps m) =>
  RegIdx -> Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> CacheSrvState -> Bool
indStepObligation8 wr wa wsi wwi sys t raw st =
  not premises || conclusion
  where
    sys' = stepSys sys

    csys = cachedOf sys t raw st
    csys' = stepCachedSysN (2 * penalty) csys

    premises =
      cacheInvAt wr wa wsi wwi sys csys
        && driver csys == 2 * penalty

    conclusion = cacheInvAt wr wa wsi wwi sys' csys'
