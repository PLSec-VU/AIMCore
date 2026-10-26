{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The system the cache's proofs reason about: the core together with the
-- cache in front of memory -- the counterpart of
-- "Proof.Core.Functional.Machine", with the cache server ('serve') in place of
-- ideal memory ('Memory.Ideal.idealServe').
--
-- A single system step is one 'Core.circuit' step plus the cache server's
-- answer, and the state the proofs reason about is the triple
--
-- > (Core.State, Input, cache server)
module Proof.Cache.Functional.Machine
  ( CachedSysG (..),
    CachedSys,
    stepCached,
    stepCachedOut,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Memory.Cache4KServer
import Memory.Types
import Core.RegFile
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Core state, the input it is about to consume, and the cache server.
data CachedSysG r m = CachedSys
  { cachedCore :: Core.StateG r,
    cachedInput :: Input,
    cachedSrv :: CacheSrv m
  }

-- | The concrete system the QuickCheck harness runs: @Vec@-backed throughout.
type CachedSys = CachedSysG RegFile MemBytes

deriving instance (Show r, Show m) => Show (CachedSysG r m)

-- | Structural equality. 'CacheSrv' has no 'Eq' instance, so we compare its
-- fields directly.
instance (Eq r, Eq m) => Eq (CachedSysG r m) where
  CachedSys s1 i1 y1 == CachedSys s2 i2 y2 =
    s1 == s2
      && i1 == i2
      && srvCache y1 == srvCache y2
      && srvMem y1 == srvMem y2
      && srvState y1 == srvState y2

-- | One system step: run the pipeline for a cycle, then let the cache server
-- answer whatever memory access it emitted.
stepCached :: (RegFileOps r, MemOps m) => CachedSysG r m -> CachedSysG r m
stepCached = fst . stepCachedOut

-- | 'stepCached', but also returning the pipeline's 'Output' for that cycle.
--
-- The system is the core in a loop with 'serve': the core's request goes to the
-- cache, and the cache's answer is the core's input in the next cycle.
stepCachedOut :: (RegFileOps r, MemOps m) => CachedSysG r m -> (CachedSysG r m, Output)
stepCachedOut (CachedSys core inp srv) =
  let (core', o) = Core.circuit core inp
      (srv', inp') = serve srv (outMem o)
   in (CachedSys core' inp' srv', o)
