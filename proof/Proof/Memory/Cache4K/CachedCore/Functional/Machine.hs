{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The system link 2 reasons about: the core together with the cache in front
-- of memory -- the counterpart of
-- "Proof.Core.Functional.Machine", with the cache server ('serve') in place of
-- ideal memory ('Memory.Ideal.idealServe').
--
-- A single system step is one 'Core.circuit' step plus the cache server's
-- answer, and the state the proofs reason about is the triple
--
-- > (Core.State, Input, cache server)
module Proof.Memory.Cache4K.CachedCore.Functional.Machine
  ( CachedSysG (..),
    CachedSys,
    initCachedSys,
    stepCached,
    stepCachedOut,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.Pipeline as Core
import Memory.Cache4KServer
import Memory.Types
import Core.RegFile
import Types (Address)
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

deriving instance (Eq r, Eq m) => Eq (CachedSysG r m)

-- | The system at start: the core at start with PC @pc@ and register file @rf@
-- ('Core.initAt'), its input at start ('Core.initInput'), and the cache server
-- at start in front of memory @mem@ ('Memory.Cache4KServer.initCacheSrv').
initCachedSys :: Address -> r -> m -> CachedSysG r m
initCachedSys pc rf mem =
  CachedSys (Core.initAt pc rf) Core.initInput (initCacheSrv mem)

-- | One system step: run the pipeline for a cycle, then let the cache server
-- answer whatever memory access it emitted.
stepCached :: (RegFileOps r, MemOps m) => CachedSysG r m -> CachedSysG r m
stepCached = fst . stepCachedOut

-- | 'stepCached', but also returning the pipeline's 'Output' for that cycle.
--
-- The system is the core in a loop with 'serve': the core's request goes to the
-- cache, and the cache's answer is the core's input in the next cycle.
stepCachedOut ::
  (RegFileOps r, MemOps m) => CachedSysG r m -> (CachedSysG r m, Output)
stepCachedOut (CachedSys core inp srv) =
  let (core', o) = Core.circuit core inp
      (srv', inp') = serve srv (outputMem o)
   in (CachedSys core' inp' srv', o)
