-- | The driver of the core behind the cache: how many cycles the cached system
-- ("Proof.Cache.Functional.Machine") takes beyond one cycle of the core with
-- ideal memory, before the two can be compared again -- the counterpart of
-- "Proof.Core.Functional.Driver".
--
--   * 'driver', a case table over the cache and the request the core makes, and
--   * 'driverRef', the operational reading, obtained by actually stepping.
module Proof.Cache.Functional.Driver
  ( driver,
    driverRef,
  )
where

import Core (MemAccess (..), Output (..))
import qualified Core
import Memory.Cache4K (cacheHit, setOf)
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Proof.Cache.Functional.Machine (CachedSysG (..), stepCached)
import Core.RegFile (RegFileOps)
import Types

-- | The cycles after this one until the cache has answered the request the
-- core makes in this cycle: 'penalty' for every line a data read misses,
-- nothing for anything else. A read touches the lines of its first and last
-- byte; within four bytes those are different lines exactly when they are in
-- different sets. The driver is only ever run from systems that satisfy the
-- invariant ('Proof.Cache.Functional.Invariant.cacheInvAt'), whose cache is
-- ready, so the cache's state is not consulted; elsewhere it need not agree
-- with 'driverRef'.
driver :: (RegFileOps r) => CachedSysG r m -> Cycles
driver (CachedSys core inp srv) = case req of
  Just (MemRead addr size) ->
    let off = case size of
          Byte -> 0
          Half -> 1
          Word -> 3

        hit1stLine = cacheHit addr c
        penalty1stLine = if hit1stLine then 0 else penalty

        hit2ndLine = setOf (addr + off) == setOf addr || cacheHit (addr + off) c
        penalty2ndLine = if hit2ndLine then 0 else penalty

    in penalty1stLine + penalty2ndLine
    where c = srvCache srv
  _ -> 0
  where (_, Output req) = Core.circuit core inp

-- | The operational reading of 'driver': step the cached system until the cache
-- is ready again, and count every cycle of the hop, including the first. So,
-- from a system that satisfies the invariant,
-- @'driverRef' fuel csys0 == Just ('driver' csys0 + 1)@. 'Nothing' if the hop
-- takes more than @fuel@ cycles.
driverRef :: (RegFileOps r, MemOps m) => Int -> CachedSysG r m -> Maybe Int
driverRef fuel csys0 = go 1 (stepCached csys0)
  where
    go n csys
      | isReady (srvState (cachedSrv csys)) = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (stepCached csys)

    isReady :: CacheSrvState -> Bool
    isReady CacheSrvReady = True
    isReady _ = False
