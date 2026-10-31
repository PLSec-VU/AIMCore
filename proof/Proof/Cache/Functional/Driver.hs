-- | The cache's driver: how many cycles after a request the cache's answer is
-- final -- the cache's counterpart of "Proof.Core.Functional.Driver".
--
--   * 'driver', a case table over the cache and the request, and
--   * 'driverRef', the operational reading, obtained by actually serving.
module Proof.Cache.Functional.Driver
  ( driver,
    driverRef,
  )
where

import Core (MemAccess (..), Request)
import Memory.Cache4K (cacheHit, setOf)
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Types

-- | The cycles after the request until the cache's answer is final: 'penalty'
-- for every line a data read misses, nothing for anything else. A read touches
-- the lines of its first and last byte; within four bytes those are different
-- lines exactly when they are in different sets. The driver is only ever run
-- from servers that satisfy the invariant
-- ('Proof.Cache.Functional.Invariant.srvInvAt'), which are ready, so the
-- server's state is not consulted; elsewhere it need not agree with
-- 'driverRef'.
driver :: CacheSrv m -> Request -> Cycles
driver (CacheSrv c _ _) req = case req of
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
  _ -> 0

-- | The operational reading of 'driver': serve the request, then serve without
-- one until the server is ready again, and count every cycle of the hop,
-- including the request's. So, from a server that satisfies the invariant,
-- @'driverRef' fuel srv0 req == Just ('driver' srv0 req + 1)@. 'Nothing' if the
-- hop takes more than @fuel@ cycles.
driverRef :: (MemOps m) => Int -> CacheSrv m -> Request -> Maybe Int
driverRef fuel srv0 req = go 1 (fst (serve srv0 req))
  where
    go n srv
      | srvState srv == CacheSrvReady = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (fst (serve srv Nothing))
