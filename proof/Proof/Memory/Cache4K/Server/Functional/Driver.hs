-- | The cache's driver: how many cycles after a request the cache's answer is
-- final -- the cache's counterpart of "Proof.Core.Functional.Driver".
--
-- 'srvDriver' is a case table over the cache and the request. Its operational
-- reading, obtained by actually serving, makes sense for any memory and so
-- lives in the contract
-- ('Proof.Memory.Cache4K.Server.Functional.Contract.srvDriverSpec'), together
-- with the property that the two agree.
module Proof.Memory.Cache4K.Server.Functional.Driver
  ( srvDriver,
    srvDriverValues,
  )
where

import Core.Pipeline (MemAccess (..), Request)
import Memory.Cache4K (cacheHit, setOf)
import Memory.Cache4KServer
import Types

-- | The cycles after the request until the cache's answer is final: 'penalty'
-- for every line a data read misses, nothing for anything else. A read touches
-- the lines of its first and last byte; within four bytes those are different
-- lines exactly when they are in different sets. The driver is only ever run
-- from servers that satisfy the invariant
-- ('Proof.Memory.Cache4K.Server.Functional.Invariant.srvInvAt'), which are
-- ready, so the server's state is not consulted; elsewhere it need not agree
-- with 'srvDriverSpec'.
srvDriver :: CacheSrv m -> Request -> Int
srvDriver (CacheSrv c _ _) req = case req of
  Just (MemRead addr size) ->
    let off = case size of
          Byte -> 0
          Half -> 1
          Word -> 3

        hit1stLine = cacheHit addr c
        penalty1stLine = if hit1stLine then 0 else fromIntegral penalty

        hit2ndLine = setOf (addr + off) == setOf addr || cacheHit (addr + off) c
        penalty2ndLine = if hit2ndLine then 0 else fromIntegral penalty

    in penalty1stLine + penalty2ndLine
  _ -> 0

-- | The driver is 0, 'penalty' or twice 'penalty': the hop lengths of the
-- cache's steps ('Proof.Memory.Cache4K.Server.Functional.Obligation.indStep0',
-- 'Proof.Memory.Cache4K.Server.Functional.Obligation.indStep4',
-- 'Proof.Memory.Cache4K.Server.Functional.Obligation.indStep8'), which so cover
-- every hop.
srvDriverValues :: CacheSrv m -> Request -> Bool
srvDriverValues srv req =
  k == 0 || k == fromIntegral penalty || k == 2 * fromIntegral penalty
  where
    k = srvDriver srv req
