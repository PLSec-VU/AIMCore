-- | The cache as a machine: how the cache's proof runs the server -- the
-- cache's counterpart of "Proof.Core.Functional.Machine". The server itself,
-- 'Memory.Cache4KServer.serve', is part of the design and lives in src.
module Proof.Cache.Functional.Machine
  ( serveN,
  )
where

import Core (Input, Request)
import Memory.Cache4KServer
import Memory.Types (MemOps)

-- | Serve a request, then @n@ cycles without one: the cache's counterpart of
-- 'Proof.Core.Functional.Machine.stepSysN'. With @n = 'Proof.Cache.Functional.Driver.driver'@
-- this is the request's whole hop, ending with the cache's final answer.
serveN :: (MemOps m) => Int -> CacheSrv m -> Request -> (CacheSrv m, Input)
serveN n srv req = go n (serve srv req)
  where
    go k (y', x)
      | k <= 0 = (y', x)
      | otherwise = go (k - 1) (serve y' Nothing)
