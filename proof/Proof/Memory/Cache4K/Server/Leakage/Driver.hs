-- | The cache simulator's driver: how many cycles after an observed request the
-- simulator answers -- the counterpart of
-- "Proof.Memory.Cache4K.Server.Functional.Driver" for
-- "Proof.Memory.Cache4K.Server.Leakage.Simulator".
--
-- The cache's leakage proof does not need it: the simulator runs cycle by
-- cycle. The proof of the core behind the cache does, to know how long a hop
-- takes from the simulators alone.
module Proof.Memory.Cache4K.Server.Leakage.Driver
  ( simSrvDriver,
    simSrvDriverSpec,
    simSrvDriverMatchesSpec,
  )
where

import Memory.Cache4K (setOf)
import Memory.Cache4KServer (penalty)
import Proof.Memory.Cache4K.Server.Leakage.Simulator
  ( SimCacheSrv (..),
    simServe,
    simSrvReady,
    tagHit,
  )
import Proof.Core.Leakage.Model (Obs (..))
import Types

-- | The cycles after the observed request until the simulator answers:
-- 'penalty' for every line a data read misses, nothing for anything else, as
-- 'Proof.Memory.Cache4K.Server.Functional.Driver.srvDriver' computes for the
-- server. Only ever run from a ready simulator, so its state is not consulted.
simSrvDriver :: SimCacheSrv -> Obs -> Int
simSrvDriver (SimCacheSrv t _) obs = case obs of
  DataRead addr size ->
    let off = case size of
          Byte -> 0
          Half -> 1
          Word -> 3

        hit1stLine = tagHit addr t
        penalty1stLine = if hit1stLine then 0 else fromIntegral penalty

        hit2ndLine = setOf (addr + off) == setOf addr || tagHit (addr + off) t
        penalty2ndLine = if hit2ndLine then 0 else fromIntegral penalty

    in penalty1stLine + penalty2ndLine
  _ -> 0

-- | The operational reading of 'simSrvDriver', as
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvDriverSpec' is of the
-- server's: step on the observed request, then with no input until the
-- simulator answers, and count the cycles after the request's. 'Nothing' if
-- more than
-- @fuel@ cycles follow the request's.
simSrvDriverSpec :: Int -> SimCacheSrv -> Obs -> Maybe Int
simSrvDriverSpec fuel sim0 obs =
  let (sim', ans) = simServe sim0 obs
  in go 0 sim' ans
  where
    go n sim answered
      | answered = Just n
      | n >= fuel = Nothing
      | otherwise =
        let (sim', ans) = simServe sim NoAccess
        in go (n + 1) sim' ans

-- | From a ready simulator ('simSrvReady'), its driver agrees with its
-- operational reading, as
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvDriverMatchesSpec' for
-- the server. Eight cycles of fuel, 2 * 'penalty', cover the longest hop, two
-- missed lines. Readiness suffices, as for the server: the driver only asks
-- which lines hit.
simSrvDriverMatchesSpec :: SimCacheSrv -> Obs -> Bool
simSrvDriverMatchesSpec sim obs =
  not premises || conclusion
  where
    premises = simSrvReady sim

    conclusion = simSrvDriverSpec 8 sim obs == Just (simSrvDriver sim obs)
