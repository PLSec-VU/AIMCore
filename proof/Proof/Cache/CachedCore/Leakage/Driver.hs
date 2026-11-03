-- | The driver of link 2's leakage half: how many cycles the core behind the
-- cache waits after a request, computed from the request's observation alone
-- -- the counterpart of "Proof.Cache.CachedCore.Functional.Driver".
module Proof.Cache.CachedCore.Leakage.Driver
  ( simCachedDriver,
    simCachedDriverSpec,
    simCachedDriverMatchesSpec,
  )
where

import Data.Maybe (fromMaybe)
import Proof.Cache.Server.Leakage.Driver (simSrvDriver)
import Proof.Cache.CachedCore.Leakage.Simulator
  ( SimCachedInput,
    SimCachedSys (..),
    simStepCached,
  )
import Proof.Cache.Server.Leakage.Simulator (simSrvReady)
import Proof.Core.Leakage.Model (Obs (..))

-- | The cycles the core waits after a request observed as @inp@: the cache
-- simulator's driver ('Proof.Cache.Server.Leakage.Driver.simSrvDriver') on that
-- observation. Only ever run from a ready simulator, so whether the last cycle
-- brought an answer is not consulted.
simCachedDriver :: SimCachedSys -> SimCachedInput -> Int
simCachedDriver (SimCachedSys sim _) inp =
  simSrvDriver sim (fromMaybe NoAccess inp)

-- | The operational reading of 'simCachedDriver', as
-- 'Proof.Cache.Server.Leakage.Driver.simSrvDriverSpec' is of the cache
-- simulator's: step on the observed request, then with no input until the
-- simulator answers, and count the cycles after the request's. 'Nothing' if
-- more than @fuel@ cycles follow the request's.
simCachedDriverSpec :: Int -> SimCachedSys -> SimCachedInput -> Maybe Int
simCachedDriverSpec fuel sim0 inp = go 0 (fst (simStepCached sim0 inp))
  where
    go n sim
      | simCachedAnswered sim = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (fst (simStepCached sim Nothing))

-- | From a ready simulator -- its last cycle brought an answer, and the cache's
-- simulator is ready ('Proof.Cache.Server.Leakage.Simulator.simSrvReady') --
-- its driver agrees with its operational reading, for the observation of any
-- request: a hop starts with one, never in the middle of a driver hop. Eight
-- cycles of fuel, 2 * 'penalty', cover the longest hop.
--
-- Follows from the cache simulator's
-- ('Proof.Cache.Server.Leakage.Driver.simSrvDriverMatchesSpec'): since the last
-- cycle brought an answer, 'simStepCached' passes the input on to the cache's
-- simulator, and until it answers again passes it no input, so the cache's
-- simulator is stepped exactly as
-- 'Proof.Cache.Server.Leakage.Driver.simSrvDriverSpec' steps it, and
-- 'simCachedDriver' is its driver.
simCachedDriverMatchesSpec :: SimCachedSys -> Obs -> Bool
simCachedDriverMatchesSpec sim@(SimCachedSys srvSim answered) obs =
  not premises || conclusion
  where
    premises = answered && simSrvReady srvSim

    conclusion =
      simCachedDriverSpec 8 sim (Just obs)
        == Just (simCachedDriver sim (Just obs))
