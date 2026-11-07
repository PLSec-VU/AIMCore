-- | The simulator of link 2's leakage half: what the bus between the core and
-- the cache shows, computed from what the bus between the core and ideal
-- memory shows -- the counterpart of
-- "Proof.Memory.Cache4K.CachedCore.Functional.Machine".
--
-- It runs cycle by cycle, as the core behind the cache does. In a cycle in
-- which the core does not wait, its input is the observation the core behind
-- ideal memory makes; while the core waits, it has no input. It carries only
-- the cache's simulator ("Proof.Memory.Cache4K.Server.Leakage.Simulator"),
-- which decides from the observations alone in which cycles the answer comes.
module Proof.Memory.Cache4K.CachedCore.Leakage.Simulator
  ( SimCachedInput,
    SimCachedSys (..),
    simInitCachedSys,
    simStepCached,
  )
where

import Data.Maybe (fromMaybe)
import Proof.Memory.Cache4K.Server.Leakage.Simulator
  ( SimCacheSrv,
    simInitCacheSrv,
    simServe,
  )
import Proof.Core.Leakage.Model (Obs (..))

-- | The simulator's input in one cycle: the observation of the core behind
-- ideal memory, or 'Nothing' while the core behind the cache waits.
--
-- 'Nothing' means the simulator is in the middle of a driver hop: the core
-- behind ideal memory has no cycle here, and its trace does not advance. It is
-- not the same as @'Just' 'NoAccess'@, a cycle of that core with no request,
-- which does advance it.
type SimCachedInput = Maybe Obs

-- | The simulator's counterpart of
-- 'Proof.Memory.Cache4K.CachedCore.Functional.Machine.CachedSysG': the cache's
-- simulator, and whether the simulator is ready to receive an input.
data SimCachedSys = SimCachedSys
  { -- | The cache's simulator.
    simCachedSrv :: SimCacheSrv,
    -- | Is the simulator ready to receive an input?
    simCachedReady :: Bool
  }
  deriving (Show, Eq)

-- | The simulator at start: the cache's simulator at start
-- ('Proof.Memory.Cache4K.Server.Leakage.Simulator.simInitCacheSrv'), ready to
-- receive an input.
simInitCachedSys :: SimCachedSys
simInitCachedSys = SimCachedSys simInitCacheSrv True

-- | One cycle of the core behind the cache, as
-- 'Proof.Memory.Cache4K.CachedCore.Functional.Machine.stepCachedOut'. If the
-- simulator is ready, it takes the ideal observation, shows it on the bus and
-- gives it to the cache's simulator; otherwise the bus shows no request and the
-- input is ignored. It is ready again once the cache's simulator answers.
-- Returns the simulator afterwards and the bus's observation.
simStepCached :: SimCachedSys -> SimCachedInput -> (SimCachedSys, Obs)
simStepCached (SimCachedSys sim ready) inp
  | ready =
      let obs = fromMaybe NoAccess inp
          (sim', ans) = simServe sim obs
      in (SimCachedSys sim' ans, obs)
  | otherwise =
      let (sim', ans) = simServe sim NoAccess
      in (SimCachedSys sim' ans, NoAccess)
