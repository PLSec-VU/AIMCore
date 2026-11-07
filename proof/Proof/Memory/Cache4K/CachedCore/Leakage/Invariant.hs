-- | The invariant of link 2's leakage half: the cache's simulator agrees with
-- the server of the core behind the cache -- the counterpart of
-- "Proof.Memory.Cache4K.CachedCore.Functional.Invariant".
--
-- The steps assume link 2's functional invariant
-- ('Proof.Memory.Cache4K.CachedCore.Functional.Invariant.cachedInvAt') as well,
-- so this only adds the simulator
-- ("Proof.Memory.Cache4K.CachedCore.Leakage.Simulator").
module Proof.Memory.Cache4K.CachedCore.Leakage.Invariant
  ( cachedSimInvAt,
    cachedSimInvFunctional,
    simCachedOf,
  )
where

import Memory.Cache4K (SetIdx, tagRead)
import Proof.Memory.Cache4K.CachedCore.Functional.Machine (CachedSysG (..))
import Proof.Memory.Cache4K.Server.Leakage.Invariant (simOf, srvSimInvAt)
import Proof.Memory.Cache4K.CachedCore.Leakage.Simulator (SimCachedSys (..))
import Proof.Memory.Cache4K.Server.Leakage.Simulator (SimCacheSrv (..))

-- | The simulator agrees with the cached system at a hop boundary: the cache's
-- simulator agrees with the server at set @wsi@, as in the cache's leakage
-- proof ('Proof.Memory.Cache4K.Server.Leakage.Invariant.srvSimInvAt'), and the
-- simulator is ready to receive an input.
cachedSimInvAt :: SetIdx -> SimCachedSys -> CachedSysG r m -> Bool
cachedSimInvAt wsi (SimCachedSys sim ready) (CachedSys _ _ srv) =
  srvSimInvAt wsi sim srv && ready

-- | The invariant is functional: two simulators it relates to the same cached
-- system agree at set @wsi@: the cache's simulators have the same tag there
-- and the same state, and both are ready. Quantifying over @wsi@ gives
-- equality of the simulators. Follows from the cache's simulator invariant
-- being functional
-- ('Proof.Memory.Cache4K.Server.Leakage.Invariant.srvSimInvFunctional').
cachedSimInvFunctional ::
  SetIdx -> SimCachedSys -> SimCachedSys -> CachedSysG r m -> Bool
cachedSimInvFunctional wsi sim1 sim2 csys =
  not premises || conclusion
  where
    premises = cachedSimInvAt wsi sim1 csys && cachedSimInvAt wsi sim2 csys
    srvSim1 = simCachedSrv sim1
    srvSim2 = simCachedSrv sim2
    conclusion =
      tagRead wsi (simTagArr srvSim1) == tagRead wsi (simTagArr srvSim2)
        && simSrvState srvSim1 == simSrvState srvSim2
        && simCachedReady sim1 == simCachedReady sim2

-- | The simulator a cached system determines at the start of a hop: the
-- cache's simulator built from its server
-- ('Proof.Memory.Cache4K.Server.Leakage.Invariant.simOf'), ready to receive an
-- input, as at every hop boundary.
simCachedOf :: CachedSysG r m -> SimCachedSys
simCachedOf (CachedSys _ _ srv) = SimCachedSys (simOf srv) True
