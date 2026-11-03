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
    simCachedOf,
  )
where

import Memory.Cache4K (SetIdx)
import Proof.Memory.Cache4K.CachedCore.Functional.Machine (CachedSysG (..))
import Proof.Memory.Cache4K.Server.Leakage.Invariant (simOf, srvSimInvAt)
import Proof.Memory.Cache4K.CachedCore.Leakage.Simulator (SimCachedSys (..))

-- | The simulator agrees with the cached system at a hop boundary: the cache's
-- simulator agrees with the server at set @wsi@, as in the cache's leakage
-- proof ('Proof.Memory.Cache4K.Server.Leakage.Invariant.srvSimInvAt'), and the
-- simulator records that an answer has just arrived.
cachedSimInvAt :: SetIdx -> SimCachedSys -> CachedSysG r m -> Bool
cachedSimInvAt wsi (SimCachedSys sim answered) (CachedSys _ _ srv) =
  srvSimInvAt wsi sim srv && answered

-- | The simulator a cached system determines at the start of a hop: the
-- cache's simulator built from its server
-- ('Proof.Memory.Cache4K.Server.Leakage.Invariant.simOf'), and an answer in the
-- previous cycle, as at every hop boundary.
simCachedOf :: CachedSysG r m -> SimCachedSys
simCachedOf (CachedSys _ _ srv) = SimCachedSys (simOf srv) True
