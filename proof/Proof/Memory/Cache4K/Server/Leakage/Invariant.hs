-- | The invariant of the cache's leakage proof: the simulator's tags are the
-- server's, at a witness set, and both wait for memory alike -- the leakage
-- counterpart of "Proof.Memory.Cache4K.Server.Functional.Invariant".
module Proof.Memory.Cache4K.Server.Leakage.Invariant
  ( srvSimInvAt,
    simOf,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Memory.Cache4K
import Memory.Cache4KServer (CacheSrv (..), CacheSrvState (..))
import Proof.Memory.Cache4K.Server.Leakage.Simulator
  ( SimCacheSrv (..),
    SimCacheSrvState (..),
  )
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The simulator and the server agree at set @wsi@: their tags are equal
-- there, and they are in the same state ('sameSrvState').
srvSimInvAt :: SetIdx -> SimCacheSrv -> CacheSrv m -> Bool
srvSimInvAt wsi (SimCacheSrv tags sst) (CacheSrv c _ st) =
  tagRead wsi tags == tagRead wsi (tagArr c)
    && sameSrvState sst st

-- | Both are ready, or both wait for memory with the same number of cycles
-- left. The word the server will answer is not compared: the simulator does
-- not know it.
sameSrvState :: SimCacheSrvState -> CacheSrvState -> Bool
sameSrvState SimCacheSrvReady CacheSrvReady = True
sameSrvState (SimCacheSrvStall n) (CacheSrvStall m _) = n == m
sameSrvState _ _ = False

-- | The simulator a server determines: its tags, and its state without the
-- word it will answer. The simulator the invariant relates to the server, as
-- 'Proof.Core.Functional.Invariant.isaOf' is the ISA state the core's
-- invariant relates to the system.
simOf :: CacheSrv m -> SimCacheSrv
simOf (CacheSrv c _ st) = SimCacheSrv (tagArr c) simSt
  where
    simSt = case st of
      CacheSrvReady -> SimCacheSrvReady
      CacheSrvStall n _ -> SimCacheSrvStall n
