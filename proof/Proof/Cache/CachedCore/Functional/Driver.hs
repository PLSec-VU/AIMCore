-- | The driver of link 2: how many cycles the core behind the cache
-- ("Proof.Cache.CachedCore.Functional.Machine") takes beyond one cycle of the
-- core behind ideal memory, before the two can be compared again -- the
-- counterpart of "Proof.Cache.Server.Functional.Driver".
--
--   * 'cachedDriver', the server's driver on the request the core makes, and
--   * 'cachedDriverSpec', the operational reading, obtained by actually
--     stepping.
module Proof.Cache.CachedCore.Functional.Driver
  ( cachedDriver,
    cachedDriverValues,
    cachedDriverSpec,
    cachedDriverMatchesSpec,
  )
where

import Core (Input (..), Output (..))
import qualified Core
import Core.RegFile (RegFileOps)
import Data.Maybe (isJust)
import Memory.Cache4K (SetIdx, WordIdx)
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Proof.Cache.CachedCore.Functional.Invariant (cachedInvAt)
import Proof.Cache.CachedCore.Functional.Machine (CachedSysG (..), stepCached)
import Proof.Cache.Server.Functional.Driver (srvDriver)
import Proof.Core.Functional.Machine (SysG)
import Types

-- | The cycles after this one until the cache has answered the request the
-- core makes in this cycle: the server's driver
-- ('Proof.Cache.Server.Functional.Driver.srvDriver') on that request. The
-- driver is only ever run from systems that satisfy the invariant
-- ('Proof.Cache.CachedCore.Functional.Invariant.cachedInvAt'), whose cache is
-- ready; elsewhere it need not agree with 'cachedDriverSpec'.
cachedDriver :: (RegFileOps r) => CachedSysG r m -> Int
cachedDriver (CachedSys core inp srv) =
  let (_, Output req) = Core.circuit core inp
  in srvDriver srv req

-- | The driver is 0, 'penalty' or twice 'penalty': the hop lengths of the
-- steps ('Proof.Cache.CachedCore.Functional.Obligation.indStep0',
-- 'Proof.Cache.CachedCore.Functional.Obligation.indStep4',
-- 'Proof.Cache.CachedCore.Functional.Obligation.indStep8'), which so cover
-- every hop. Follows from the server's
-- ('Proof.Cache.Server.Functional.Driver.srvDriverValues'), since
-- 'cachedDriver' is the server's driver on the core's request.
cachedDriverValues :: (RegFileOps r) => CachedSysG r m -> Bool
cachedDriverValues csys =
  k == 0 || k == fromIntegral penalty || k == 2 * fromIntegral penalty
  where
    k = cachedDriver csys

-- | The operational reading of 'cachedDriver': step the system until the cache
-- answers the core, and count the cycles after the first. 'Nothing' if more
-- than @fuel@ cycles follow the first.
cachedDriverSpec ::
  (RegFileOps r, MemOps m) => Int -> CachedSysG r m -> Maybe Int
cachedDriverSpec fuel csys0 = go 0 (stepCached csys0)
  where
    go n csys
      | isJust (inputMem (cachedInput csys)) = Just n
      | n >= fuel = Nothing
      | otherwise = go (n + 1) (stepCached csys)

-- | On systems the invariant relates to an ideal system, the driver agrees with
-- its operational reading, as 'Proof.Core.Functional.Driver.driverMatchesSpec'
-- for the core: the driver reads the core's request, which only matches the
-- stepping on states the invariant covers. Eight cycles of fuel, 2 * 'penalty',
-- cover the longest hop, two missed lines. Follows from the server's
-- ('Proof.Cache.Server.Functional.Contract.srvDriverMatchesSpec'): under the
-- invariant the server is ready
-- ('Proof.Cache.Server.Functional.Contract.srvReadyUnderInv'), and while it
-- has not answered, the core makes no request
-- ('Proof.Cache.Server.Functional.Contract.corePatient'), so the server is
-- stepped exactly as 'Proof.Cache.Server.Functional.Contract.srvDriverSpec'
-- steps it.
cachedDriverMatchesSpec ::
  (RegFileOps r, Eq r, MemOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> CachedSysG r m -> Bool
cachedDriverMatchesSpec wa wsi wwi sys csys =
  not premises || conclusion
  where
    premises = cachedInvAt wa wsi wwi sys csys

    conclusion = cachedDriverSpec 8 csys == Just (cachedDriver csys)
