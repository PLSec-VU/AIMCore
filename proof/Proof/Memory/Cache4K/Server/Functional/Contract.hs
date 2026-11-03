-- | The contract between the core and the 4 KiB cache in front of memory: what
-- each side promises beyond the cache's proof against ideal memory
-- ("Proof.Memory.Cache4K.Server.Functional.Obligation"), so that the core runs
-- behind the cache as it would behind ideal memory, with stutter steps
-- inserted.
--
-- The two sides are not symmetric: the core makes the requests and the cache
-- answers them.
--
--   * The core waits for what it asked for ('corePatient'): after a request,
--     given no answer, it makes no request and its state stays the same.
--   * The cache answers 'Nothing' only while a request is outstanding. Ready
--     is defined by behaviour ('srvReady'): asked nothing, the server answers
--     within the cycle and stays the same. After a step, a server is ready
--     exactly when it has answered ('srvReadyIffAnswered'), a server related
--     to ideal memory by the invariant is ready ('srvReadyUnderInv'), and from
--     a ready server the driver agrees with its operational reading
--     ('srvDriverMatchesSpec').
--     While a request is outstanding, the driver and the cache's step
--     obligations say how it answers: 'Nothing' for as many cycles as the
--     driver counts, then ideal memory's answer.
--
-- Both sides are treated as opaque: states are compared whole, with '==',
-- not field by field.
--
-- The properties are checked in
-- "Proof.Memory.Cache4K.Server.Functional.Solver".
module Proof.Memory.Cache4K.Server.Functional.Contract
  ( srvReady,
    srvDriverSpec,
    srvReadyIffAnswered,
    srvReadyUnderInv,
    srvDriverMatchesSpec,
    corePatient,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.Pipeline (Input (..), Output (..), Request, StateG (..), circuit)
import Data.Maybe (isJust, isNothing)
import Memory.Cache4KServer (CacheSrv (..), serve)
import Memory.Types (MemOps)
import Proof.Memory.Cache4K.Server.Functional.Driver (srvDriver)
import Proof.Memory.Cache4K.Server.Functional.Invariant (srvInvAt)
import Core.RegFile (RegFileOps (..))
import Types (Address)
import Memory.Cache4K (SetIdx, WordIdx)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The cache's side --------------------------------------------------------------

-- | Is the server ready for a request? Defined by its behaviour, not by its
-- fields: asked nothing, it answers within the cycle and stays the same.
srvReady :: (MemOps m, Eq m) => CacheSrv m -> Bool
srvReady srv =
  let (srv', ans) = serve srv Nothing
  in srv' == srv && ans == Input (Just 0)

-- | After a step, the server is ready exactly when it has answered.
srvReadyIffAnswered :: (MemOps m, Eq m) => CacheSrv m -> Request -> Bool
srvReadyIffAnswered srv req =
  let (srv', ans) = serve srv req
  in srvReady srv' == isJust (inputMem ans)

-- | A server related to ideal memory by the invariant is ready.
srvReadyUnderInv ::
  (MemOps m, Eq m) =>
  Address -> SetIdx -> WordIdx -> m -> CacheSrv m -> Bool
srvReadyUnderInv wa wsi wwi ideal srv =
  not premises || conclusion
  where
    premises = srvInvAt wa wsi wwi ideal srv

    conclusion = srvReady srv

-- | How many cycles after a request the server answers, by serving: serve the
-- request, then serve without one until the server answers, and count the
-- cycles after the request's. 'Nothing' if more than @fuel@ cycles follow the
-- request's. The operational reading of a memory's driver, such as
-- 'Proof.Memory.Cache4K.Server.Functional.Driver.srvDriver'
-- ('srvDriverMatchesSpec').
srvDriverSpec :: (MemOps m) => Int -> CacheSrv m -> Request -> Maybe Int
srvDriverSpec fuel srv0 req =
  let (srv', ans) = serve srv0 req
  in go 0 srv' (isJust (inputMem ans))
  where
    go n srv answered
      | answered = Just n
      | n >= fuel = Nothing
      | otherwise =
        let (srv', ans) = serve srv Nothing
        in go (n + 1) srv' (isJust (inputMem ans))

-- | From a ready server ('srvReady'), the driver
-- ('Proof.Memory.Cache4K.Server.Functional.Driver.srvDriver') agrees with its
-- operational reading ('srvDriverSpec').
-- Eight cycles of fuel, 2 * 'penalty', cover the longest hop, two missed
-- lines, and no more, since Pantomime unrolls all of them. Readiness suffices,
-- unlike for the core ('Proof.Core.Functional.Driver.driverMatchesSpec'): the
-- driver only asks which lines hit, so every ready server is a valid start,
-- and the invariant, which implies readiness, would only narrow the statement.
srvDriverMatchesSpec :: (MemOps m, Eq m) => CacheSrv m -> Request -> Bool
srvDriverMatchesSpec srv req =
  not premises || conclusion
  where
    premises = srvReady srv

    conclusion = srvDriverSpec 8 srv req == Just (srvDriver srv req)

-- The core's side ---------------------------------------------------------------

-- | The core waits for what it asked for: after a request, given no answer, it
-- makes no request and stays where it was.
corePatient :: (RegFileOps r, Eq r) => StateG r -> Input -> Bool
corePatient s inp =
  case outputMem o of
    Nothing -> True
    Just _ -> s'' == s' && isNothing (outputMem o')
  where
    (s', o) = circuit s inp
    (s'', o') = circuit s' (Input Nothing)
