-- | The cache's leakage obligations: the simulator
-- ("Proof.Memory.Cache4K.Server.Leakage.Simulator"), which sees only the
-- observations of the requests, answers in the same cycles as the server.
--
-- The simulator runs cycle by cycle, as the server does, so there is no driver
-- and one step obligation, for one cycle, from any state of the server: ready
-- or waiting for memory.
--
-- As in the functional proof, the simulator is built from the server ('simOf'),
-- as the core's ISA state is built from the system
-- ('Proof.Core.Functional.Invariant.isaOf'): a simulator that satisfies the
-- invariant at every set is the one built from the server. The conclusion
-- compares the two after the cycle, at a witness set.
module Proof.Memory.Cache4K.Server.Leakage.Obligation
  ( baseCaseLeak,
    indStepLeak,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.Pipeline (Request)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types (MemOps)
import Proof.Memory.Cache4K.Server.Leakage.Invariant (simOf, srvSimInvAt)
import Proof.Memory.Cache4K.Server.Leakage.Model (obsAns, obsReq)
import Proof.Memory.Cache4K.Server.Leakage.Simulator
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The base case ----------------------------------------------------------------

-- | The base case: the server at start, in front of any memory, and the
-- simulator at start agree.
baseCaseLeak :: SetIdx -> m -> Bool
baseCaseLeak wsi mem = srvSimInvAt wsi simInitCacheSrv (initCacheSrv mem)

-- The inductive step -----------------------------------------------------------

-- | The step: in one cycle, the simulator, given the observation of the
-- request, answers exactly when the server does, and the two agree afterwards.
indStepLeak ::
  (MemOps m) =>
  SetIdx -> CacheSrv m -> Request -> Bool
indStepLeak wsi srv req =
  not premises || conclusion
  where
    sim = simOf srv

    (srv', ans) = serve srv req
    (sim', simAns) = simServe sim (obsReq req)

    premises = srvSimInvAt wsi sim srv

    conclusion =
      simAns == obsAns ans
        && srvSimInvAt wsi sim' srv'
