-- | The obligations of link 2's leakage half, in one place: what the bus
-- between the core and the cache shows follows from what the bus between the
-- core and ideal memory shows -- the counterpart of
-- "Proof.Cache.CachedCore.Functional.Obligation".
--
-- One cycle of the core behind ideal memory, with observation @o@, corresponds
-- to @k + 1@ cycles of the core behind the cache: the first shows @o@ too, the
-- other @k@ show no request. The cache's simulator, given @o@ alone, computes
-- @k@ ('Proof.Cache.CachedCore.Leakage.Driver.simCachedDriver') and answers in
-- the same cycles as the server. Together with the core's leakage proof, which
-- gives the ideal observations from the ISA leakage, this gives the
-- observations of the core behind the cache from the ISA leakage, with no
-- combination of stalls to consider: the stalls are inserted one ideal cycle
-- at a time.
--
-- The steps build the cached system from the ideal system, as the functional
-- half does ('Proof.Cache.CachedCore.Functional.Obligation.cachedOf'), and the
-- simulator from the cached system ('simCachedOf'), as the cache's leakage
-- proof does from the server ('Proof.Cache.Server.Leakage.Invariant.simOf').
-- They assume link 2's functional invariant, which the functional half
-- establishes, and add only the simulator's. One step per hop length, as in
-- the functional half.
--
-- == Why they hold
--
-- The base case and the steps follow from the cache's leakage proof
-- ("Proof.Cache.Server.Leakage.Obligation"), the contract
-- ("Proof.Cache.Server.Functional.Contract") and link 2's functional half; they
-- are checked here only as a sanity check of that argument.
--
--   * Base case: the cache's simulator and the server at start agree
--     ('Proof.Cache.Server.Leakage.Obligation.baseCaseLeak'), and the core's
--     input at start holds an answer, as the simulator at start records. The
--     functional invariant at start is the functional half's base case.
--
--   * Step of length @k@: by the functional half, the first cycle's request is
--     the ideal one, and in the @k@ cycles after it the core makes no request
--     ('Proof.Cache.Server.Functional.Contract.corePatient'). In every cycle,
--     the simulator, given the observation of the request, answers exactly
--     when the server does and agrees with it afterwards
--     ('Proof.Cache.Server.Leakage.Obligation.indStepLeak', once per cycle).
--     The simulator's wait is the functional driver's: both are the cache's
--     drivers, which agree with their operational readings
--     ('Proof.Cache.Server.Functional.Contract.srvDriverMatchesSpec',
--     'Proof.Cache.Server.Leakage.Driver.simSrvDriverMatchesSpec').
module Proof.Cache.CachedCore.Leakage.Obligation
  ( baseCaseLeak,
    indStepLeak0,
    indStepLeak4,
    indStepLeak8,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.RegFile (RegFileOps)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Proof.Cache.CachedCore.Functional.Driver (cachedDriver)
import Proof.Cache.CachedCore.Functional.Invariant (cachedInvAt)
import Proof.Cache.CachedCore.Functional.Machine
  ( CachedSysG (..),
    initCachedSys,
    stepCachedOut,
  )
import Proof.Cache.CachedCore.Functional.Obligation (cachedOf)
import Proof.Cache.CachedCore.Leakage.Driver (simCachedDriver)
import Proof.Cache.CachedCore.Leakage.Invariant
  ( cachedSimInvAt,
    simCachedOf,
  )
import Proof.Cache.CachedCore.Leakage.Simulator
  ( SimCachedInput,
    SimCachedSys (..),
    simInitCachedSys,
    simStepCached,
  )
import Proof.Cache.Server.Functional.Obligation (CoherentOps (..))
import Proof.Cache.SMT.Raw (RawData (..))
import Proof.Core.Functional.Machine (SysG (..), stepSysOut)
import Proof.Core.Leakage.Model (Obs, obsOf)
import Types (Address)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The observations of a run, cycle by cycle: what the attacker sees over a
-- hop. A type of its own, rather than a list, so that its equality is derived
-- here and the solver can unroll it; the equality of lists comes from @base@,
-- whose definitions the solver cannot see.
data Trace = TraceSingle Obs | TraceCons Obs Trace
  deriving (Show, Eq)

-- The base case ----------------------------------------------------------------

-- | The base case: the simulator at start agrees with the core at start behind
-- the cache at start, in front of any memory. Link 2's functional invariant
-- at start is its functional base case
-- ('Proof.Cache.CachedCore.Functional.Obligation.baseCase'). A sanity check:
-- it follows from the cache's leakage base case (see the module header).
baseCaseLeak :: SetIdx -> Address -> r -> m -> Bool
baseCaseLeak wsi pc rf mem =
  cachedSimInvAt wsi simInitCachedSys (initCachedSys pc rf mem)

-- The inductive steps ----------------------------------------------------------

-- | The one-cycle hop: a request the cache answers at once. A sanity check: it
-- follows from the cache's leakage step and the contract (see the module
-- header).
indStepLeak0 ::
  (RegFileOps r, Eq r, CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> Bool
indStepLeak0 wa wsi wwi sys tags raw =
  not premises || conclusion
  where
    csys = cachedOf sys tags raw
    sim = simCachedOf csys

    (_, out) = stepSysOut sys
    obs = obsOf out

    hopLength = 0

    (tr1, csys') = stepCachedOutN hopLength csys
    (tr2, sim') = simStepCachedN hopLength sim (Just obs)

    premises =
      cachedInvAt wa wsi wwi sys csys
        && cachedSimInvAt wsi sim csys
        && cachedDriver csys == hopLength

    conclusion =
      simCachedDriver sim (Just obs) == hopLength
        && tr1 == tr2
        && cachedSimInvAt wsi sim' csys'

-- | The hop of a read that misses one line: 'penalty' + 1 cycle. A sanity
-- check: it follows from the cache's leakage step and the contract (see the
-- module header).
indStepLeak4 ::
  (RegFileOps r, Eq r, CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> Bool
indStepLeak4 wa wsi wwi sys tags raw =
  not premises || conclusion
  where
    csys = cachedOf sys tags raw
    sim = simCachedOf csys

    (_, out) = stepSysOut sys
    obs = obsOf out

    hopLength = fromIntegral penalty

    (tr1, csys') = stepCachedOutN hopLength csys
    (tr2, sim') = simStepCachedN hopLength sim (Just obs)

    premises =
      cachedInvAt wa wsi wwi sys csys
        && cachedSimInvAt wsi sim csys
        && cachedDriver csys == hopLength

    conclusion =
      simCachedDriver sim (Just obs) == hopLength
        && tr1 == tr2
        && cachedSimInvAt wsi sim' csys'

-- | The hop of a read that misses both its lines: 2 * 'penalty' + 1 cycle. A
-- sanity check: it follows from the cache's leakage step and the contract (see
-- the module header).
indStepLeak8 ::
  (RegFileOps r, Eq r, CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> TagArr -> RawData -> Bool
indStepLeak8 wa wsi wwi sys tags raw =
  not premises || conclusion
  where
    csys = cachedOf sys tags raw
    sim = simCachedOf csys

    (_, out) = stepSysOut sys
    obs = obsOf out

    hopLength = 2 * fromIntegral penalty

    (tr1, csys') = stepCachedOutN hopLength csys
    (tr2, sim') = simStepCachedN hopLength sim (Just obs)

    premises =
      cachedInvAt wa wsi wwi sys csys
        && cachedSimInvAt wsi sim csys
        && cachedDriver csys == hopLength

    conclusion =
      simCachedDriver sim (Just obs) == hopLength
        && tr1 == tr2
        && cachedSimInvAt wsi sim' csys'

-- Helpers ----------------------------------------------------------------------

-- | A hop of the cached system: @n + 1@ steps, the request's cycle and the @n@
-- cycles the driver counts after it ('stepCachedOut' repeated). Returns the
-- trace of the bus's observations and the system after the hop. Recursive on
-- @n@, which each step fixes to a literal.
stepCachedOutN ::
  (RegFileOps r, MemOps m) =>
  Int -> CachedSysG r m -> (Trace, CachedSysG r m)
stepCachedOutN n csys
  | n == 0 = (TraceSingle obs, csys')
  | otherwise =
    let (tr, csys'') = stepCachedOutN (n - 1) csys'
    in (TraceCons obs tr, csys'')
  where
    (csys', out) = stepCachedOut csys
    obs = obsOf out

-- | The simulator's counterpart of 'stepCachedOutN' ('simStepCached'
-- repeated): its input in the first cycle, then no input, as in the middle of
-- a driver hop. Returns the trace of what it shows on the bus and the
-- simulator after the hop.
simStepCachedN :: Int -> SimCachedSys -> SimCachedInput -> (Trace, SimCachedSys)
simStepCachedN n sim inp
  | n == 0 = (TraceSingle bus, sim')
  | otherwise =
    let (tr, sim'') = simStepCachedN (n - 1) sim' Nothing
    in (TraceCons bus tr, sim'')
  where
    (sim', bus) = simStepCached sim inp
