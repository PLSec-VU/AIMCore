-- | The leakage proof obligations, in one place.
--
-- The leakage theorem: an attacker watching the memory bus learns nothing
-- beyond the ISA-level leakage. Stated as a simulation: the simulator, which
-- sees only 'leakOf' of the ISA state, produces the same bus observation as the
-- core on every cycle, and takes the same number of cycles per instruction.
--
-- Each inductive step assumes the functional invariant ('invAt') as a
-- premise. It pins the core's execute stage to the ISA's next instruction, so
-- feeding the simulator that instruction's leakage keeps the two machines in
-- step. The functional proof supplies the invariant for the next hop, so it is
-- not re-proved here. 'simInv' carries the rest from hop to hop.
module Proof.Core.Leakage.Obligation
  ( baseCaseLeak,
    indStepLeak0,
    indStepLeak1,
    indStepLeak2,
    indStepLeak3
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Memory.Types (MemOps (..))
import Proof.Core.Functional.Driver
import Proof.Core.Functional.Invariant (invAt, isaOf)
import Proof.Core.Leakage.Driver
import Proof.Core.Leakage.Invariant
import Proof.Core.Leakage.Model
import Proof.Core.Leakage.Simulator
import Proof.Core.Functional.Machine
import Core.RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The observations of a run, cycle by cycle: what the attacker sees over a
-- hop. A type of its own, rather than a list, so that its equality is derived
-- here and the solver can unroll it; the equality of lists comes from @base@,
-- whose definitions the solver cannot see.
data Trace = TraceSingle Obs | TraceCons Obs Trace
  deriving (Show, Eq)

-- The base case ----------------------------------------------------------------

-- | The base case: from the initial state, the core and the simulator agree
-- on the first hop.
--
-- Both start from their initial states with the same start PC. Over the
-- two-cycle first hop, in which no instruction is executed, so the simulator
-- gets no leakage, the simulator's driver gives the same hop length as the
-- core's, the two make the same bus observations, and 'simInv' holds
-- afterwards. The register file and memory are arbitrary, as in
-- 'Proof.Core.Functional.Obligation.baseCase'.
baseCaseLeak ::
  (RegFileOps r, MemOps m) => Address -> r -> m -> Bool
baseCaseLeak pc rf mem =
  simDriver Nothing sim == 1
    && tr1 == tr2
    && simInv sim' sys'
  where
    sys = initSys pc rf mem
    sim = simInit {simStateFePc = pc}

    (tr1, sys') = stepSysOutN 1 sys
    (tr2, sim') = simCircuitN 1 sim Nothing

-- The inductive steps ----------------------------------------------------------

-- | The @k = 0@ leakage step: the one-cycle hop.
--
-- > inv(isa, sys) /\ simInv(sim, sys) /\ driver(sys) = 0
-- >   ==>  simDriver(leak, sim) = 0 /\ obs(sim) = obs(sys)
-- >          /\ simInv(sim', sys')
--
-- The simulator gets @leakOf isa@ on the hop's first cycle and nothing after.
indStepLeak0 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> SimState -> Bool
indStepLeak0 wr wa sys sim =
  not premises || conclusion
  where
    isa = isaOf sys
    leak = leakOf isa

    hopLength = 0

    (tr1, sys') = stepSysOutN hopLength sys
    (tr2, sim') = simCircuitN hopLength sim (Just leak)

    premises =
      invAt wr wa isa sys
        && simInv sim sys
        && driver sys == hopLength

    conclusion =
      simDriver (Just leak) sim == hopLength
        && tr1 == tr2
        && simInv sim' sys'

-- | The @k = 1@ leakage step: the two-cycle hop.
indStepLeak1 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> SimState -> Bool
indStepLeak1 wr wa sys sim =
  not premises || conclusion
  where
    isa = isaOf sys
    leak = leakOf isa

    hopLength = 1

    (tr1, sys') = stepSysOutN hopLength sys
    (tr2, sim') = simCircuitN hopLength sim (Just leak)

    premises =
      invAt wr wa isa sys
        && simInv sim sys
        && driver sys == hopLength

    conclusion =
      simDriver (Just leak) sim == hopLength
        && tr1 == tr2
        && simInv sim' sys'

-- | The @k = 2@ leakage step: the three-cycle hop.
indStepLeak2 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> SimState -> Bool
indStepLeak2 wr wa sys sim =
  not premises || conclusion
  where
    isa = isaOf sys
    leak = leakOf isa

    hopLength = 2

    (tr1, sys') = stepSysOutN hopLength sys
    (tr2, sim') = simCircuitN hopLength sim (Just leak)

    premises =
      invAt wr wa isa sys
        && simInv sim sys
        && driver sys == hopLength

    conclusion =
      simDriver (Just leak) sim == hopLength
        && tr1 == tr2
        && simInv sim' sys'

-- | The @k = 3@ leakage step: the four-cycle hop, the longest.
indStepLeak3 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> SimState -> Bool
indStepLeak3 wr wa sys sim =
  not premises || conclusion
  where
    isa = isaOf sys
    leak = leakOf isa

    hopLength = 3

    (tr1, sys') = stepSysOutN hopLength sys
    (tr2, sim') = simCircuitN hopLength sim (Just leak)

    premises =
      invAt wr wa isa sys
        && simInv sim sys
        && driver sys == hopLength

    conclusion =
      simDriver (Just leak) sim == hopLength
        && tr1 == tr2
        && simInv sim' sys'

-- Helpers ----------------------------------------------------------------------

-- | A hop of the system: @n + 1@ steps ('stepSysOut' repeated). Returns the
-- trace of its bus observations and the system after the hop. Recursive on
-- @n@, which each step fixes to a literal.
stepSysOutN ::
  (RegFileOps r, MemOps m) => Int -> SysG r m -> (Trace, SysG r m)
stepSysOutN n sys
  | n == 0 = (TraceSingle obs, sys')
  | otherwise =
    let (tr, sys'') = stepSysOutN (n - 1) sys'
    in (TraceCons obs tr, sys'')
  where
    (sys', out) = stepSysOut sys
    obs = obsOf out

-- | The simulator's counterpart of 'stepSysOutN' ('simCircuit' repeated): its
-- input in the first cycle, then no input, as in the middle of a driver hop.
-- Returns the trace of its bus observations and the simulator after the hop.
simCircuitN :: Int -> SimState -> SimInput -> (Trace, SimState)
simCircuitN n sim inp
  | n == 0 = (TraceSingle obs, sim')
  | otherwise =
    let (tr, sim'') = simCircuitN (n - 1) sim' Nothing
    in (TraceCons obs tr, sim'')
  where
    (sim', obs) = simCircuit sim inp
