-- | The leakage proof obligations, in one place.
--
-- The leakage theorem: an attacker watching the memory bus learns nothing
-- beyond the ISA-level leakage. Stated as a simulation: the simulator, which
-- sees only 'leakOf' of the ISA state, produces the same bus observation as the
-- core on every cycle, and takes the same number of cycles per instruction.
--
-- Each inductive step assumes the functional invariant ('invAtFree') as a
-- premise. It pins the core's execute stage to the ISA's next instruction, so
-- feeding the simulator that instruction's leakage keeps the two machines in
-- step. The functional proof supplies the invariant for the next hop, so it is
-- not re-proved here. 'simInv' carries the rest from hop to hop.
module Proof.Leakage.Obligation
  ( baseCaseLeakObligation,
    indStepLeakObligation,
    indStepLeakObligation1,
    indStepLeakObligation2,
    indStepLeakObligation3
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Memory.Types (MemOps (..))
import Proof.Functional.Driver
import Proof.Functional.Invariant (invAtFree)
import Proof.Functional.Obligation
import Proof.Leakage.Driver
import Proof.Leakage.Invariant
import Proof.Leakage.Model
import Proof.Leakage.Simulator
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The base case: from the initial state, the core and the simulator agree
-- on the first hop.
--
-- Both start from their initial states with the same start PC. Over the
-- two-cycle first hop, in which no instruction is executed, so the simulator
-- gets no leakage, the simulator's driver gives the same hop length as the
-- core's, the two make the same bus observations, and 'simInv' holds
-- afterwards. The register file and memory are arbitrary, as in
-- 'Proof.Functional.Obligation.baseCaseObligation'.
baseCaseLeakObligation ::
  (RegFileOps r, MemOps m) => r Identity -> m -> Address -> Bool
baseCaseLeakObligation rf mem pc =
  simDriver Nothing sim == 1
    && obs1 == obsOf out1
    && obs2 == obsOf out2
    && simInv sim2 sys2
  where
    sys = Sys (Core.initAt pc rf) Core.initInput mem
    sim = simInit {simStateFePc = pc}

    (sys1, out1) = stepSysOut sys
    (sim1, obs1) = simCircuit sim Nothing

    (sys2, out2) = stepSysOut sys1
    (sim2, obs2) = simCircuit sim1 Nothing

-- | The @k = 0@ leakage step: the one-cycle hop.
--
-- > inv(isa, sys) /\ simInv(sim, sys) /\ driver(sys) = 0
-- >   ==>  simDriver(leak, sim) = 0 /\ obs(sim) = obs(sys) /\ simInv(sim', sys')
--
-- The simulator gets @leakOf isa@ on the hop's first cycle and nothing after.
indStepLeakObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> SimState -> Bool
indStepLeakObligation wr wa ipc sys sim =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    leak = leakOf isa

    (sys', out') = stepSysOut sys
    (sim', obs') = simCircuit sim (Just leak)

    premises =
      invAtFree wr wa isa sys
        && driver sys == 0
        && simInv sim sys

    conclusion =
      simDriver (Just leak) sim == 0
        && obs' == obsOf out'
        && simInv sim' sys'

-- | The @k = 1@ leakage step: the two-cycle hop.
indStepLeakObligation1 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> SimState -> Bool
indStepLeakObligation1 wr wa ipc sys sim =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    leak = leakOf isa

    (sys1, out1) = stepSysOut sys
    (sim1, obs1) = simCircuit sim (Just leak)

    (sys2, out2) = stepSysOut sys1
    (sim2, obs2) = simCircuit sim1 Nothing

    premises =
      invAtFree wr wa isa sys
        && driver sys == 1
        && simInv sim sys

    conclusion =
      simDriver (Just leak) sim == 1
        && obs1 == obsOf out1
        && obs2 == obsOf out2
        && simInv sim2 sys2

-- | The @k = 2@ leakage step: the three-cycle hop.
indStepLeakObligation2 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> SimState -> Bool
indStepLeakObligation2 wr wa ipc sys sim =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    leak = leakOf isa

    (sys1, out1) = stepSysOut sys
    (sim1, obs1) = simCircuit sim (Just leak)

    (sys2, out2) = stepSysOut sys1
    (sim2, obs2) = simCircuit sim1 Nothing

    (sys3, out3) = stepSysOut sys2
    (sim3, obs3) = simCircuit sim2 Nothing

    premises =
      invAtFree wr wa isa sys
        && driver sys == 2
        && simInv sim sys

    conclusion =
      simDriver (Just leak) sim == 2
        && obs1 == obsOf out1
        && obs2 == obsOf out2
        && obs3 == obsOf out3
        && simInv sim3 sys3

-- | The @k = 3@ leakage step: the four-cycle hop, the longest.
indStepLeakObligation3 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> Address -> SysG r m -> SimState -> Bool
indStepLeakObligation3 wr wa ipc sys sim =
  not premises || conclusion
  where
    isa = isaAt ipc sys
    leak = leakOf isa

    (sys1, out1) = stepSysOut sys
    (sim1, obs1) = simCircuit sim (Just leak)

    (sys2, out2) = stepSysOut sys1
    (sim2, obs2) = simCircuit sim1 Nothing

    (sys3, out3) = stepSysOut sys2
    (sim3, obs3) = simCircuit sim2 Nothing

    (sys4, out4) = stepSysOut sys3
    (sim4, obs4) = simCircuit sim3 Nothing

    premises =
      invAtFree wr wa isa sys
        && driver sys == 3
        && simInv sim sys

    conclusion =
      simDriver (Just leak) sim == 3
        && obs1 == obsOf out1
        && obs2 == obsOf out2
        && obs3 == obsOf out3
        && obs4 == obsOf out4
        && simInv sim4 sys4
