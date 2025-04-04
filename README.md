# uc-risc-v

A RISC-V core. Here's where the important files are:

```
.
├── src
│   ├── Core.hs                    -- The RISC-V core itself.
│   ├── HardwareSim.hs             -- Clash-based simulation for the core. (Broken.)
│   ├── Instruction.hs
│   ├── ISA.hs                     -- An interpreter for the ISA.
│   ├── Leak
│   │   ├── Arch
│   │   │   └── Arch.hs            -- Architectural leakage (Broken.)
│   │   ├── PC
│   │   │   ├── PC.hs              -- PC/Constant-time leakage.
│   │   │   ├── Sim.hs             -- Sim circuit for the leakage proof.
│   │   │   └── Time.hs            -- Time circuit for the leakage proof.
│   │   └── PCWithState
│   │       └── PCWithState.hs     -- PC/Constant-time leakage with internal memory/regfile. (Broken.)
│   ├── Pretty.hs
│   ├── Regfile.hs
│   ├── Simulate.hs                -- Haskell-based simulator for the core.
│   ├── Types.hs
│   └── Util.hs
├── test
│   └── Spec.hs                    -- Tests.
```
