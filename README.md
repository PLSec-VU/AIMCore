# uc-risc-v

A RISC-V core for Pantomime case study.

## Building

This project uses git submodules for dependencies. To clone and build:

```bash
git clone --recurse-submodules git@github.com:PLSec-VU/uc-risc-v.git
cd uc-risc-v
stack build
```

If you've already cloned without submodules, initialize them:

```bash
git submodule update --init --recursive
```

## Usage

The main executable can run RISC-V ELF binaries with various analysis modes:

```bash
# Run a standard RISC-V executable
stack run -- benchmark/bench_chacha20

# Run with verbose output (shows PC and instruction trace)
stack run -- --verbose benchmark/bench_blake2b

# Run with secure memory tracking (PubSec mode)
stack run -- --secure-memory benchmark/bench_secure_memory

# Output leakage traces to a file
stack run -- --leakage-output traces.txt benchmark/bench_x25519

# Run test suite
stack test
```

## Project Structure

```
.
├── app
│   └── Main.hs                    -- Main executable with CLI interface
├── src
│   ├── Core.hs                    -- The RISC-V core itself
│   ├── HardwareSim.hs             -- Clash-based simulation for the core (Broken)
│   ├── Instruction.hs             -- Instruction definitions
│   ├── ISA.hs                     -- An interpreter for the ISA
│   ├── Access.hs                  -- Security access control types
│   ├── Leak
│   │   ├── Existence.hs           -- Construct simulator from non-interference proof
│   │   ├── Arch
│   │   │   └── Arch.hs            -- Architectural leakage (Broken)
│   │   ├── PC
│   │   │   ├── PC.hs              -- PC/Constant-time leakage analysis
│   │   │   ├── Sim.hs             -- Sim circuit for the leakage proof
│   │   │   ├── Leak.hs            -- Leakage model for PC analysis
│   │   │   ├── ISA.hs             -- ISA-level leakage definitions
│   │   └── SecretPC
│   │       ├── Leak.hs            -- Leakage model for secret PC analysis
│   │       └── PC.hs              -- Secret PC leakage analysis with formal verification
│   ├── Elf
│   │   ├── ElfLoader.hs           -- ELF file loading and parsing
│   │   ├── Memory.hs              -- Memory management (standard and secure)
│   │   └── Syscall.hs             -- System call handling
│   ├── Pretty.hs                  
│   ├── RegFile.hs                 
│   ├── Simulate.hs                -- Haskell-based simulator for the core
│   ├── Types.hs                   -- Core type definitions
│   └── Util.hs                    -- Utility functions
├── test
│   ├── Spec.hs                    -- Main test suite
│   ├── BenchmarkSpec.hs           -- Tests on actual software
├── benchmark/                     -- Cryptographic benchmarks
│   ├── libsodium/                 -- Libsodium submodule for crypto primitives
│   └── *.c                        -- Various programs to run on the core
└── uc-plugin/                     -- Pantomime verification plugin submodule
```

## Note

Currently, only a very limited number of syscalls are supported. You can do `exit`, `printf`,
open and read from `/dev/{u}random`.