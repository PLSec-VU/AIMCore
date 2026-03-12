# RISC-V Cryptographic Benchmarks

This directory contains a collection of cryptographic RISC-V programs.

## Prerequisites

### RISC-V Toolchain

You need a RISC-V cross-compilation toolchain:
#### Installing RISC-V Toolchain

**MacOS:**
```bash
brew tap riscv-software-src/riscv
brew install riscv-tools
```

### Build
```bash
# Build libsodium for RISC-V
make -f sodium_build.mk all

# Build WolfSSL libraries
./wolfSSL_build.sh

# Build the programs
make all
```
