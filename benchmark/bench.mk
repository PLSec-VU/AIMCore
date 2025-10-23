# RISC-V Libsodium Benchmarks Makefile

# RISC-V toolchain configuration (32-bit using 64-bit toolchain)
RISCV_PREFIX = riscv64-unknown-elf
CC = $(RISCV_PREFIX)-gcc
OBJDUMP = $(RISCV_PREFIX)-objdump

# Paths
INSTALL_PREFIX = $(PWD)/install-riscv
INCLUDE_DIR = $(INSTALL_PREFIX)/include
LIB_DIR = $(INSTALL_PREFIX)/lib

# Compiler flags (32-bit RISC-V)
CFLAGS = -march=rv32i -mabi=ilp32 -O2 -g -I$(INCLUDE_DIR)
LDFLAGS = -march=rv32i -mabi=ilp32 -L$(LIB_DIR) -lsodium

# Benchmark sources and targets
BENCHMARKS = bench_chacha20 bench_x25519 bench_sha256 bench_blake2b
SOURCES = $(addsuffix .c, $(BENCHMARKS))
OBJECTS = $(addsuffix .o, $(BENCHMARKS))

# Default target
all: $(BENCHMARKS)

# Build individual benchmarks
bench_chacha20: bench_chacha20.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

bench_x25519: bench_x25519.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

bench_sha256: bench_sha256.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

bench_blake2b: bench_blake2b.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

# Generic rule for all benchmarks
%: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

# Show objdump info for a benchmark
objdump-%: %
	$(OBJDUMP) -f $<

# Clean targets
clean:
	rm -f $(BENCHMARKS) $(OBJECTS)

# Show configuration
show-config:
	@echo "RISC-V Benchmark Configuration:"
	@echo "  CC:           $(CC)"
	@echo "  CFLAGS:       $(CFLAGS)"
	@echo "  LDFLAGS:      $(LDFLAGS)"
	@echo "  INCLUDE_DIR:  $(INCLUDE_DIR)"
	@echo "  LIB_DIR:      $(LIB_DIR)"
	@echo "  BENCHMARKS:   $(BENCHMARKS)"

# Check if libsodium is built
check-deps:
	@if [ ! -f "$(LIB_DIR)/libsodium.a" ]; then \
		echo "Error: libsodium not found. Run 'make install' first."; \
		exit 1; \
	fi
	@echo "Dependencies OK"

.PHONY: all clean show-config check-deps