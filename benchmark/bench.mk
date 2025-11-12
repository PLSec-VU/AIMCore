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
CFLAGS = -march=rv32i -mabi=ilp32 -O3 -g -I$(INCLUDE_DIR) -fPIC
LDFLAGS = -march=rv32i -mabi=ilp32 -L$(LIB_DIR) -lsodium

# Benchmark sources and targets
BENCHMARKS = bench_chacha20 bench_x25519 bench_sha256 bench_blake2b bench_vuln_memcmp bench_sodium_memcmp bench_random bench_secure_memory
SOURCES = $(addsuffix .c, $(BENCHMARKS))
OBJECTS = $(addsuffix .o, $(BENCHMARKS))

# Secure memory library and getrandom
SECURE_MEMORY_OBJS = secure_memory.o getrandom.o

# Default target
all: $(BENCHMARKS)

# Build individual benchmarks
bench_chacha20: bench_chacha20.c secure_memory.o getrandom.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o getrandom.o $(LDFLAGS)

bench_x25519: bench_x25519.c secure_memory.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o $(LDFLAGS)

bench_sha256: bench_sha256.c secure_memory.o getrandom.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o getrandom.o $(LDFLAGS)

bench_blake2b: bench_blake2b.c secure_memory.o getrandom.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o getrandom.o $(LDFLAGS)

bench_vuln_memcmp: bench_vuln_memcmp.c secure_memory.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o $(LDFLAGS)

bench_sodium_memcmp: bench_sodium_memcmp.c secure_memory.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o $(LDFLAGS)

bench_random: bench_random.c secure_memory.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o $(LDFLAGS)

bench_secure_memory: bench_secure_memory.c secure_memory.o
	$(CC) $(CFLAGS) -o $@ $< secure_memory.o $(LDFLAGS)

# Build secure memory library
secure_memory.o: secure_memory.c secure_memory.h
	$(CC) $(CFLAGS) -c -o $@ $<

# Build getrandom library
getrandom.o: getrandom.c getrandom.h
	$(CC) $(CFLAGS) -c -o $@ $<

# Generic rule for all benchmarks
%: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

# Show objdump info for a benchmark
objdump-%: %
	$(OBJDUMP) -f $<

# Clean targets
clean:
	rm -f $(BENCHMARKS) $(OBJECTS) $(SECURE_MEMORY_OBJS)
	rm -f *.core

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