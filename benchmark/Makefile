# Makefile for building libsodium for RISC-V
# This Makefile cross-compiles libsodium using RISC-V toolchain

# Configuration variables
RISCV_PREFIX ?= riscv64-unknown-elf
RISCV_TOOLCHAIN_PATH ?= /opt/homebrew
LIBSODIUM_DIR = libsodium
BUILD_DIR = build-riscv
INSTALL_PREFIX = $(PWD)/install-riscv

# Toolchain configuration
CC = $(RISCV_TOOLCHAIN_PATH)/bin/$(RISCV_PREFIX)-gcc
CXX = $(RISCV_TOOLCHAIN_PATH)/bin/$(RISCV_PREFIX)-g++
AR = $(RISCV_TOOLCHAIN_PATH)/bin/$(RISCV_PREFIX)-ar
RANLIB = $(RISCV_TOOLCHAIN_PATH)/bin/$(RISCV_PREFIX)-ranlib
STRIP = $(RISCV_TOOLCHAIN_PATH)/bin/$(RISCV_PREFIX)-strip

# Build flags
CFLAGS ?= -O2 -g
CPPFLAGS ?=
LDFLAGS ?=

# Number of parallel jobs
NPROC = $(shell nproc 2>/dev/null || echo 4)

# Default target
.PHONY: all
all: libsodium

# Help target
.PHONY: help
help:
	@echo "Makefile for building libsodium for RISC-V"
	@echo ""
	@echo "Available targets:"
	@echo "  all              - Build libsodium (default)"
	@echo "  libsodium        - Build libsodium"
	@echo "  configure        - Configure libsodium for RISC-V"
	@echo "  autogen          - Run autogen.sh to generate configure script"
	@echo "  clean            - Clean build artifacts"
	@echo "  distclean        - Clean all generated files"
	@echo "  install          - Install libsodium to INSTALL_PREFIX"
	@echo "  test             - Run libsodium tests (requires RISC-V emulator)"
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Configuration variables:"
	@echo "  RISCV_PREFIX     - RISC-V toolchain prefix (default: $(RISCV_PREFIX))"
	@echo "  RISCV_TOOLCHAIN_PATH - Path to RISC-V toolchain (default: $(RISCV_TOOLCHAIN_PATH))"
	@echo "  INSTALL_PREFIX   - Installation prefix (default: $(INSTALL_PREFIX))"
	@echo "  CFLAGS           - C compiler flags (default: $(CFLAGS))"
	@echo ""
	@echo "Example usage:"
	@echo "  make RISCV_TOOLCHAIN_PATH=/usr/local/riscv"
	@echo "  make RISCV_PREFIX=riscv32-unknown-elf"

# Check if toolchain exists
.PHONY: check-toolchain
check-toolchain:
	@if [ ! -x "$(CC)" ]; then \
		echo "Error: RISC-V toolchain not found at $(CC)"; \
		echo "Please set RISCV_TOOLCHAIN_PATH to the correct path"; \
		echo "Current setting: RISCV_TOOLCHAIN_PATH=$(RISCV_TOOLCHAIN_PATH)"; \
		exit 1; \
	fi
	@echo "Using RISC-V toolchain: $(CC)"

# Run autogen.sh to generate configure script
.PHONY: autogen
autogen:
	@echo "Running autogen.sh..."
	cd $(LIBSODIUM_DIR) && ./autogen.sh

# Configure libsodium for RISC-V cross-compilation
.PHONY: configure
configure: check-toolchain autogen
	@echo "Configuring libsodium for RISC-V..."
	@mkdir -p $(BUILD_DIR)
	cd $(LIBSODIUM_DIR) && \
	./configure \
		--host=$(RISCV_PREFIX) \
		--prefix=$(INSTALL_PREFIX) \
		--disable-shared \
		--enable-static \
		--disable-pie \
		--disable-soname-versions \
		CC="$(CC)" \
		CXX="$(CXX)" \
		AR="$(AR)" \
		RANLIB="$(RANLIB)" \
		STRIP="$(STRIP)" \
		CFLAGS="$(CFLAGS)" \
		CPPFLAGS="$(CPPFLAGS)" \
		LDFLAGS="$(LDFLAGS)"

# Build libsodium
.PHONY: libsodium
libsodium: configure
	@echo "Building libsodium for RISC-V..."
	cd $(LIBSODIUM_DIR) && $(MAKE) -j$(NPROC)

# Install libsodium
.PHONY: install
install: libsodium
	@echo "Installing libsodium to $(INSTALL_PREFIX)..."
	cd $(LIBSODIUM_DIR) && $(MAKE) install
	@echo "Installation complete!"
	@echo "Headers: $(INSTALL_PREFIX)/include"
	@echo "Library: $(INSTALL_PREFIX)/lib/libsodium.a"

# Run tests (requires RISC-V emulator like QEMU)
.PHONY: test
test: libsodium
	@echo "Running libsodium tests..."
	@echo "Note: This requires a RISC-V emulator (e.g., qemu-riscv64) in your PATH"
	cd $(LIBSODIUM_DIR) && $(MAKE) check

# Clean build artifacts
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	-cd $(LIBSODIUM_DIR) && $(MAKE) clean
	-rm -rf $(BUILD_DIR)

# Clean all generated files
.PHONY: distclean
distclean:
	@echo "Cleaning all generated files..."
	-cd $(LIBSODIUM_DIR) && $(MAKE) distclean
	-rm -rf $(BUILD_DIR) $(INSTALL_PREFIX)
	-cd $(LIBSODIUM_DIR) && rm -f configure config.h.in aclocal.m4
	-cd $(LIBSODIUM_DIR) && rm -rf autom4te.cache build-aux m4/libtool.m4 m4/lt*.m4

# Show current configuration
.PHONY: show-config
show-config:
	@echo "Current configuration:"
	@echo "  RISCV_PREFIX:        $(RISCV_PREFIX)"
	@echo "  RISCV_TOOLCHAIN_PATH: $(RISCV_TOOLCHAIN_PATH)"
	@echo "  CC:                  $(CC)"
	@echo "  INSTALL_PREFIX:      $(INSTALL_PREFIX)"
	@echo "  CFLAGS:              $(CFLAGS)"
	@echo "  NPROC:               $(NPROC)"

# Minimal build (smaller library with fewer features)
.PHONY: minimal
minimal: check-toolchain autogen
	@echo "Configuring libsodium for RISC-V (minimal build)..."
	cd $(LIBSODIUM_DIR) && \
	./configure \
		--host=$(RISCV_PREFIX) \
		--prefix=$(INSTALL_PREFIX) \
		--disable-shared \
		--enable-static \
		--enable-minimal \
		--disable-pie \
		--disable-soname-versions \
		CC="$(CC)" \
		CXX="$(CXX)" \
		AR="$(AR)" \
		RANLIB="$(RANLIB)" \
		STRIP="$(STRIP)" \
		CFLAGS="$(CFLAGS)" \
		CPPFLAGS="$(CPPFLAGS)" \
		LDFLAGS="$(LDFLAGS)"
	@echo "Building minimal libsodium for RISC-V..."
	cd $(LIBSODIUM_DIR) && $(MAKE) -j$(NPROC)

# Debug build
.PHONY: debug
debug:
	$(MAKE) libsodium CFLAGS="-O0 -g3 -DDEBUG"

# Release build with optimizations
.PHONY: release
release:
	$(MAKE) libsodium CFLAGS="-O3 -DNDEBUG -flto"

# Show libsodium version
.PHONY: version
version:
	@cd $(LIBSODIUM_DIR) && grep "AC_INIT" configure.ac | head -1