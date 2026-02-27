# Simple build for WolfSSL crypto components for RV32I (No-M Extension)
CC = riscv64-unknown-elf-gcc
AR = riscv64-unknown-elf-ar

# march=rv32i ensures no hardware multiplication instructions are generated
CFLAGS_BASE = -march=rv32i -mabi=ilp32 -g \
              -I. -IwolfSSL/ \
              -DWOLFSSL_USER_SETTINGS \
              -Wno-missing-prototypes -Wno-error -Wno-unused-function -Wno-unused-variable

CFLAGS_O0 = $(CFLAGS_BASE) -O0
CFLAGS_O3 = $(CFLAGS_BASE) -O3

# Optimized Source List for SP Math (Constant-Time)
# Removed 'integer.c' and 'tfm.c' as they are replaced by SP math for ECDSA
CRYPTO_SRCS = \
  wolfSSL/wolfcrypt/src/sha256.c \
  wolfSSL/wolfcrypt/src/ecc.c \
  wolfSSL/wolfcrypt/src/asn.c \
  wolfSSL/wolfcrypt/src/coding.c \
  wolfSSL/wolfcrypt/src/random.c \
  wolfSSL/wolfcrypt/src/hash.c \
  wolfSSL/wolfcrypt/src/hmac.c \
  wolfSSL/wolfcrypt/src/memory.c \
  wolfSSL/wolfcrypt/src/wc_port.c \
  wolfSSL/wolfcrypt/src/error.c \
  wolfSSL/wolfcrypt/src/logging.c \
  wolfSSL/wolfcrypt/src/wolfmath.c \
  wolfSSL/wolfcrypt/src/misc.c \
  wolfSSL/wolfcrypt/src/sp_int.c \
  wolfSSL/wolfcrypt/src/sp_c32.c \
  ./custom_rng.c \
  ./getrandom.c

CRYPTO_OBJS_O0 = $(CRYPTO_SRCS:.c=_O0.o)
CRYPTO_OBJS_O3 = $(CRYPTO_SRCS:.c=_O3.o)

LIBWOLFSSL_O0 = libwolfssl_minimal_O0.a
LIBWOLFSSL_O3 = libwolfssl_minimal_O3.a

all: prep $(LIBWOLFSSL_O0) $(LIBWOLFSSL_O3)

# Fix: Perform the copy once before starting compilation to avoid race conditions
prep:
	@cp wolfSSL_user_settings.h wolfSSL/user_settings.h

$(LIBWOLFSSL_O0): $(CRYPTO_OBJS_O0)
	$(AR) rcs $@ $^

$(LIBWOLFSSL_O3): $(CRYPTO_OBJS_O3)
	$(AR) rcs $@ $^

# Pattern rule for O0
%_O0.o: %.c
	$(CC) $(CFLAGS_O0) -c -o $@ $<

# Pattern rule for O3
%_O3.o: %.c
	$(CC) $(CFLAGS_O3) -c -o $@ $<

clean:
	rm -f $(CRYPTO_OBJS_O0) $(CRYPTO_OBJS_O3) $(LIBWOLFSSL_O0) $(LIBWOLFSSL_O3)
	rm -f wolfSSL/user_settings.h

.PHONY: all clean prep