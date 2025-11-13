# Simple build for WolfSSL crypto components needed for ECDSA SHA256
CC = riscv64-unknown-elf-gcc
CFLAGS = -march=rv32i -mabi=ilp32 -O2 -g -IwolfSSL/ -DWOLFSSL_USER_SETTINGS -Wno-missing-prototypes -Wno-error -Wno-unused-function -Wno-unused-variable

# Source files we need
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
	wolfSSL/wolfcrypt/src/integer.c \
	wolfSSL/wolfcrypt/src/wolfmath.c \
	wolfSSL/wolfcrypt/src/misc.c \
	wolfSSL/wolfcrypt/src/tfm.c \
	wolfSSL/wolfcrypt/src/sp_int.c \
	wolfSSL/wolfcrypt/src/sp_c32.c \
	./custom_rng.c \
	./getrandom.c

CRYPTO_OBJS = $(CRYPTO_SRCS:.c=.o)

# Target library
LIBWOLFSSL = libwolfssl_minimal.a

all: $(LIBWOLFSSL)

$(LIBWOLFSSL): $(CRYPTO_OBJS)
	riscv64-unknown-elf-ar rcs $@ $^

%.o: %.c
	cp wolfSSL_user_settings.h wolfSSL/user_settings.h
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(CRYPTO_OBJS) $(LIBWOLFSSL)

.PHONY: all clean