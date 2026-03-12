#!/bin/bash
set -e

cd wolfSSL
cp ../wolfSSL_user_settings.h user_settings.h

make distclean || true
rm -f wolfcrypt/src/*.o
rm -f wolfcrypt/src/*_O0.o
rm -f wolfcrypt/src/*_O3.o

# Define your toolchain and base flags
# We use --enable-sp-math-all to ensure all ECC functions are present
COMMON_FLAGS="--host=riscv64-unknown-elf \
              CC=riscv64-unknown-elf-gcc \
              --enable-sp --enable-sp-math --enable-staticmemory \
              --enable-ecc --enable-sha256 --disable-crypttests \
              --disable-asn --disable-examples --disable-benchmark \
              --disable-shared --enable-static --disable-filesystem \
              --disable-rsa --disable-dh --disable-dsa --enable-cryptonly"

USER_SETTINGS="-DWOLFSSL_USER_SETTINGS"

# Build O0 version
mkdir -p build-O0 && cd build-O0
../configure $COMMON_FLAGS CFLAGS="-march=rv32i -mabi=ilp32 -O0 -g $USER_SETTINGS"
make 
cp ./src/.libs/libwolfssl.a ../libwolfssl_rv32i_O0.a
cd ..

# Build O3 version
mkdir -p build-O3 && cd build-O3
../configure $COMMON_FLAGS CFLAGS="-march=rv32i -mabi=ilp32 -O3 -g $USER_SETTINGS"
make 
cp ./src/.libs/libwolfssl.a ../libwolfssl_rv32i_O3.a
cd ..

echo "Build complete: libwolfssl_rv32i_O0.a and libwolfssl_rv32i_O3.a"
cp libwolfssl_rv32i_O0.a ../libwolfssl_minimal_O0.a
cp libwolfssl_rv32i_O3.a ../libwolfssl_minimal_O3.a