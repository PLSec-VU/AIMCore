#include <string.h>
#include <stdio.h>
#include <sodium.h>

#define SIZE 256

void populate_random_bytes(char *buf, size_t len) {
    // NOTE: randombytes_uniform is NOT used because
    // libsodium will use entropy to seed its PRNG internally,
    // which generates a non-deterministic leakage.
    for (size_t i = 0; i < len; i++) {
        buf[i] = (randombytes_random() & 0xF) == 0;
    }
}

int main() {
    char a[SIZE];
    char b[SIZE];

    populate_random_bytes(a, sizeof(a));
    populate_random_bytes(b, sizeof(b));

    // sodium_memcmp is constant-time
    if (sodium_memcmp(a, b, SIZE) == 0) {
        printf("sodium_memcmp: equal\n");
    } else {
        printf("sodium_memcmp: not equal\n");
    }

    return 0;
}