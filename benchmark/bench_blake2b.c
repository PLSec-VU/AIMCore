#include <sodium.h>
#include "secure_memory.h"

#define MESSAGE ((const unsigned char *) "Arbitrary data to hash")
#define MESSAGE_LEN 22
#define OUTPUT "3dc7925e13e4c5f0f8756af2cc71d5624b58833bb92fa989c3e87d734ee5a600"

int main() {
    unsigned char hash[crypto_generichash_BYTES];
    unsigned char expected_hash[crypto_generichash_BYTES];

    // Mark sensitive data as secret
    SECURE_VAR(hash);            // Hash output can be sensitive

    int i = crypto_generichash(hash, sizeof hash, MESSAGE, MESSAGE_LEN, NULL, 0);
    if (i != 0) {
        printf("Error in BLAKE2b hashing, %d\n", i);
        return -1;
    }

    char hex_out[crypto_generichash_BYTES * 2 + 1];
    sodium_bin2hex(hex_out, sizeof hex_out, hash, crypto_generichash_BYTES);
    printf("BLAKE2b hash: %s\n", hex_out);

    sodium_hex2bin(expected_hash, crypto_generichash_BYTES,
                       OUTPUT, sizeof(OUTPUT) - 1,
                       NULL, NULL, NULL);
    return sodium_memcmp(hash, expected_hash, crypto_generichash_BYTES);
}