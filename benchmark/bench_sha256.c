#include <sodium.h>

#define MSG_LEN 128
#define OUTPUT "38723a2e5e8a17aa7950dc008209944e898f69a7bd10a23c839d341e935fd5ca"

int main() {
    unsigned char msg[MSG_LEN];
    unsigned char hash[crypto_hash_sha256_BYTES];
    unsigned char expected_hash[crypto_hash_sha256_BYTES];

    if (crypto_hash_sha256(hash, msg, MSG_LEN) != 0) {
        printf("Error in SHA-256 hashing\n");
        return -1;
    }

    char hex_out[crypto_hash_sha256_BYTES * 2 + 1];
    sodium_bin2hex(hex_out, sizeof hex_out, hash, crypto_hash_sha256_BYTES);
    printf("SHA-256 hash: %s\n", hex_out);

    sodium_hex2bin(expected_hash, crypto_hash_sha256_BYTES,
                       OUTPUT, sizeof(OUTPUT) - 1,
                       NULL, NULL, NULL);
    return sodium_memcmp(hash, expected_hash, crypto_hash_sha256_BYTES);
}