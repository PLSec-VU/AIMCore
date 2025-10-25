#include <sodium.h>

#define MSG_LEN 128

int main() {
    unsigned char msg[MSG_LEN];
    unsigned char hash[crypto_generichash_blake2b_BYTES];

    int i = crypto_generichash_blake2b(hash, crypto_generichash_blake2b_BYTES, msg, MSG_LEN, NULL, 0);
    if (i != 0) {
        printf("Error in BLAKE2b hashing, %d\n", i);
        return -1;
    }

    char hex_out[crypto_generichash_blake2b_BYTES * 2 + 1];
    sodium_bin2hex(hex_out, sizeof hex_out, hash, crypto_generichash_blake2b_BYTES);
    printf("BLAKE2b hash: %s\n", hex_out);

    return 0;
}