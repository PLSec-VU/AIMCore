#include <sodium.h>

#define MESSAGE ((const unsigned char *) "Arbitrary data to hash")
#define MESSAGE_LEN 22

int main() {
    unsigned char hash[crypto_generichash_BYTES];

    int i = crypto_generichash(hash, sizeof hash, MESSAGE, MESSAGE_LEN, NULL, 0);
    if (i != 0) {
        printf("Error in BLAKE2b hashing, %d\n", i);
        return -1;
    }

    char hex_out[crypto_generichash_BYTES * 2 + 1];
    sodium_bin2hex(hex_out, sizeof hex_out, hash, crypto_generichash_BYTES);
    printf("BLAKE2b hash: %s\n", hex_out);

    return 0;
}