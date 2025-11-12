#include <sodium.h>
#include "secure_memory.h"
#include "getrandom.h"

#define MSG_LEN 64

int main() {
    unsigned char msg[MSG_LEN];
    unsigned char hash[crypto_hash_sha256_BYTES];
    unsigned char expected_hash[crypto_hash_sha256_BYTES];

    SECURE_VAR(msg);
    if (getrandom(msg, sizeof msg) != 0) {
        printf("Error generating random message\n");
        return -1;
    }

    if (crypto_hash_sha256(hash, msg, MSG_LEN) != 0) {
        printf("Error in SHA-256 hashing\n");
        return -1;
    }

    return 0;
}