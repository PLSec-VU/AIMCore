#include <sodium.h>
#include "secure_memory.h"
#include "getrandom.h"

#define MESSAGE_LEN 64

int main() {
    unsigned char msg[MESSAGE_LEN];
    unsigned char hash[crypto_generichash_BYTES];
    unsigned char expected_hash[crypto_generichash_BYTES];

    SECURE_VAR(msg);
    if (getrandom(msg, sizeof msg) != 0) {
        printf("Error generating random message\n");
        return -1;
    }

    int i = crypto_generichash(hash, sizeof hash, msg, MESSAGE_LEN, NULL, 0);
    if (i != 0) {
        printf("Error in BLAKE2b hashing, %d\n", i);
        return -1;
    }

    return 0;
}