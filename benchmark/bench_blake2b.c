#include <sodium.h>

#define MSG_LEN 1024

int main() {
    unsigned char msg[MSG_LEN];
    unsigned char hash[crypto_generichash_blake2b_BYTES];

    return crypto_generichash_blake2b(hash, crypto_generichash_blake2b_BYTES,
                                      msg, MSG_LEN, NULL, 0);
}