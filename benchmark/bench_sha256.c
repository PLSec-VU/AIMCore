#include <sodium.h>

#define MSG_LEN 128

int main() {
    unsigned char msg[MSG_LEN];
    unsigned char hash[crypto_hash_sha256_BYTES];

    return crypto_hash_sha256(hash, msg, MSG_LEN);
}