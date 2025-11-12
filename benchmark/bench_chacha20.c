#include <sodium.h>
#include "secure_memory.h"
#include "getrandom.h"

#define MSG_LEN 64
#define OUTPUT "76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586"

int main() {
    unsigned char out[MSG_LEN];
    unsigned char msg[MSG_LEN];
    unsigned char key[crypto_stream_chacha20_KEYBYTES];
    unsigned char nonce[crypto_stream_chacha20_NONCEBYTES];
    unsigned char expected_out[MSG_LEN];

    // Mark sensitive cryptographic data as secret
    SECURE_VAR(key);           // Encryption key is highly sensitive
    SECURE_VAR(msg);           // Message content should be protected
    SECURE_VAR(out);           // Encrypted output is sensitive
    SECURE_VAR(nonce);         // Nonce can be sensitive in some contexts

    if (getrandom(key, sizeof key) != 0) {
        printf("Error generating random key\n");
        return -1;
    }
    if (getrandom(nonce, sizeof nonce) != 0) {
        printf("Error generating random nonce\n");
        return -1;
    }
    if (getrandom(msg, sizeof msg) != 0) {
        printf("Error generating random message\n");
        return -1;
    }

    if (crypto_stream_chacha20_xor(out, msg, MSG_LEN, nonce, key) != 0) {
        printf("Error in ChaCha20 encryption\n");
        return -1;
    }

    return 0;
}