#include <sodium.h>

#define MSG_LEN 64
#define OUTPUT "76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586"

int main() {
    unsigned char out[MSG_LEN];
    unsigned char msg[MSG_LEN];
    unsigned char key[crypto_stream_chacha20_KEYBYTES];
    unsigned char nonce[crypto_stream_chacha20_NONCEBYTES];
    unsigned char expected_out[MSG_LEN];

    if (crypto_stream_chacha20_xor(out, msg, MSG_LEN, nonce, key) != 0) {
        printf("Error in ChaCha20 encryption\n");
        return -1;
    }

    // print hex with sodium_bin2hex
    char hex_out[MSG_LEN * 2 + 1];
    sodium_bin2hex(hex_out, sizeof hex_out, out, MSG_LEN);
    printf("ChaCha20 output: %s\n", hex_out);

    sodium_hex2bin(expected_out, MSG_LEN,
                       OUTPUT, sizeof(OUTPUT) - 1,
                       NULL, NULL, NULL);

    return sodium_memcmp(out, expected_out, MSG_LEN);
}