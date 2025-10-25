#include <sodium.h>

#define MSG_LEN 64

int main() {
    unsigned char out[MSG_LEN];
    unsigned char msg[MSG_LEN];
    unsigned char key[crypto_stream_chacha20_KEYBYTES];
    unsigned char nonce[crypto_stream_chacha20_NONCEBYTES];

    if (crypto_stream_chacha20_xor(out, msg, MSG_LEN, nonce, key) != 0) {
        printf("Error in ChaCha20 encryption\n");
        return -1;
    }

    // print hex with sodium_bin2hex
    char hex_out[MSG_LEN * 2 + 1];
    sodium_bin2hex(hex_out, sizeof hex_out, out, MSG_LEN);
    printf("ChaCha20 output: %s\n", hex_out);

    return 0;
}