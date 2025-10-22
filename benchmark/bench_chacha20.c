#include <sodium.h>

#define MSG_LEN 1024

int main() {
    unsigned char out[MSG_LEN];
    unsigned char msg[MSG_LEN];
    unsigned char key[crypto_stream_chacha20_KEYBYTES];
    unsigned char nonce[crypto_stream_chacha20_NONCEBYTES];

    return crypto_stream_chacha20_xor(out, msg, MSG_LEN, nonce, key);
}