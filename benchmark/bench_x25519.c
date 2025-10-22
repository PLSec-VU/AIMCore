#include <sodium.h>

int main() {
    unsigned char alice_pk[crypto_scalarmult_curve25519_BYTES];
    unsigned char alice_sk[crypto_scalarmult_curve25519_SCALARBYTES];
    unsigned char bob_pk[crypto_scalarmult_curve25519_BYTES];
    unsigned char bob_sk[crypto_scalarmult_curve25519_SCALARBYTES];
    unsigned char shared_secret[crypto_scalarmult_curve25519_BYTES];

    // Generate Alice's keypair
    crypto_scalarmult_curve25519_base(alice_pk, alice_sk);

    // Generate Bob's keypair
    crypto_scalarmult_curve25519_base(bob_pk, bob_sk);

    // Compute shared secret
    return crypto_scalarmult_curve25519(shared_secret, alice_sk, bob_pk);
}