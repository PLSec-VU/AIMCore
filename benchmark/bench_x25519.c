#include <sodium.h>
#include <stdio.h>
#include <string.h>

int main() {
    unsigned char input_scalar[crypto_scalarmult_curve25519_SCALARBYTES];
    unsigned char input_u[crypto_scalarmult_curve25519_BYTES];
    unsigned char output_u[crypto_scalarmult_curve25519_BYTES];
    unsigned char expected_output[crypto_scalarmult_curve25519_BYTES];
    unsigned char hex_output[crypto_scalarmult_curve25519_BYTES * 2 + 1];

    // Test vector from specification
    // Input scalar: a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4
    if (sodium_hex2bin(input_scalar, crypto_scalarmult_curve25519_SCALARBYTES,
                       "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4",
                       64, NULL, NULL, NULL) != 0) {
        printf("Failed to decode input scalar\n");
        return -1;
    }

    // Input u-coordinate: e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c
    if (sodium_hex2bin(input_u, crypto_scalarmult_curve25519_BYTES,
                       "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c",
                       64, NULL, NULL, NULL) != 0) {
        printf("Failed to decode input u-coordinate\n");
        return -1;
    }

    // Expected output: c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552
    if (sodium_hex2bin(expected_output, crypto_scalarmult_curve25519_BYTES,
                       "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552",
                       64, NULL, NULL, NULL) != 0) {
        printf("Failed to decode expected output\n");
        return -1;
    }
    // Perform X25519 scalar multiplication
    if (crypto_scalarmult_curve25519(output_u, input_scalar, input_u) != 0) {
        printf("X25519 computation failed\n");
        return -1;
    }

    // Print computed output
    sodium_bin2hex((char *)hex_output, sizeof hex_output, output_u, sizeof output_u);
    printf("Computed output: %s\n", hex_output);

    // Verify the result matches expected output
    return sodium_memcmp(output_u, expected_output, crypto_scalarmult_curve25519_BYTES);
}