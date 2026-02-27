#include <stdio.h>
#include <string.h>
#include <wolfssl/options.h>
#include <wolfssl/wolfcrypt/ecc.h>
#include "secure_memory.h"
#include "getrandom.h"
#include "custom_rng.h"

#define MESSAGE_LEN 64


int main() {
    int ret = 0;
    ecc_key key;
    WC_RNG rng;
    byte signature[ECC_MAX_SIG_SIZE];
    word32 sigLen = sizeof(signature);
    int verify_status = 0;
    
    const char* message = "Test message for signing.";
    word32 messageLen = (word32)strlen(message);
    
    ret = wc_InitRng(&rng);
    if (ret != 0) {
        printf("Error initializing RNG: %d\n", ret);
        return -1;
    }
    
    ret = wc_ecc_init(&key);
    if (ret != 0) {
        printf("Error initializing ECC key: %d\n", ret);
        wc_FreeRng(&rng);
        return -1;
    }
    
    printf("Generating ECC P-256 key...\n");
    ret = wc_ecc_make_key(&rng, 32, &key);
    if (ret != 0) {
        printf("Error generating ECC key: %d\n", ret);
        wc_ecc_free(&key);
        wc_FreeRng(&rng);
        return -1;
    }
    
    // Mark sensitive data as secret
    SECURE_VAR(key);
    SECURE_ARRAY(signature, sigLen);
    
    // Sign the raw message directly (no hashing)
    ret = wc_ecc_sign_hash((const byte*)message, messageLen, signature, &sigLen, &rng, &key);
    if (ret != 0) {
        printf("Error signing message: %d\n", ret);
        wc_ecc_free(&key);
        wc_FreeRng(&rng);
        return -1;
    }
    
    // Print signature
    printf("Signature: ");
    for (word32 i = 0; i < sigLen; i++) {
        printf("%02x", signature[i]);
    }
    printf("\n\n");
    
    // Cleanup
    wc_ecc_free(&key);
    wc_FreeRng(&rng);
    
    return 0;
}