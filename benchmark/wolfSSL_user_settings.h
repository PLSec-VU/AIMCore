#ifndef USER_SETTINGS_H
#define USER_SETTINGS_H

/* Platform specific settings for RISC-V embedded */
#define WOLFSSL_USER_SETTINGS

/* Disable features not needed for embedded */
#define NO_FILESYSTEM
#define NO_WRITEV
#define NO_MAIN_DRIVER
#define SINGLE_THREADED

/* Disable FIPS and hardening features */
#define NO_FIPS
#define WC_NO_HARDEN

/* Configure RNG for embedded systems with custom implementation */
#define NO_DEV_RANDOM  /* Disable /dev/random support */
/* Use custom RNG implementation that doesn't require malloc */
#define CUSTOM_RAND_GENERATE_BLOCK custom_rand_generate_block
#define CUSTOM_RAND_GENERATE_SEED custom_rand_generate_seed

/* Disable SSL/TLS - crypto only */
#define WOLFCRYPT_ONLY
#define NO_TLS
#define NO_WOLFSSL_CLIENT
#define NO_WOLFSSL_SERVER

/* Disable network functionality */
#define NO_ASN_TIME
#define NO_SESSION_CACHE
#define NO_PSK

/* Disable directory operations to avoid dirent.h */
#define NO_FILESYSTEM
#define NO_WOLFSSL_DIR

/* Enable required crypto algorithms */
#define HAVE_ECC
#define HAVE_ECC_SIGN
#define HAVE_ECC_VERIFY
#define HAVE_ECC_KEY_IMPORT
#define HAVE_ECC_KEY_EXPORT
#define HAVE_ECC_KEY_GENERATION
#define ECC_TIMING_RESISTANT

/* Enable specific ECC curves - use the standard approach */
/* Don't disable the default P-256 curve */
/* #define NO_ECC256 */  /* Keep P-256 enabled by default */
#define HAVE_ECC192
#define HAVE_ECC224
#define HAVE_ECC384
#define HAVE_ECC521
#define FP_ECC

/* Ensure we have the right curve support */
#define HAVE_ALL_CURVES

/* Enable SHA256 */
#define WOLFSSL_SHA256

/* Disable unused algorithms */
#define NO_DES3
#define NO_DSA
#define NO_RC4
#define NO_MD4
#define NO_MD5
#define NO_SHA
#define NO_PWDBASED
#define NO_PKCS12
#define NO_PKCS8
#define NO_RSA
#define NO_DH

/* Disable ASN.1 functionality partially - keep what's needed for ECC signing */
/* #define NO_ASN */  /* Comment out to enable ECC signing */
#define NO_CERTS
#define NO_ASN_TIME

/* Memory and size optimizations */
#define USE_FAST_MATH
#define TFM_TIMING_RESISTANT
#define WOLFSSL_SP_MATH
#define WOLFSSL_HAVE_SP_ECC
/* #define WOLFSSL_SP_SMALL */  /* Disable small stack for simulator */

/* Disable dynamic memory allocation for RISC-V simulator */
#define WOLFSSL_STATIC_MEMORY
#define WOLFSSL_NO_MALLOC

/* Disable debugging for size */
#define NO_DEBUG_WC

/* Platform specific */
#define SIZEOF_LONG_LONG 8
#define SIZEOF_LONG 4

/* Disable problematic features */
#define NO_SIG_WRAPPER

/* Disable inline to avoid misc.c warnings */
#define NO_INLINE

/* Custom RNG function declarations */
int custom_rand_generate_block(unsigned char* output, unsigned int sz);
int custom_rand_generate_seed(unsigned char* seed, unsigned int sz);

#endif /* USER_SETTINGS_H */