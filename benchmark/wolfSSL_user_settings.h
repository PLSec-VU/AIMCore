/* Map wolfSSL RNG to your custom functions */
#define WOLFSSL_NO_GETPID
#define WOLFSSL_NO_MALLOC
#define WC_NO_HASHDRBG
#define WC_RESEED_INTERVAL (1000000)

#define CUSTOM_RAND_GENERATE_BLOCK custom_rand_generate_block
#define CUSTOM_RAND_GENERATE_SEED custom_rand_generate_seed

extern int custom_rand_generate_block(unsigned char* output, unsigned int sz);
extern int custom_rand_generate_seed(unsigned char* seed, unsigned int sz);