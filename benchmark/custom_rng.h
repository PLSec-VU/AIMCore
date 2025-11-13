#ifndef CUSTOM_RNG_H
#define CUSTOM_RNG_H

#include <stddef.h>
#include <stdint.h>

/**
 * Custom RNG implementation for WolfSSL that doesn't use malloc
 * Uses getrandom() to get entropy from /dev/urandom
 */

/**
 * Custom random block generation function for WolfSSL
 * @param output Buffer to fill with random data
 * @param sz Number of bytes to generate
 * @return 0 on success, negative on error
 */
int custom_rand_generate_block(unsigned char* output, unsigned int sz);

/**
 * Custom random seed generation function for WolfSSL
 * @param seed Buffer to fill with seed data
 * @param sz Number of seed bytes to generate
 * @return 0 on success, negative on error
 */
int custom_rand_generate_seed(unsigned char* seed, unsigned int sz);

#endif /* CUSTOM_RNG_H */