#include "custom_rng.h"
#include "getrandom.h"
#include <string.h>

int custom_rand_generate_block(unsigned char* output, unsigned int sz) {
    if (output == NULL || sz == 0) {
        return -1;
    }
    
    // Use our getrandom function to fill the buffer
    if (getrandom(output, sz) != 0) {
        return -1;
    }
    
    return 0;
}

int custom_rand_generate_seed(unsigned char* seed, unsigned int sz) {
    if (seed == NULL || sz == 0) {
        return -1;
    }
    
    // Use our getrandom function to fill the seed buffer
    if (getrandom(seed, sz) != 0) {
        return -1;
    }
    
    return 0;
}