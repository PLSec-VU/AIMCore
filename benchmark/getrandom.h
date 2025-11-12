#ifndef GETRANDOM_H
#define GETRANDOM_H

#include <stddef.h>

/**
 * Fill buffer with random bytes from /dev/urandom
 * This bypasses libsodium's PRNG to ensure consistent leakages.
 * @param buf Buffer to fill with random data
 * @param size Number of bytes to read
 * @return 0 on success, -1 on error
 */
int getrandom(void * const buf, const size_t size);

#endif // GETRANDOM_H