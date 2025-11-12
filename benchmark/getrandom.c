#include "getrandom.h"
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

int getrandom(void * const buf, const size_t size) {
    if (buf == NULL || size == 0) {
        errno = EINVAL;
        return -1;
    }
    
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd == -1) {
        return -1;
    }
    
    size_t bytes_read = 0;
    char *buffer = (char *)buf;
    
    while (bytes_read < size) {
        ssize_t result = read(fd, buffer + bytes_read, size - bytes_read);
        if (result == -1) {
            if (errno == EINTR) {
                // Interrupted by signal, try again
                continue;
            }
            close(fd);
            return -1;
        }
        if (result == 0) {
            // Unexpected EOF from /dev/urandom
            close(fd);
            errno = EIO;
            return -1;
        }
        bytes_read += result;
    }
    
    close(fd);
    return 0;
}