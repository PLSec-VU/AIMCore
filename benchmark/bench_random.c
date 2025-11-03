#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include "secure_memory.h"

int main() {
    // open /dev/urandom
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd == -1) {
        perror("Failed to open /dev/urandom");
        return 1;
    }

    struct stat sb;
    if (fstat(fd, &sb) == -1) {
        perror("Failed to fstat");
        return 1;
    }

    switch (sb.st_mode & S_IFMT) {
    case S_IFBLK:  printf("block device\n");            break;
    case S_IFCHR:  printf("character device\n");        break;
    case S_IFDIR:  printf("directory\n");               break;
    case S_IFIFO:  printf("FIFO/pipe\n");               break;
    case S_IFLNK:  printf("symlink\n");                 break;
    case S_IFREG:  printf("regular file\n");            break;
    case S_IFSOCK: printf("socket\n");                  break;
    default:       printf("unknown?\n");                break;
    }

    printf("Constants: S_IFMT: %d, S_IFCHR: %d\n", S_IFMT, S_IFCHR);

    printf("I-node number:            %ld\n", (long) sb.st_ino);
    printf("Mode:                     %lo (octal)\n", (unsigned long) sb.st_mode);
    printf("Link count:               %ld\n", (long) sb.st_nlink);
    printf("Ownership:                UID=%ld   GID=%ld\n", (long) sb.st_uid, (long) sb.st_gid);
    printf("Preferred I/O block size: %ld bytes\n", (long) sb.st_blksize);
    printf("File size:                %lld bytes\n", (long long) sb.st_size);
    printf("Blocks allocated:         %lld\n", (long long) sb.st_blocks);
    printf("Last status change:       %s", ctime(&sb.st_ctime));
    printf("Last file access:         %s", ctime(&sb.st_atime));
    printf("Last file modification:   %s", ctime(&sb.st_mtime));

    char buffer[16];
    
    // Mark random data buffer as secret since random data can be sensitive
    SECURE_VAR(buffer);
    
    ssize_t bytesRead = read(fd, buffer, sizeof(buffer));
    if (bytesRead <= 0) {
        perror("Failed to read from /dev/urandom");
        return 1;
    }

    printf("Read %zd bytes from /dev/urandom: ", bytesRead);
    // printing this will trigger non-deterministic leakage due to formatting random numbers
    for (ssize_t i = 0; i < bytesRead; i++) {
        printf("%02x", (unsigned char)buffer[i]);
    }
    printf("\n");

    return 0;
}