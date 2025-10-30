#include <string.h>
#include <stdio.h>
#include <sodium.h>

int main() {
    char a[] = "secret_password_123";
    char b[] = "secret_password_456";

    if (strcmp(a, b)) {
        printf("Strings are different\n");
    } else {
        printf("Strings are the same\n");
    }

    int i = randombytes_uniform(16);
    printf("Random index: %d\n", i);

    return 0;
}