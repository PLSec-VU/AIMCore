#include <string.h>
#include <stdio.h>

void main() {
    char a[] = "secret_password_123";
    char b[] = "secret_password_456";

    if (strcmp(a, b)) {
        printf("Strings are different\n");
    } else {
        printf("Strings are the same\n");
    }
}