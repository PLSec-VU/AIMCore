#include <string.h>
#include <stdio.h>

int main() {
    char a[] = "secret_password_123";
    char b[] = "secret_password_456";

    if (strcmp(a, b)) {
        printf("Strings are different\n");
    } else {
        printf("Strings are the same\n");
    }

    return 0;
}