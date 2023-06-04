#include <stdio.h>
#include <assert.h>

int main() {
    const char *🐱 = "cat";
    \U0001f431 = "cat2";

    printf("Hello! %s\n", \U0001f431);
    return 0;
}
