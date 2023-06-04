#include <stdlib.h>

void bar(int a) {
    int* foo = malloc(a * sizeof(int));

    if (a > 10) {
        free(foo);
    }
}
