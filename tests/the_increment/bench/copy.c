#include <stdio.h>
#include <stdlib.h>

void f(int* restrict p, int* restrict q, size_t n) {
    while (n--) *p++ = *q++;
}
