#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void f(int * restrict p, int * restrict q, size_t n) {
    while (n--) *p++ = *q++;
}

bool foo(double x, double y) {
    return x == y;
}

bool foo2(int x, int y) {
    return x > y;
}
