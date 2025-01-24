#include <stdint.h>

long long foo(long long* x, long long y, long long z) {
    return x[0] + x[y] + x[2] + x[1+y];
}


