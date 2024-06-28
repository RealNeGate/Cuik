
#include <stddef.h>

void compute(size_t n, double a, double* x, double* restrict y) {
    size_t i = 0;
    for (; i+1 < n; i += 2) {
        y[i+0] = a*x[i+0] + y[i+0];
        y[i+1] = a*x[i+1] + y[i+1];
    }

    if (i < n) {
        y[i] = a*x[i] + y[i];
    }
}

