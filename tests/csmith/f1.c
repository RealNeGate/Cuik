#include <stdint.h>
#include <stddef.h>

void foo(size_t n, float* x, float* y, float a) {
    size_t i = 0;
    for (; i < n; i += 4) {
        float y0 = y[i+0] + a*x[i+0];
        float y1 = y[i+1] + a*x[i+1];
        float y2 = y[i+2] + a*x[i+2];
        float y3 = y[i+3] + a*x[i+3];
        y[i+0] = y0;
        y[i+1] = y1;
        y[i+2] = y2;
        y[i+3] = y3;
    }

    for (; i < n; i++) {
        y[i] += a*x[0];
    }
}

