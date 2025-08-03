#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

uint64_t table[256];
void dfa_range(int state, int min, int max, int next) {
    for (int i = min; i <= max; i++) {
        // erase old entry
        table[i] &= ~(0b111111 << (state*6));
        table[i] |= (next*6) << (state*6);
    }
}

static void low_pass(int n, double* y,  double* x, double dt, double rc) {
    double a = dt / (rc + dt);
    y[0] = a * x[0];
    for (int i = 1; i < n; i++) {
        y[i] = a*x[i] + (1-a)*y[i-1];
    }
}

typedef struct {
    size_t n;
    float* arr;
} Slice;

#if 1
static void sroa() {
    Slice s = { 10, malloc(10 * sizeof(float)) };
    while (s.n--) {
        s.arr[s.n] = 1.0;
    }
}
#else
void sroa() {
    int n = 10;
    float* arr = malloc(10 * sizeof(float));
    while (n--) {
        arr[n] = 1.0;
    }
}
#endif
