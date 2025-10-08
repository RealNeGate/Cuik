#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

static void low_pass(int n, float* y,  float* x, float dt, float rc) {
    float a = dt / (rc + dt);
    y[0] = a * x[0];
    for (int i = 1; i < n; i++) {
        y[i] = a*x[i] + (1-a)*y[i-1];
    }
}

static uint32_t pcg32_pie(uint64_t *state) {
    uint64_t old = *state ^ 0xc90fdaa2adf85459ULL;
    *state = *state * 6364136223846793005ULL + 0xc90fdaa2adf85459ULL;
    uint32_t xorshifted = ((old >> 18u) ^ old) >> 27u;
    uint32_t rot = old >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

enum {
    N = 1000
};

void foo(void) {
    int n = N;
    float* arr = malloc(n * sizeof(float));

    uint64_t seed = 11400714819323198485llu;
    while (n--) {
        uint32_t x = pcg32_pie(&seed);
        arr[n] = (x & 0xFFFFFF) / 16777216.0;
    }

    float* out = malloc(N * sizeof(float));
    low_pass(N, out, arr, 1.0, 2.0);

    n = N;
    while (n--) {
        printf("%f\n", out[n]);
    }
}

int main() {
    printf("Hello, World!\n");
    foo();
    return 0;
}

#if 0
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
#endif
