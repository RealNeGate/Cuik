#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

int bar(int x, int y) {
    if (x && y) {
        return 6;
    }

    return 4;
}

void low_pass(int n, double* y,  double* x, double dt, double rc) {
    double a = dt / (rc + dt);
    y[0] = a * x[0];
    for (int i = 1; i < n; i++) {
        y[i] = a*x[i] + (1-a)*y[i-1];
    }
}

static void cmul_darray(int n, double* c,  double* a, double* b) {
    for(int i = 0; i < n; i++) {
        c[2*i] = a[2*i]*b[2*i] - a[2*i+1]*b[2*i+1];
        c[2*i+1] = a[2*i]*b[2*i+1] + a[2*i+1]*b[2*i];
    }
}

void foo(void) {
    int n = 10;
    float* arr = malloc(10 * sizeof(float));

    while (n--) {
        arr[n] = 1.0;
    }

    n = 10;
    while (n--) {
        printf("%f\n", arr[n]);
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
