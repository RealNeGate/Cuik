#include <stdio.h>

void matmul(float* dst, float* a, float* b);

static float a[] = {
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
};

static float b[] = {
    0.5, 0.0, 0.0, 4.0,
    0.0, 0.5, 0.0, 3.0,
    0.0, 0.0, 0.5, 2.0,
    0.0, 0.0, 0.0, 1.0
};

static float dst[16];

int main() {
    matmul(dst, a, b);
    return 0;
}

