#include <stdio.h>

void matmul(float* dst, float* a, float* b);

static float a[] = { 1.0, 0.0, 0.0, 1.0 };
static float b[] = { 0.5, 0.0, 0.0, 0.5 };
static float dst[4];

int main() {
    matmul(dst, a, b);
    return 0;
}

