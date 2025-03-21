#include <stdio.h>

void matmul(float* dst, float* a, float* b);

static float a[256];
static float b[256];
static float dst[256];

int main() {
    matmul(dst, a, b);
    return 0;
}

