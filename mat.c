
/* int foo(float a, float b) {
    return a > b;
}

int bar(float a) {
    return foo(a, 2.0f);
} */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

int foo(int a, int b) {
    return a / b;
}

uint32_t count(uint64_t bits) {
    uint32_t used = 0;
    for (uint32_t i = 0; i < 64; i++) {
        if (bits & (1ull << i)) {
            used += 1;
        }
    }

    return used;
}

#if 1
int main() {
    printf("H %d\n", count(0b100010010001));
    printf("F %d\n", foo(16, 3));
}
#endif

void matmul(float* dst, float* a, float* b) {
    if (dst == NULL) {
        return;
    }

    for (size_t i = 0; i < 16; i++) {
        dst[i*16 + 0] = -0.0f;
        dst[i*16 + 1] = -0.0f;
        dst[i*16 + 2] = -0.0f;
        dst[i*16 + 3] = -0.0f;
        dst[i*16 + 4] = -0.0f;
        dst[i*16 + 5] = -0.0f;
        dst[i*16 + 6] = -0.0f;
        dst[i*16 + 7] = -0.0f;
        dst[i*16 + 8] = -0.0f;
        dst[i*16 + 9] = -0.0f;
        dst[i*16 + 10] = -0.0f;
        dst[i*16 + 11] = -0.0f;
        dst[i*16 + 12] = -0.0f;
        dst[i*16 + 13] = -0.0f;
        dst[i*16 + 14] = -0.0f;
        dst[i*16 + 15] = -0.0f;
    }

    for (size_t kk = 0; kk < 16; kk += 4) {
        for (size_t jj = 0; jj < 16; jj += 4) {
            for (size_t i = 0; i < 16; i++) {
                float a0 = a[i*16 + (kk+0)];
                float a1 = a[i*16 + (kk+1)];
                float a2 = a[i*16 + (kk+2)];
                float a3 = a[i*16 + (kk+3)];
                float b0 = b[(kk+0)*16 + (jj+0)];
                float b1 = b[(kk+1)*16 + (jj+0)];
                float b2 = b[(kk+2)*16 + (jj+0)];
                float b3 = b[(kk+3)*16 + (jj+0)];
                float b4 = b[(kk+0)*16 + (jj+1)];
                float b5 = b[(kk+1)*16 + (jj+1)];
                float b6 = b[(kk+2)*16 + (jj+1)];
                float b7 = b[(kk+3)*16 + (jj+1)];
                float b8 = b[(kk+0)*16 + (jj+2)];
                float b9 = b[(kk+1)*16 + (jj+2)];
                float b10 = b[(kk+2)*16 + (jj+2)];
                float b11 = b[(kk+3)*16 + (jj+2)];
                float b12 = b[(kk+0)*16 + (jj+3)];
                float b13 = b[(kk+1)*16 + (jj+3)];
                float b14 = b[(kk+2)*16 + (jj+3)];
                float b15 = b[(kk+3)*16 + (jj+3)];
                float sum0 = dst[i*16 + (jj+0)];
                float sum1 = dst[i*16 + (jj+1)];
                float sum2 = dst[i*16 + (jj+2)];
                float sum3 = dst[i*16 + (jj+3)];
                sum0 += a0 * b0;
                sum0 += a1 * b1;
                sum0 += a2 * b2;
                sum0 += a3 * b3;
                dst[i*16 + (jj+0)] = sum0;
                sum1 += a0 * b4;
                sum1 += a1 * b5;
                sum1 += a2 * b6;
                sum1 += a3 * b7;
                dst[i*16 + (jj+1)] = sum1;
                sum2 += a0 * b8;
                sum2 += a1 * b9;
                sum2 += a2 * b10;
                sum2 += a3 * b11;
                dst[i*16 + (jj+2)] = sum2;
                sum3 += a0 * b12;
                sum3 += a1 * b13;
                sum3 += a2 * b14;
                sum3 += a3 * b15;
                dst[i*16 + (jj+3)] = sum3;
            }
        }
    }
}

