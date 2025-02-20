
/* int foo(float a, float b) {
    return a > b;
}

int bar(float a) {
    return foo(a, 2.0f);
} */

void matmul(float* dst, float* a, float* b) {
    float a0 = a[0];
    float b0 = b[0];
    float a1 = a[1];
    float b1 = b[1];
    float a2 = a[2];
    float b2 = b[2];
    float a3 = a[3];
    float b3 = b[3];
    float sum0_0 = -0.0f;
    sum0_0 += a0 * b0;
    sum0_0 += a1 * b2;
    dst[0] = sum0_0;
    float sum0_1 = -0.0f;
    sum0_1 += a0 * b1;
    sum0_1 += a1 * b3;
    dst[1] = sum0_1;
    float sum1_0 = -0.0f;
    sum1_0 += a2 * b0;
    sum1_0 += a3 * b2;
    dst[2] = sum1_0;
    float sum1_1 = -0.0f;
    sum1_1 += a2 * b1;
    sum1_1 += a3 * b3;
    dst[3] = sum1_1;
}

