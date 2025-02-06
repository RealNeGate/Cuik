void matmul(int* dst, int* a, int* b) {
    int a0 = a[0];
    int b0 = b[0];
    int a1 = a[1];
    int b1 = b[1];
    int a2 = a[2];
    int b2 = b[2];
    int a3 = a[3];
    int b3 = b[3];
    int sum0_0 = 0;
    sum0_0 += a0 * b0;
    sum0_0 += a1 * b1;
    dst[0] = sum0_0;
    int sum0_1 = 0;
    sum0_1 += a0 * b2;
    sum0_1 += a1 * b3;
    dst[1] = sum0_1;
    int sum1_0 = 0;
    sum1_0 += a2 * b0;
    sum1_0 += a3 * b1;
    dst[2] = sum1_0;
    int sum1_1 = 0;
    sum1_1 += a2 * b2;
    sum1_1 += a3 * b3;
    dst[3] = sum1_1;
}
