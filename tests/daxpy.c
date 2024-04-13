












/*void daxpy(long long n, double a, double* x, double* y) {
    for (long long i = 0; i < n; i++) {
        y[i] = a*x[i] + y[i];
    }
}*/

#if 0
void daxpy2(long long n, double a, double* x, double* y, bool enable) {
    for (int i = 0; i != enable; i++) {
        // manual 4-way unroll
        long long i = 0, unrolled_n = n & ~3;
        while (i < unrolled_n) {
            y[i] = a*x[i] + y[i], i += 1;
            y[i] = a*x[i] + y[i], i += 1;
            y[i] = a*x[i] + y[i], i += 1;
            y[i] = a*x[i] + y[i], i += 1;
        }

        while (i < n) {
            y[i] = a*x[i] + y[i], i += 1;
        }
    }
}
#endif

#if 0
void matmul(float* c, float* a, float* b) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            float sum = 0.0f;
            for (int k = 0; k < 4; k++) {
                sum += a[k*4 + i] * b[j*4 + k];
            }
            c[j*4 + i] = sum;
        }
    }

    /*float sum = 0.0f;
    for (int k = 0; k < 4; k++) {
        sum += a[k*4 + i] * b[j*4 + k];
    }
    c[j*4 + i] = sum;*/
}
#else
int* bar;
int foo(int n) {
    int i = 0;
    for (; i < n; i++) {
        bar[i] = 16;
    }
    return i;
}
#endif

/*
matmul:                                 // @matmul
        mov     x8, xzr
.LBB0_1:                                // =>This Loop Header: Depth=1
        add     x10, x0, x8, lsl #2
        mov     x9, xzr
        mov     x11, x2
.LBB0_2:                                //   Parent Loop BB0_1 Depth=1
        movi    d0, #0000000000000000
        mov     x12, xzr
.LBB0_3:                                //   Parent Loop BB0_1 Depth=1
        ldr     s1, [x1, x12, lsl #2]
        ldr     s2, [x11, x12]
        add     x12, x12, #4
        cmp     x12, #16
        fmadd   s0, s1, s2, s0
        b.ne    .LBB0_3
        lsl     x12, x9, #4
        add     x9, x9, #1
        add     x11, x11, #16
        cmp     x9, #4
        str     s0, [x10, x12]
        b.ne    .LBB0_2
        add     x8, x8, #1
        add     x1, x1, #4
        cmp     x8, #4
        b.ne    .LBB0_1
        ret


sum += a[k*4 + i] * b[j*4 + k];

ld a, [(k + i)*4]
ld b, [j*16 + k]
add k, k, 4

*/

