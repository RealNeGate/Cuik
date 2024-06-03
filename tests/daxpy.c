









#if 0
int foo3(int a, int b, int c) {
    if (a > b) {
        return a + c;
    } else {
        return a + b;
    }
}
#else





void daxpy(long long n, double a, double* restrict x, double* y) {
    for (long long i = 0; i < n; i++) {
        y[i] = a*x[i] + y[i];
    }
}







#endif

#if 0
void daxpy2(long long n, double a, double* x, double* y) {
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
void bar();
#else
typedef unsigned long long size_t;

void bar(int i);
void matmul(float* c, float* a, float* b) {
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++) {
            float sum = 0.0f;
            for (size_t k = 0; k < 4; k++) {
                sum += a[k*4 + i] * b[j*4 + k];
            }
            c[j*4 + i] = sum;
        }

        if (a) {
            bar(0);
        }
        bar(1);
    }
}
#endif

#if 0
int foo(int x, int y) {
    return x + y*4;
}

void bar();
void foo(int n) {
    for (int i = 0; i < n; i++) {
        bar();
    }
}
#endif

