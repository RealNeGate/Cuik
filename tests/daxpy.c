












void daxpy(long long n, double a, double* x, double* y) {
    for (long long i = 0; i < n; i++) {
        y[i] = a*x[i] + y[i];
    }
}


void daxpy2(long long n, double a, double* x, double* y) {
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


