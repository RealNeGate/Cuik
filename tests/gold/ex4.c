


void compute(double a, double* x, double* restrict y) {
    y[0] = a*x[0] + y[0];
    y[1] = a*x[1] + y[1];
}
