void max_array(float* restrict x, float* restrict y) {
    for (int i = 0; i < 65536; i++) {
        x[i] = y[i] > x[i] ? y[i] : x[i];
    }
}
