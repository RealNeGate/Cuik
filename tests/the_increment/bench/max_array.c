void max_array(short* restrict x, short* restrict y) {
    for (int i = 0; i < 65536; i++) {
        x[i] = y[i] > x[i] ? y[i] : x[i];
    }
}
