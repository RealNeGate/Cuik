


int foo(int* a, int b) {
    /* if (b) {
        a[0] = 16;
    } else {
        a[0] = 5;
    } */
    a[0] = 16;
    while (b--) {
        a[1] = 4;
        a[2] += a[1];
    }
    return a[0];
}

/* int foo(int* a) {
    float b[4];
    b[0] = 0.0f;
    a[0] = 16;
    b[1] = 0.0f;
    b[2] = 0.0f;
    a[1] = 8;
    a[2] = 33;
    b[3] = 0.0f;
    a[1] = 2;
    return a[0];
} */

