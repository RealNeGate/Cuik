




int foo(int* a) {
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
}

