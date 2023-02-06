#include <stddef.h>
#include <stdint.h>

size_t* tile1(size_t* a, size_t b) { return &a[b]; }
size_t  tile2(size_t* a, size_t b) { return a[b]; }
size_t* tile3(size_t* a, size_t b) { return &a[b*2]; }
size_t* tile4(size_t* a, size_t b) { return &a[b*3]; }
size_t* tile5(size_t* a, size_t b) { return &a[b*2 + 1]; }
size_t* tile6(size_t* a, size_t b) { return &a[b*3 + 1]; }

void tile7(size_t* a, size_t b) { a[b*3] = b; }

int test_mod(int a) {
    return a % 2 != 0 ? 4 : 2;
}

void test_fun(int* b, int** c) {
    for (unsigned i = 0; i < 100; i++) {
        c[i] = &b[i];
    }
}

void fn(int16_t* A, uint16_t* B, uint16_t* C) {
    for (int i = 0; i < 16; i++) {
        if (A[i] > 0) A[i] = (C[i] + A[i]) * B[i] >> 16;
        else A[i] = -((C[i] - A[i]) * B[i] >> 16);
    }
}

void foo(size_t* a, size_t* b, size_t c) {
    a[c] += 16;
    b[c] += 16;
}

void bar(size_t* a, size_t b) {
    a[b] += 16;
}

#if 0
int test(int a, int b) {
    int x, y, z;

    if(a > 3) {
        x = 4;
        y = 1;
        z = b & 0xFF00;
    }
    else {
        x = 9;
        y = 2;
        z = b << 8;
    }

    int p = (x * y) * 4;
    int q = z & 0xF;
    return p >= 16 && q == 0;
}

size_t foo2(size_t* a, size_t n) {
    size_t b = 0;
    for (size_t i = 0; i < n; i++) {
        b += a[i];
    }
    return b;
}

static size_t bar2(size_t a) {
    return a % 2 != 0 ? 4 : 2;
}

size_t foo3(size_t a) {
    return a % 2 != 0 ? 4 : 2;
}
#endif
