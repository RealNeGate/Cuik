#include <assert.h>
#include <stdint.h>

/*int foo() {
    int x = 5 + 5;
    int y = 2 * 5 + 5;
    assert(x && "foo");

    return x * y;
}


int ptr(int k) {
    static int a, b;
    return !!(k ? &a : &b);
}

int vol(volatile int* x) {
    *x = 1;
    return *x;
}*/

uint32_t mur(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    return (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
}

float foo(float* x, int i, int j) {
    return x[i + j]*2.0f + 1.0f;
}
