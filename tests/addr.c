/*
#include <assert.h>
*/

#include <stdint.h>
#include <string.h>
#include <stddef.h>

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

#if 0
/*void mem(int* a, int i) {
    while (i--) {
        a[i] = 0;
    }
}*/
#else
uint32_t murmur3_32(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k; // = ((uint32_t*) key)[i];
        memcpy(&k, &key[i * 4], sizeof(k));

        k *= 0xcc9e2d51;
        k = ((k << 15) | (k >> 17))*0x1b873593;
        h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    }

    // load/mix up to 3 remaining tail bytes into a tail block
    uint32_t t = 0;
    const uint8_t *tail = ((const uint8_t*) key) + 4*(len/4);
    switch(len & 3) {
        case 3: t ^= tail[2] << 16;
        case 2: t ^= tail[1] <<  8;
        case 1: {
            t ^= tail[0] <<  0;
            h ^= ((0xcc9e2d51*t << 15) | (0xcc9e2d51*t >> 17))*0x1b873593;
        }
    }

    // finalization mix, including key length
    h = ((h^len) ^ ((h^len) >> 16))*0x85ebca6b;
    h = (h ^ (h >> 13))*0xc2b2ae35;
    return (h ^ (h >> 16));
}
#endif

#if 0
float floats(float* x, int i, int j) {
    return x[i*4]*2.0f + 1.0f;
}

uint64_t baz() { return __rdtsc(); }
#endif

extern int bar(int, int, int, int, int);
int foo3(int x, int y) {
    return bar(x, 1, 2, 3, 4) + y;
}

extern int bar2(int x);
int foo2(int x) { return bar2(x) + 16; }

void mur(uint32_t* ptr, uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    *ptr = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
}

uint64_t foo() {
    return 93487194713ull;
}

