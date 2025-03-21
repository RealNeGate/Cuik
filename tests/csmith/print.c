#include <stdint.h>
#include <stddef.h>
#include <string.h>

long long foo(long long* x, long long y, long long z) {
    return x[0] + x[y] + x[2] + x[1+y];
}

#if 0
uint32_t murmur3_32(const void* key, size_t len) {
    const uint32_t* key32 = key;
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k = key32[i];
        k *= 0xcc9e2d51;
        k = ((k << 15) | (k >> 17))*0x1b873593;
        h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    }

    // load/mix up to 3 remaining tail bytes into a tail block
    /* uint32_t t = 0;
    const uint8_t *tail = ((const uint8_t*) key) + 4*(len/4);
    switch(len & 3) {
        case 3: t ^= tail[2] << 16;
        case 2: t ^= tail[1] <<  8;
        case 1: {
            t ^= tail[0] <<  0;
            h ^= ((0xcc9e2d51*t << 15) | (0xcc9e2d51*t >> 17))*0x1b873593;
        }
    } */

    // finalization mix, including key length
    h = ((h^len) ^ ((h^len) >> 16))*0x85ebca6b;
    h = (h ^ (h >> 13))*0xc2b2ae35;
    return (h ^ (h >> 16));
}

// void matmul(int* dst, int* a, int* b) {}

uint8_t cmp(uint32_t* key, uint32_t h) {
    if (key[1] >= 16) {
        return 0;
    }
    return 1;
}
#endif

/*int foo(int arg) {
    int x = arg/3;
    return x*4+arg;
}*/


