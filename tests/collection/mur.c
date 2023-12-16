#include <stdint.h>
#include <stddef.h>

int imm(void)   { return 69420; }
int mul3(unsigned x) { return x * 3; }
int bits(int x) { return x << 2; }

/*int loop2(int n) {
    int i = 0;
    while (i < n) {
        // bits(i);
        i += 1;
    }
    return i;
}*/

int foo(int x, int y) {
    int z = (x + 2) / y;
    z += y;
    return z;
}

int bar() {
    return 4 + 3 * 2 + 10;
}

/*uint32_t murmur3_32(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k = ((uint32_t*) key)[i];
        // memcpy(&k, &key[i * 4], sizeof(k));

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
}*/
