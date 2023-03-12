#include <stddef.h>
#include <string.h>
#include <stdint.h>

#if 0
int do_stuff(int a, int b, int c) {
    int d = b + a;
    int e = b & c;
    int f = e - b;
    int g = d ^ f;
    return d + e + f + g;
}

int fa(int value) {
    return value;
}

int* fb(int value) {
    return &value;
}

int fc(int value) {
    return value + 16 - 5 & 3;
}

int fd(int value) {
    if (value - 10) return ~value;

    return value;
}

int fe(int value) {
    int sum = 0;
    while (value--) {
        sum += value;
    }
    return sum;
}

long long ff(int* value) {
    return *value;
}

long long fg(int* value) {
    return ((long long) *value) & 63;
}

long long fh(int* value) {
    return fe(10);
}

int main() {}

int without_restrict(int const* const x, int* y) {
    if(*x == 0) {
        *y = 1;
        return *x;
    }

    return 1;
}
#endif

#if 1
// murmur3 32-bit without UB unaligned accesses
// https://github.com/demetri/scribbles/blob/master/hashing/ub_aware_hash_functions.c
uint32_t hash_ident(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k = *(uint32_t*) &key[i * 4];
        // convert into normal loads and stores which can be mem2reg'd:
        //   tmp = load &key[i * 4] (1 align)
        //   store k, tmp           (1 align)
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
}
#endif
