#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

uint32_t hash_ident(const void* key, size_t len);

uint32_t clang_hash_ident(const void* key, size_t len) {
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

static void test(const char* word) {
    size_t l = strlen(word);
    printf("%x\t%x\n", hash_ident(word, l), clang_hash_ident(word, l));
}

int main() {
    test("Hello");
    test("Goodbyte");
    return 0;
}
