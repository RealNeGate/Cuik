#include <stdio.h>
#include <stdint.h>

static int murmur_32b(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return h;
}

int main() {
    size_t len = 5;
    const char* key = "hello";

    // main body, work on 32-bit blocks at a time
    uint32_t h = 0xFFFFFFFF;
    for (int i=0;i<len/4;i++) {
        h = murmur_32b(h, ((uint32_t*) key)[i]);
    }

    return printf("Hello! %x\n", h);
}
