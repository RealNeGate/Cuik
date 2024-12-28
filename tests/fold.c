#include <stdint.h>
#include <stddef.h>

uint32_t foo(uint8_t* key, uint32_t h, uint32_t k, size_t i) {
    uint32_t k = ((uint32_t*) key)[i];
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    return h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
}
