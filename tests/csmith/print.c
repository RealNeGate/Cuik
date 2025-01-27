#include <stdint.h>

#if 1
long long foo(long long* x, long long y, long long z) {
    return x[0] + x[y] + x[2] + x[1+y] + 16;
}
#endif

/*uint32_t murmur(uint32_t* key, uint32_t h) {
    uint32_t k = *key;
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return h;
}*/

