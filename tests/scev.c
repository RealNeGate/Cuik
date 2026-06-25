#include <stdint.h>

uint64_t foo(void) {
    uint64_t x = 2;
    for (uint64_t i = 1; i != 0; i++) {
        x += i;
    }
    return x;
}

