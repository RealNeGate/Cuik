#include <stdint.h>

uint32_t float_bits(float x) {
    union {
        uint32_t i;
        float f;
    } r = { .f = x };

    return r.i;
}
