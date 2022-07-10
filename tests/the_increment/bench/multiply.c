#include <stdint.h>
#include <stddef.h>

uint32_t mul_by(const uint32_t *data, size_t len, uint32_t m) {
    uint32_t sum = 0;
    for (size_t i = 0; i < len - 1; i++) {
        uint32_t x = data[i], y = data[i + 1];
        sum += x * y * m * i * i;
    }
    return sum;
}
