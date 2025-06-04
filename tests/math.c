
#include <stdint.h>
#include <stdio.h>

uint64_t foo(uint64_t x);
int main() {
    for (int i = 0; i < 100; i++) {
        uint64_t x = (1ull << 63ull) + i;
        printf("%llu %llu\n", x, foo(x));
        // printf("%llu\n", foo(i)); // (1ull << 63ull) + foo(i));
    }
}

uint64_t foo(uint64_t x) {
    return x / 7;
    // return x / ((1ull << 63ull) + 1ull);
}

// aaaaaaaaaaaaaaab_16

