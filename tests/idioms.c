#include <stdio.h>
#include <stdlib.h>

void block_copy(int* dst, size_t m, int* src, size_t n) {
    for (int i = 0; i < n && i < m; i++) {
        dst[i] = src[i];
    }
}
