#include <stdio.h>
#include <stdlib.h>

void block_copy(int* restrict dst, size_t m, int* src, size_t n) {
    for (size_t i = 0; i < n && i < m; i += 4) {
        // dst[i] = src[i];
        dst[i+1] = -1;
        dst[i+0] = -1;
        dst[i+3] = -1;
        dst[i+2] = -1;
    }
}

static void block_copy2(int* restrict a, size_t m, int* b, size_t n) {
    for (size_t i = 0; i < n && i < m; i += 2) {
        a[i+1] = 0;
        a[i+0] = 0;
        b[i+1] = -1;
        b[i+0] = -1;
    }
}

