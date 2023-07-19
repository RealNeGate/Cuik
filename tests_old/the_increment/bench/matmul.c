#include <stddef.h>

typedef size_t index_t;
#define FOR_N(i, n) for (index_t i = 0, n_ = (n); i < n_; i++)

void foo(float* restrict c, float* restrict a, float* restrict b) {
    // let's walk different refinements of the types
    //   TB pointers are opaque
    //     c = [? * ?]          #restrict #aligned(4)
    //
    //   but since all stores were floats
    //     c = [? * float]      #restrict #aligned(4)
    //
    //   we also know that both i & j span 0..3 so
    //     c = [4 * 4 * float]  #restrict #aligned(4)
    //
    FOR_N(i, 4) {
        FOR_N(j, 4) {
            float x = 0.0f;
            FOR_N(k, 4) x += a[i + k*4] * b[k + j*4];

            c[i + j*4] = x;
        }
    }
}
