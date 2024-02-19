#include <stddef.h>
#include <stdint.h>

typedef struct {
    int type : 3;
    int val  : 29;
} Ref;

/* void foo(int n, int* arr) {
    for (int i = 0; i < 2; i++) {
        arr[i] += 1;
    }
} */

void matmul(float* c, float* a, float* b) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            float sum = 0.0f;
            for (int k = 0; k < 4; k++) {
                sum += a[k*4 + i] * b[j*4 + k];
            }
            c[j*4 + i] = sum;
        }
    }
}

#if 0
int ref_eq(Ref a, Ref b) {
    return a.type == b.type && a.val == b.val;
}

void foo(uint64_t* dst) {
    *dst += 5;
}
#elif 0
void foo(int n, int* arr) {
    for (int i = 2; i < n; i++) {
        if (arr[n] != 0) {
            arr[0] += arr[i];
        } else {
            arr[1] += 1;
        }
    }
}
#endif

#if 0
int  stuff_a();
void stuff_b();
void stuff_c();
void stuff_d(int);

int example(void) {
    int i = 10;
    while (stuff_a()) {
        if (i == 10) {
            stuff_b();
        } else {
            stuff_c();
            i += 1;
        }
    }

    return i * 1000 * 1000 * 10;
}

int main() {
    size_t iter = 0;
    size_t stop = 1000 * 1000 * 10;
    size_t step = 1;
    size_t *ptr_iter = &iter;
    size_t *ptr_stop = &stop;
    size_t *ptr_step = &step;
    size_t **ptr_ptr_iter = &ptr_iter;
    size_t **ptr_ptr_stop = &ptr_stop;
    size_t **ptr_ptr_step = &ptr_step;
    size_t ***ptr_ptr_ptr_iter = &ptr_ptr_iter;
    size_t ***ptr_ptr_ptr_stop = &ptr_ptr_stop;
    size_t ***ptr_ptr_ptr_step = &ptr_ptr_step;
    while (***ptr_ptr_ptr_iter < ***ptr_ptr_ptr_stop) {
        ***ptr_ptr_ptr_iter += 1;
    }

    ***ptr_ptr_ptr_iter += 1;
    return ***ptr_ptr_ptr_iter;
}

void foo() {
    int x=1;
    if( false ) {
        while( true ) { x++; }
        stuff_d(x);
    }
}
#endif