#include <stddef.h>

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

#if 0
void foo() {
    int x=1;
    if( false ) {
        while( true ) { x++; }
        stuff_d(x);
    }
}
#endif