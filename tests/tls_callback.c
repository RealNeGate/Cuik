#include <threads.h>
#include <stdlib.h>
#include <stdio.h>

void dtor(void* dat) {
    // not called in this program
    printf("Hello! %p\n", dat);
}

static tss_t t;

int main(void) {
    if(tss_create(&t, dtor) != thrd_success) {
        return 1;
    }
    if(tss_set(t, (void*)42) != thrd_success) {
        return 1;
    }
    if(tss_get(t) != (void*)42) {
        return 1;
    }
    return 0;
}