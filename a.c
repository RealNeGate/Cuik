#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

int foo(void) {
    int a = 1, c = 123700, g = 0, h = 0;
    for (int b = 106700; b <= c; b += 17) {
        printf("B=%d (%d / 1000)\n", b, (b - 106700) / 17);

        int f = 1;
        for (int d = 2; d != b; d++) {
            if (b % d == 0) { h += 1; break; }
        }
    }

    return h;
}

int main() {
    /* pthread_t* arr = malloc(num_threads * sizeof(pthread_t));
    for (int i = 0; i < num_threads; i++) {
        pthread_create(&arr[i], NULL, test_thread_fn, (void*) (uintptr_t) i);
    }
    for (int i = 0; i < num_threads; i++) {
        pthread_join(arr[i], NULL);
    } */

    printf("H = %d\n", foo());
}
