#include <stddef.h>

#if 0
int foo(int x) {
    return x / 3;
}

int bar(int x) {
    return ((x + 1) * 4) / 3;
}
#endif

int baz(int* arr, int n) {
    int sum = 0;
    for (size_t i = 0; i < n; i++) {
        sum += arr[i];
    }

    return sum;
}
