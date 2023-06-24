#include <stddef.h>

int foo(int x) {
    return x / 3;
}

int bar(int x) {
    return ((x + 1) * 4) * 0;
}

int baz(int* arr, int n) {
    int sum = 0;
    for (size_t i = 0; i < n; i++) {
        sum += arr[n - i - 1];
    }

    return sum;
}
