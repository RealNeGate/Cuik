#include <stddef.h>
#include <string.h>
#include <stdio.h>

int cse_test(int x, int y) {
    return (x*y) + (x*y);
}

int libcalls_test(char* y) {
    int x;
    memcpy(&x, y, sizeof(int));
    return x;
}

int baz(int* arr, int n) {
    int sum = 0;
    for (size_t i = 0; i < n; i++) {
        sum += arr[n - i - 1];
    }

    return sum;
}

unsigned int foo(unsigned int x) {
    return x / 3;
}

int bar(int x) {
    return ((x + 1) * 4) * 0;
}

#if 0
int arr[] = { 1, 2, 3, 4 };

int main() {
    printf("%d\n", baz(arr, 4));
    return 0;
}
#endif

