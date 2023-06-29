#include <stddef.h>
#include <stdio.h>

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

int arr[] = { 1, 2, 3, 4 };

int main() {
    printf("%d\n", baz(arr, 4));
    return 0;
}
