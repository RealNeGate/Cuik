#include <stdio.h>

// This is just my playground for testing TB's loop analysis/optimizations
int body(int i);

static int dumb_loop(int i, int n) {
    while (i < n) {
        body(i);
        i++;
    }
}

int mem_loop(int* arr, int i, int n) {
    int j = 0;
    while (i < n) {
        j += arr[i];
        i++;
    }
    return j;
}

int main() {
    int arr[3] = { 1, 5, 3 };
    printf("%d\n", mem_loop(arr, 0, 3));
}
