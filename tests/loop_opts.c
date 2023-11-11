#include <stdio.h>

// This is just my playground for testing TB's loop analysis/optimizations
int body(int i);

int dumb_loop(int i, int n) {
    while (i < n) {
        body(i);
        i++;
    }
}

static int main() {
    int arr[3] = { 1, 5, 3 };
    printf("%d\n", dumb_loop(arr, 0, 3));
}
