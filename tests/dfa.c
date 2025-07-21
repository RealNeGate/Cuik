#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

int count_newlines(int* arr, int n) {
    int c = 0;
    for (int i = 0; i < n; i++) {
        c += arr[i] == 0;
    }
    return c;
}

