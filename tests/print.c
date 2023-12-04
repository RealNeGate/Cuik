#include <stdio.h>

int bar() {
    return 4 + 3 * 2 + 10;
}

int imm(void)   { return 69420; }
int mul3(unsigned x) { return x * 3; }

int bits(int x) { return x << 2; }

int foo(int x, int y) {
    printf("H");
    int z = (x + 2) / y;
    z += y;
    return z;
}

/*int loop(int* arr, int n) {
    int i = 0, a = 0;
    while (i < n) {
        if (i >= n) return -1;
        a += arr[i];
        i += 1;
    }

    return a;
}*/

int ctrl(int* x) {
    if (*x) {
        return 0;
    } else {
        *x -= 1;
        return 1;
    }
}

#if 0
int loop() {
    int a = 1;
    while (a < 10) {
        a = a + 3;
    }
    return a;
}

int single_if(int x, int y) {
    if (x && y) {
        return 1;
    }

    return 0;
}

int do_while(int x, int y) {
    if (x < y) {
        do {
            x += 1;
        } while(x < y);
    }

    return x;
}
#endif
