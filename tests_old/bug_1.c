#if 0
int foo(int c, int d);

int foo(int x, int y) {
    return x + y;
}

int foo(int a, int b);

int main() {
    return foo(1, 5);
}
#else
#include <limits.h>
#include <stdio.h>
#include <stdint.h>

int test(size_t x) {
    if(x > SIZE_MAX) {
        return 0;
    }
    return 1;
}

int test2(size_t x, size_t y) {
    if(x > SIZE_MAX/y) {
        return 0;
    }
    return 1;
}

int main() {
    printf("%d\n", test2(512, 1));
    return 0;
}
#endif