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

int test(size_t nmemb, size_t size) {
    if(nmemb > SIZE_MAX/size) {
        return 0;
    }
    return 1;
}

int main() {
    return test(512, 1);
}
#endif