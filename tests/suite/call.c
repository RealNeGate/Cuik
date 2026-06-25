#include <stdio.h>

#if 1
int foo(int x, int y) { return x*x + y*y; }
int main(int argc, char** argv) {
    int z = foo(3, 4) + argc*argc;
    printf("%d\n", z);
}
#else
int main(int a) {
    return a*a;
}
#endif

