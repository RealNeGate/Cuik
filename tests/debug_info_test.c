#include <stdio.h>

int slow_fib(int n) {
    if (n < 2) return n;
    else return slow_fib(n - 1) + slow_fib(n - 2);
}

int foo(int n) {
    if (n == 0) {
        return 0;
    }

    return foo(n - 1);
}

int main(void) {
    long long x = 16;
    printf("Hello, World! %lld\n", x);

    int f = slow_fib(10);
    printf("Fib(10) = %d\n", f);

    foo(10);
}
