#include <assert.h>

int foo() {
    int x = 5 + 5;
    int y = 2 * 5 + 5;
    assert(x && "foo");

    return x * y;
}


int ptr(int k) {
    static int a, b;
    return !!(k ? &a : &b);
}
