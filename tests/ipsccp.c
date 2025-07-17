#include <stdint.h>

int pred();

static int bar(int i) {
    return i * 4;
}

static int baz(int i) {
    return i * 2;
}

int foo(void) {
    int i = 0, j = 0;
    while (j++ < 100) {
        i = bar(i);
    }
    return baz(i);
}
