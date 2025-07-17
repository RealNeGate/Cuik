#include <stdint.h>

int pred();

static int bar(int i) {
    return i * 4;
}

static int baz(int i) {
    return i * 2;
}

int foo(void) {
    int i = 0;
    while (pred()) {
        i = bar(i);
    }
    return baz(i);
}
