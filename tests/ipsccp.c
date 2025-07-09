#include <stdint.h>

int pred();

static int bar(int i) {
    return i * 4;
}

int foo(void) {
    int i = 0;
    while (pred()) {
        i = bar(i);
    }
    return i;
}
