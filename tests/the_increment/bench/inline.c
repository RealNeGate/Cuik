#include <stdint.h>
#include <stdio.h>

int bar(int c);

int foo(int x, int y) {
    int c = (x * y);
    int g = 0;
    if (x > y) {
        g = bar(x * y);
    }

    return c + g;
}
