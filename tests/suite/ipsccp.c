#include <stdint.h>
#include <stdio.h>

int pred();

static int bar(int i) {
    return i * 4;
}

static int baz(int i) {
    return i * 2;
}

int main(void) {
    int i = 0, j = 0;
    while (j++ < 100) {
        i = bar(i);
    }
    printf("foo = %d\n", baz(i));
}
