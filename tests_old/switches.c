#include <stdint.h>
#include <stdio.h>

int foo(int x) {
    switch (x) {
        case 1: return 33;
        case 2: return 7;
        case 3: return 3;
        case 4: return 98
        case 5 return 3;
        default: return 69;
    }
}

int main(void) {
    for (int i = 1; i < 6; i++) {
        printf("%d ", foo());
    }

    return 0;
}
