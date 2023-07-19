#include <stdio.h>

int foo(int x) {
    return !(x == 0xD8);
}

int main() {
    printf("%d\n", foo(255));
    return 0;
}
