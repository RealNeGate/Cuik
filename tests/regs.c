
#include <stdio.h>

int foo() {
    int i = 0;
    if (i < 10) {
        do {
            i++;
        } while (i < 10);
    }

    return printf("Hello %d\n", i);
}

int main() {
    return foo();
}
