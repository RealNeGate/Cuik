#include <stdio.h>

int main() {
    int i = 0;
    for (;;) {
        printf("%d\n", i);
        i = (i + 1) % 1000;
    }
}

