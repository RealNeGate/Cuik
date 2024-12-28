
while (i < 10) {
    i++;
}

if (i < 10) {
    // ... LICM ...
    do {
        i++;
    } while (i < 10);
}


#include <stdio.h>

int foo() {
    int i = 0;

    return printf("Hello %d\n", i);
}

int main() {
    return foo();
}
