//#Hello, World!
/*#include <stdio.h>

typedef char MuhString[15 - 1];

int main() {
    MuhString s = { "Hello, World!" };
    printf(s);
}*/

int main() {
    __builtin_syscall(1, 1, "Hello, world!\n", 14);
    __builtin_syscall(60);
}
