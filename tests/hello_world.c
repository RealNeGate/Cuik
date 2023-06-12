//#Hello, World!
#include <stdio.h>

typedef char MuhString[15 - 1];

int main() {
    MuhString s = { "Hello, World!" };
    printf(s);
}
