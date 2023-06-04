//#Hello, World!
#include <stdio.h>

int main() {
    MuhString s = { "Hello, World!" };
    printf(s);
}

typedef char MuhString[14];
