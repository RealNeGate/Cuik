
int foo(int* x) {
    return *x > 4 && *x < 10;
}

int bar(int* x, int* y) {
    return *x <= 256 || *y >= 4;
}

int bar2(int x, int y) {
    return x <= 256 || y >= 4;
}

extern void baz(void);
int bar3(const char* regex) {
    while(*regex && *regex != ')') {
        baz();
        regex++;
    }

    return 0;
}

