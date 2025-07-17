
extern int foo();
extern int baz();
extern int bar();
extern int pred();

int foo() {
    int i = bar(), j = 1;
    if (baz()) {
        goto mid;
    }

    while (pred()) {
        mid:
        i *= j;
    }

    return i;
}
