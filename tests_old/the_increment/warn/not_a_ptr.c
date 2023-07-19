#define SOME_MACRO(x) *x

int square(int num) {
    return SOME_MACRO(1);
}
