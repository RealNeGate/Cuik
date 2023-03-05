
/*int do_stuff(void) {
    return 3 + 16;
}*/

int do_stuff(int a, int b, int c) {
    int d = b + a;
    int e = b - c;
    int f = e & b;
    int g = d ^ f;
    return d + e + f + g;
}

#if 0
int fa(int value) {
    return value;
}

int* fb(int value) {
    return &value;
}

int fc(int value) {
    return value + 16 - 5 & 3;
}

int fd(int value) {
    if (value - 10) return ~value;

    return value;
}

int fe(int value) {
    int sum = 0;
    while (value--) {
        sum += value;
    }
    return sum;
}
#endif

/*int without_restrict(int const* const x, int* y) {
    if(*x == 0) {
        *y = 1;
        return *x;
    }

    return 1;
}*/

