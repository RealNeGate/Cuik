
long long do_stuff(long long value) {
    if (value < 0) {
        return 0;
    }
    return value;
}

/*int without_restrict(int const* const x, int* y) {
    if(*x == 0) {
        *y = 1;
        return *x;
    }

    return 1;
}*/
