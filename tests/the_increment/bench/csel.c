
int do_stuff(int a, int b, int c) {
    int d = b + a;
    int e = b & c;
    int f = e - b;
    int g = d ^ f;
    return d + e + f + g;
}

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

long long ff(int* value) {
    return *value;
}

long long fg(int* value) {
    return ((long long) *value) & 63;
}

long long fh(int* value) {
    return fe(10);
}

int main() {}

/*int without_restrict(int const* const x, int* y) {
    if(*x == 0) {
        *y = 1;
        return *x;
    }

    return 1;
}*/
