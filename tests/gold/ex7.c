

// rpc: LR

int foo(int x, int y) {
    int a = (x & y) + 1;
    int b = (x | y) + 2;
    int c = (x ^ y) + 3;
    int d = (x + y) + 4;
    int e = (x - y) + 5;
    int f = (x * y) + 6;
    int g = (x / y) + 7;
    int h = (x << y) + 8;
    int i = (x >> y) + 9;
    int j = x % y;
    int z = a + b + c + d + e + f + g + h + i + j;
    return -z;
}

float bar(float a) {
    a = a + 135.0f;
    a = a - 246.0f;
    a = a * 69.0f;
    a = a / 42.0f;
    return -a;
}

int baz(int a, float b) {
    return (int)(a + b);
}

unsigned int rotate(unsigned int a) {
    return (a << 12) | (a >> 20);
}
