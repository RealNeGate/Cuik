

// rpc: LR

int foo(int x, int y) {
    int a = (x & y) + 1;
    int b = (x | y) + 2;
    int c = (x ^ y) + 3;
    int d = (x + y) + 4;
    int e = (x - y) + 5;
    int f = (x * y) + 6;
    int g = (x / y) + 7;
    // int h = x % y;
    int z = a + b + c + d + e + f + g;
    return z + 8;
}

/*float bar(float a) {
    a = a + 135.0f;
    a = a - 246.0f;
    a = a * 69.0f;
    a = a / 42.0f;
    return -a;
}*/

