

// rpc: LR

int foo(int x, int y) {
    int a = x & y;
    int b = x ^ y;
    int c = a | b;
    return /*a ? b :*/ c;
    // int b = x | y;
    // int c = x ^ y;
    // int d = x + y;
    // int e = a - b;
    // int f = c * d;
    // return e % f;
    // return x + y + 1;
}

