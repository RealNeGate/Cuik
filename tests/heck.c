
int foo(int64_t x) {
    return x;
}

typedef unsigned long long int64_t;

typedef struct {
    int x, y;
} Foo;

typedef Foo Bar;

Bar bar(Foo x) {
    return x;
}
