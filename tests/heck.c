
int foo(int64_t x) {
    return x;
}

typedef unsigned long long int64_t;

#if 0
typedef struct Foo Foo;

struct Foo {
    int x, y;
};

int bar(Foo x) {
    return x.x;
}

#else
typedef struct {
    int x, y;
} Foo;

Bar bar(Foo x) {
    return x;
}

typedef Foo Bar;

#endif
