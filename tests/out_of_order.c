#define NULL ((void*)0)
#warning "Hello"
#define FOO(a, b) (a + b)

T* da_global;
int I = 16;

enum { A, B, C };

struct Foo foo() {
    T a = 16, b = 5;
    T (*func)(T a, T b) = bar;

    int zz = FOO(5, zzz);

    struct Foo f = { 1, 2, NULL };
    return f;
}

T bar(T a, T b);

struct Foo {
    T a, b;
    void* ptr;
};

struct Baz {
    struct Foo f;
};

static_assert(sizeof(T) != 2, "Wack");
static_assert(sizeof(T) != 8, "Woah");

typedef int T;
