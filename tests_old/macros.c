
typedef struct {
    int(*bar)(int, int);
} Foo;

#define Foo_Bar(this, x, y) ((this)->bar(x, y))

int internal_foo_bar(int x, int y) { return x + y; }

int test(void) {
    Foo f = { .bar = internal_foo_bar };
    // return (f)->bar(16, 4);
    return Foo_Bar(&f, 16, 4);
}
