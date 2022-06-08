typedef struct {
	int a, b, c, d;
} Foo;

int foo(const Foo f) {
    return f.a+f.b;
}

int bar(Foo f) {
    return f.a+f.b;
}

int baz(Foo* f) {
    return f.a+f.b;
}
