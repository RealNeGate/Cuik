
struct Foo {
	int a, b;
};

// These are the fun extensions :)
int bar(struct Foo* f) {
	return f.a + f.b;
}
