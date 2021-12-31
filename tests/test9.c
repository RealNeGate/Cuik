
// These are the fun extensions :)
struct Foo {
	int a, b;
};

int bar(struct Foo* f) {
	// this should only compile with -P (pedantic) disabled.
	return f.a + f.b;
}
