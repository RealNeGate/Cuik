
long long do_stuff (long long value) {
	if (value < 0) {
		return 0;
	}
	return value;
}

struct Foo {
	long long x;
};

struct Foo do_stuff2(struct Foo value) {
	if (value.x < 0) {
		return 0;
	}
	return value.x;
}
