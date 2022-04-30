#define NULL ((void*)0)

T* da_global;
int I = 16;

enum { A, B, C };

struct Foo foo() {
	T a = 16, b = 5;
	T (*func)(T a, T b) = bar;
	
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

typedef int T;
