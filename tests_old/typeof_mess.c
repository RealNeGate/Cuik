
#define typeof(T) _Typeof(T)

typedef typeof(func2) Foo;

int main() {
	Foo apple = func2;
	
	return apple(1, 2);
}

int func(int a, int b) {
	return a + b;
}

int (*func2)(int a, int b) = func;
