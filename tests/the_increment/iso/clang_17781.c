int printf(const char *, ...);

struct S0 { int f0, f1, f2, f3, f4; } b = {0,0,1,0,0};

int a;
void foo(struct S0 p) {
	b.f2 = 0;
	if (p.f2) a = 1;
}

int main() {
	foo(b);
	printf("%d", a);
	return 0;
}
