
int foo(int *x, int *y) {
	*x = 0;
	*y = 1;
	return *x;
}

int foo2(int* restrict x, int* restrict y) {
	*x = 0;
	*y = 1;
	return *x;
}
