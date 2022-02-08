
// simple test of aliasing
int pointer_alias(int *x, int *y) {
	*x = 0;
	*y = 1;
	return *x;
}

int pointer_alias2(int* restrict x, int* restrict y) {
	*x = 0;
	*y = 1;
	return *x;
}
