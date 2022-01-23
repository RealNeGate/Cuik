
// simple test of aliasing
// foo can alias so we
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

// this example shows how
