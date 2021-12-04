

int fast_fib(int n) {
    int a = 0;
	int b = 1;

	int i = n - 1;
    while (i--) {
		int c = a + b;
		a = b;
		b = c;
	}

	return b;
}

