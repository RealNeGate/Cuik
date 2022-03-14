#include <stdio.h>

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

int slow_fib(int n) {
	if (n < 2) return n;
	else return slow_fib(n - 1) + slow_fib(n - 2);
}

int main(void) {
	// 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 
	for (int i = 1; i <= 15; i++) {
		printf("%d ", fast_fib(i));
	}
	
	for (int i = 1; i <= 15; i++) {
		printf("%d ", slow_fib(i));
	}
	
	return 0;
}
