#include <stdio.h>

int main() {
	printf("Hello, World!\n");
	int a = 16;
	struct { int c; } b = { 16 };
	
	int c = a + b;

	return 0;
}
