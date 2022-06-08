#include <stdio.h>
#include <stdbool.h>

enum { A = sizeof(B) / sizeof(B[0]) };
int B[] = { 0, 1, 2 };

int main() {
	printf("%d", A);
	return 0;
}
