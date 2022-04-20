// Based on:
// https://github.com/rui314/chibicc/blob/main/test/generic.c
#include <stdio.h>

int main() {
	printf("%d ", _Generic(100.0, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic((int *)0, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic((int[3]){}, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic(100, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic(100f, double: 1, int *: 2, int: 3, float: 4));

	printf("OK\n");
	return 0;
}
