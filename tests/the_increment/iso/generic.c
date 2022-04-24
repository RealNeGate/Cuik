// Based on:
// https://github.com/rui314/chibicc/blob/main/test/generic.c
extern int printf(const char* format, ...);
extern int puts(const char* str);
extern void exit(int status);

int hack() { return 42; }

int main() {
	puts("Hello, World!");
	
	printf("%d ", _Generic(100.0, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic((int *)0, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic((int[3]){}, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic(100, double: 1, int *: 2, int: 3, float: 4));
	printf("%d ", _Generic(100.0f, double: 1, int *: 2, int: 3, float: 4));

	printf("OK\n");
	return 0;
}
