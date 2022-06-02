// "Finding and Understanding Bugs in C Compilers" crap
// Random bug in CompCert 1.6
#include <stdio.h>

_Bool lmao(_Bool a, _Bool b) {
	return a + b;
}

void baz(int* a, int* b) {
	*a = *b = 16;
}

int bar(unsigned x) {
	return -1 <= (1 && x);
}

int foo(void) {
	signed char x = 1;
	unsigned char y = 255;
	return x > y;
}

int main(void) {
	printf("%d ", foo());      // 0
	printf("%d ", bar(1));     // 1
	printf("%d ", bar(54897)); // 1
	printf("%d ", bar(128));   // 1
	printf("%d ", bar(-2u));   // 1
	printf("%d ", lmao(1, 0)); // 1
	printf("%d ", lmao(1, 1)); // 1
	return 0;
}
