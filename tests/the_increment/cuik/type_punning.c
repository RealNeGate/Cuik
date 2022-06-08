#include <stdio.h>
#include <stdint.h>

float foo(unsigned int x) {
    return *((float*) &x);
}

unsigned int bar(float x) {
    return *((unsigned int*) &x);
}

int main() {
	printf("%f\n", foo(0x3fc00000u));
	printf("%x\n", bar(1.5f));
	
	unsigned int x = 0x3f800000u;
	float y = foo(x);
	unsigned int z = bar(y);
	printf("%x %f %x\n", x, y, z);
	return 0;
}
