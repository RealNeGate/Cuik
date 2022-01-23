#define STBI_NO_SIMD
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
//#include <stdio.h>

int main() {
	int x,y,n;
	stbi_load("test.png", &x, &y, &n, 0);
	printf(stbi_failure_reason());
	printf("Hello! %d, %d, %d", x, y, n);
	return 0;
}
