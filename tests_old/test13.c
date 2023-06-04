#define STBI_NO_SIMD
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
//#include <stdio.h>

static void handle_stuff() {
	
}

int main() {
	printf("Hello, World! %d, %d, %d, %d, %d, %d", 16, 32, 64, 128, 256, 512);
	putchar('\n');
	
	const char* path = "./test.png";
	printf("Trying to load image! %s", path);
	putchar('\n');
	
	int x,y,n;
	stbi_load(path, &x, &y, &n, 0);
	printf(stbi_failure_reason());
	putchar('\n');
	
	printf("Results: %d, %d, %d", x, y, n);
	putchar('\n');
	return 0;
}
