#include <stdio.h>
#include <assert.h>

int main() {
	assert(0 && "Woah!");
	
	printf("Hello, World!\n");
	return 0;
}
