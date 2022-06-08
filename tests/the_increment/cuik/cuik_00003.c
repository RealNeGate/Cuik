#include <stdio.h>
#include <stdbool.h>

int main() {
	switch (20) {
		case 10 ... 20: printf("A"); break;
		default: break;
	}
	return 0;
}
