#include <stdio.h>

int main(void) {
	for (int i = 0; i <= 100; i += 2) {
		printf("%d -> %f -> %f -> %d\n", i, (float)i, i + 0.25f, (int) (i * 0.33));
	}
	
	for (int j = 0; j < 20; j++) {
		double y = j / 20.0;
		
		for (int i = 0; i < 20; i++) {
			double x = i / 10.0;
			printf("%s ", x < y ? "X":" ");
		}
		printf("\n");
	}
	return 0;
}
