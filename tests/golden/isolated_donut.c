// Based on the IOCCC donut since i got some miscompilations from it
#include <stdio.h>
#include <string.h>
#include <math.h>

const char string[] = ".,-~:;=!*#$@";

int main() {
	float A = 0, B = 0, i, j, z[1760];
	char b[1760];
  
	for (j = 0; 6.28 > j; j += 0.07) {
		memset(b, 32, 1760);
		memset(z, 0, 7040);
	
		printf("j=%f\n", (double)j);
		for (i = 0; 6.28 > i; i += 0.02) {
			printf("  i=%f\n", (double)i);
			float c = sin(i);
			float d = cos(j);
			float e = sin(A), f = sin(j), g = cos(A);
			float h = d + 2;
			float D = 1 / (c * h * e + f * g + 5);
			float l = cos(i), m = cos(B), n = sin (B);
			float t = c * h * g - f * e;
			
			int x = 40 + 30 * D * (l * h * m - t * n);
		}
	}
	
	return 0;
}
