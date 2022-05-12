// Based on the IOCCC donut since i got some miscompilations from it
extern void printf(const char * fmt, ...);
extern void memset(void * dst, int val, unsigned long long n);
extern int putchar(int ch);
extern double cos(double x);
extern double sin(double x);

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
