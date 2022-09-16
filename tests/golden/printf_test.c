#include <stdio.h>

int main(int argc, char* argv[]) {
	// 7.21.6.1 The fprintf function
	printf("%s ", "Hello");
	printf("%.*s %.5s ", 3, "Hello", "Goodbye");
	printf("%hhd %hhd %hhd ", +127, 63, 0);
	printf("%hhu %hhu %hhu ", 254, 63, 0);
	printf("%hd %hd %hd %hd ", 32000, 32767, 4, 17);
	printf("%hu %hu %hd %hd ", 65532, 65530, 4, 16);
	printf("%d %d %d %d ", 32000, 32767, 4, 17);
	printf("%u %u %u %u ", 65532, 65530, 4, 16);
	printf("%lu %lu %ld %ld ", 4294967295u, 6731943u, 2147483646, 16);
	printf("%llu %llu %lld %lld ", 123456789ull, 57486731943ull, 985429ll, 9123456ll);
	
	//printf("%f %f %.03f %.3f %f", 1.0, 123e+3f, 1e-1, 0.234f, 3.0f);
	printf("%f%.03f %.3f", 1.0, 0.234f, 3.0f);
	
	return 0;
}
