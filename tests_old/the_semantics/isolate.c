
typedef struct {
	char data[7];
} Foo7;

/*void bar(Foo7* f) {
	*f = (Foo7){ { 69 } };
}*/

int foo(int a, int b) {
	return a && b;
}

int foo2(int a, int b, int c) {
	return a && b && c;
}

int bar(float a, float b) {
	return a > b;
}

int bar2(float a, float b) {
	return a < b;
}

extern void printf(const char* fmt, ...);

int main() {
	printf("1.0 > 0.0 = %d\n", bar(1.0, 0.0));
	printf("1.0 < 0.0 = %d\n", bar2(1.0, 0.0));
	
	/*printf("0 && 16 = %d\n", foo(0, 16));
	printf("16 && 8 = %d\n", foo(16, 8));
	printf("16 && 0 = %d\n", foo(16, 0));
	
	printf("0  && 8 && 4 = %d\n", foo2(0, 8, 4));
	printf("16 && 0 && 9 = %d\n", foo2(16, 0, 9));
	printf("16 && 8 && 0 = %d\n", foo2(16, 8, 0));
	
	printf("1  && 8 && 4 = %d\n", foo2(1, 8, 4));
	printf("16 && 1 && 9 = %d\n", foo2(16, 1, 9));
	printf("16 && 8 && 1 = %d\n", foo2(16, 8, 1));*/
	
	return 0;
}
