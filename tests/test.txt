typedef int Index;

void ExitProcess(unsigned int code);
int foo() { return 16; }
int bar() { int x = 16; return x++; }
int baz() { int* x = (void*)0; x++; x--; x += 16; x -= 16; *x++ = 16; return *x; }
Index foo2() { return 16; }
int params(int x, int y) { return x + y; }

int baz2(int *a, int *b)
{
    *a = 5;
    *b = 6;
    return *a + *b;
}

int bar2(int x) {
	int y = x + 1, z = x + 2;
	int w, a;
	return y + z;
}

int sum_of(int n, int* arr) {
	int i = 0;
	int sum = 0;
	while (i < n) {
		sum += arr[i];
	}
	return sum;
}

int sum_of2(int n, int* arr) {
	int sum = 0;
	for (int i = 0; i < n; i++) {
		sum += arr[i];
	}
	return sum;
}

int fast_fib(int n) {
    int a = 0;
	int b = 1;

	int i = n - 1;
    while(i--) {
		int c = a + b;
		a = b;
		b = c;
	}

	return b;
}

int main(int argc, char* argv[]) {
	int a = params(1, 56);
	ExitProcess(0);

	int x = 16;
	int* ptr = &x;
	int** ptr2 = &ptr;
	int val = **ptr2;

	x = 640;
	x += 16;
	x -= 16;
	x *= 64;
	x /= 16;
	x &= 16;
	x |= 16;
	x ^= 16;
	x <<= 16;

	//struct Point* start = 0;
	//start->x = 16;

	int y = (x * 2);
	if (y) y = 16;
	if (x) { y = 16; } else { x = 16; }

	while (x) { x -= 1; }

	short apple[16];
	apple[0] = 16;

	char table[8][8];
	char z = (table[0][0] + 1);

	int w = 4;
	do { table[z][z] = 16; } while (w--);

	if (x && z) {
		x = 16;
	}

	return y;
}
