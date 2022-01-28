#include <stdio.h>

int main() {
	printf("Running tests!\n\n");
	
	pointer_test();
	div_test();
	sizeof_test();
	enum_test();
	string_test();
	
	return 0;
}

void div_test() {
	int a = divide3(150);
	printf("150 / 3 = %d (expected 50)\n", a);
	
	int b = modulo64(70);
	printf("70 %% 64 = %d (expected 6)\n", b);
	printf("\n");
}

int divide3(int x) {
	return x / 3;
}

int modulo64(int x) {
	return x % 64;
}

void pointer_test() {
	int x, y;
	int result;
	
	result = pointer_alias(&x, &y);
	printf("pointer_alias test 1: %d (expected 0)\n", result); 
	
	result = pointer_alias(&x, &x);
	printf("pointer_alias test 2: %d (expected 1)\n", result); 
	printf("\n");
}

int pointer_alias(int* x, int* y) {
	*x = 0;
	*y = 1;
	return *x;
}

void sizeof_test() {
	printf("sizeof(char)\t\t= %zu\n", sizeof(char));
	printf("sizeof(short)\t\t= %zu\n", sizeof(short));
	printf("sizeof(int)\t\t= %zu\n", sizeof(int));
	printf("sizeof(long)\t\t= %zu\n", sizeof(long));
	printf("sizeof(long long)\t= %zu\n", sizeof(long long));
	printf("sizeof(float)\t\t= %zu\n", sizeof(float));
	printf("sizeof(double)\t\t= %zu\n", sizeof(double));
	printf("sizeof(long double)\t= %zu\n", sizeof(long double));
	
	printf("sizeof(char*)\t\t= %zu\n", sizeof(char*));
	printf("sizeof(char[4])\t\t= %zu\n", sizeof(char[4]));
	printf("sizeof(\"Hello, World!\") = %zu\n", sizeof("Hello, World!"));
	printf("\n");
}

enum {
    E0,
    E1 = 2,
    E2 = 4,
    E3,
    E4,
};

enum test {
    E5 = 1000,
};

void enum_test() {
    enum test b1;
    printf("Enum:\tE0=%d E1=%d E2=%d E3=%d E4=%d E5=%d\n", E0, E1, E2, E3, E4, E5);
	
    b1 = 1;
    printf("\tb1=%d\n", b1);
}

static int strings_equal(const char* src, const char* dst, size_t dstlen)
{
    while (*src && dstlen-- && *dst)
    {
        if (*src++ != *dst++)
        {
            return 0;
        }
    }

    return (dstlen && *src == *dst) || (!dstlen && *src == 0);
}

void string_test() {
	const char str[] = "Hello";
	
	printf("\n");
	printf("String: %s\n", str);
	printf("Compare: %s == %s (%d)\n", "Hola", "Hola", strings_equal("Hola", "Hola", 4));
	printf("Compare: %s != %s (%d)\n", "Hello", "Bye", strings_equal("Hello", "Bye", 3));
	printf("Compare: %s != %s (%d)\n", "Goat", "Boat", strings_equal("Goat", "Boat", 4));
	printf("Compare: %s == %s (%d)\n", "Joat", "Joat", strings_equal("Joat", "Joat", 4));
}
