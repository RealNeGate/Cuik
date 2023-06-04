#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

// odd structures
typedef struct {
	char data[7];
} Foo7;

typedef struct {
	char data[8];
} Foo8;

typedef struct {
	char data[14];
} Foo14;

typedef struct {
	char data[24];
} Foo24;


// tests with int8_t
ptrdiff_t foo0_int8_t(int8_t* a, int8_t* b) { return a - b; }
int8_t* foo1_int8_t(int8_t* a, intptr_t b) { return &a[b]; }
int8_t* foo2_int8_t(int8_t* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_int8_t(int8_t* a, int8_t* b) { return &a[b - a] == b; }
int8_t foo4_int8_t(int8_t**** a) { return ****a; }
int8_t foo5_int8_t(int8_t*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
int8_t foo6_int8_t(int8_t* a, size_t b) { return *((int8_t*) ((uintptr_t) (b + a))); }
uintptr_t foo7_int8_t(int8_t* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with int16_t
ptrdiff_t foo0_int16_t(int16_t* a, int16_t* b) { return a - b; }
int16_t* foo1_int16_t(int16_t* a, intptr_t b) { return &a[b]; }
int16_t* foo2_int16_t(int16_t* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_int16_t(int16_t* a, int16_t* b) { return &a[b - a] == b; }
int16_t foo4_int16_t(int16_t**** a) { return ****a; }
int16_t foo5_int16_t(int16_t*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
int16_t foo6_int16_t(int16_t* a, size_t b) { return *((int16_t*) ((uintptr_t) (b + a))); }
uintptr_t foo7_int16_t(int16_t* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with int32_t
ptrdiff_t foo0_int32_t(int32_t* a, int32_t* b) { return a - b; }
int32_t* foo1_int32_t(int32_t* a, intptr_t b) { return &a[b]; }
int32_t* foo2_int32_t(int32_t* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_int32_t(int32_t* a, int32_t* b) { return &a[b - a] == b; }
int32_t foo4_int32_t(int32_t**** a) { return ****a; }
int32_t foo5_int32_t(int32_t*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
int32_t foo6_int32_t(int32_t* a, size_t b) { return *((int32_t*) ((uintptr_t) (b + a))); }
uintptr_t foo7_int32_t(int32_t* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with int64_t
ptrdiff_t foo0_int64_t(int64_t* a, int64_t* b) { return a - b; }
int64_t* foo1_int64_t(int64_t* a, intptr_t b) { return &a[b]; }
int64_t* foo2_int64_t(int64_t* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_int64_t(int64_t* a, int64_t* b) { return &a[b - a] == b; }
int64_t foo4_int64_t(int64_t**** a) { return ****a; }
int64_t foo5_int64_t(int64_t*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
int64_t foo6_int64_t(int64_t* a, size_t b) { return *((int64_t*) ((uintptr_t) (b + a))); }
uintptr_t foo7_int64_t(int64_t* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo7
ptrdiff_t foo0_Foo7(Foo7* a, Foo7* b) { return a - b; }
Foo7* foo1_Foo7(Foo7* a, intptr_t b) { return &a[b]; }
Foo7* foo2_Foo7(Foo7* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_Foo7(Foo7* a, Foo7* b) { return &a[b - a] == b; }
Foo7 foo4_Foo7(Foo7**** a) { return ****a; }
Foo7 foo5_Foo7(Foo7*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
Foo7 foo6_Foo7(Foo7* a, size_t b) { return *((Foo7*) ((uintptr_t) (b + a))); }
uintptr_t foo7_Foo7(Foo7* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo8
ptrdiff_t foo0_Foo8(Foo8* a, Foo8* b) { return a - b; }
Foo8* foo1_Foo8(Foo8* a, intptr_t b) { return &a[b]; }
Foo8* foo2_Foo8(Foo8* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_Foo8(Foo8* a, Foo8* b) { return &a[b - a] == b; }
Foo8 foo4_Foo8(Foo8**** a) { return ****a; }
Foo8 foo5_Foo8(Foo8*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
Foo8 foo6_Foo8(Foo8* a, size_t b) { return *((Foo8*) ((uintptr_t) (b + a))); }
uintptr_t foo7_Foo8(Foo8* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo14
ptrdiff_t foo0_Foo14(Foo14* a, Foo14* b) { return a - b; }
Foo14* foo1_Foo14(Foo14* a, intptr_t b) { return &a[b]; }
Foo14* foo2_Foo14(Foo14* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_Foo14(Foo14* a, Foo14* b) { return &a[b - a] == b; }
Foo14 foo4_Foo14(Foo14**** a) { return ****a; }
Foo14 foo5_Foo14(Foo14*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
Foo14 foo6_Foo14(Foo14* a, size_t b) { return *((Foo14*) ((uintptr_t) (b + a))); }
uintptr_t foo7_Foo14(Foo14* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo24
ptrdiff_t foo0_Foo24(Foo24* a, Foo24* b) { return a - b; }
Foo24* foo1_Foo24(Foo24* a, intptr_t b) { return &a[b]; }
Foo24* foo2_Foo24(Foo24* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo3_Foo24(Foo24* a, Foo24* b) { return &a[b - a] == b; }
Foo24 foo4_Foo24(Foo24**** a) { return ****a; }
Foo24 foo5_Foo24(Foo24*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
Foo24 foo6_Foo24(Foo24* a, size_t b) { return *((Foo24*) ((uintptr_t) (b + a))); }
uintptr_t foo7_Foo24(Foo24* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }


int main() {
	// tests with int8_t
	{
		int8_t arr[20];
		
		arr[4] = (int8_t){ 69 };
		
		ptrdiff_t test0 = foo0_int8_t(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_int8_t (got %zu; expected 4)\n", test0); abort(); }
		
		int8_t* test1 = foo1_int8_t(arr, 4);
		int8_t test1_deref = *test1;
		if (test1_deref != 69) { printf("test fail: foo1_int8_t (got %zu; expected 69)\n", (size_t)test1_deref); abort(); }
		
	}

	// tests with int16_t
	{
		int16_t arr[20];
		
		arr[4] = (int16_t){ 69 };
		
		ptrdiff_t test0 = foo0_int16_t(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_int16_t (got %zu; expected 4)\n", test0); abort(); }
		
		int16_t* test1 = foo1_int16_t(arr, 4);
		int16_t test1_deref = *test1;
		if (test1_deref != 69) { printf("test fail: foo1_int16_t (got %zu; expected 69)\n", (size_t)test1_deref); abort(); }
		
	}

	// tests with int32_t
	{
		int32_t arr[20];
		
		arr[4] = (int32_t){ 69 };
		
		ptrdiff_t test0 = foo0_int32_t(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_int32_t (got %zu; expected 4)\n", test0); abort(); }
		
		int32_t* test1 = foo1_int32_t(arr, 4);
		int32_t test1_deref = *test1;
		if (test1_deref != 69) { printf("test fail: foo1_int32_t (got %zu; expected 69)\n", (size_t)test1_deref); abort(); }
		
	}

	// tests with int64_t
	{
		int64_t arr[20];
		
		arr[4] = (int64_t){ 69 };
		
		ptrdiff_t test0 = foo0_int64_t(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_int64_t (got %zu; expected 4)\n", test0); abort(); }
		
		int64_t* test1 = foo1_int64_t(arr, 4);
		int64_t test1_deref = *test1;
		if (test1_deref != 69) { printf("test fail: foo1_int64_t (got %zu; expected 69)\n", (size_t)test1_deref); abort(); }
		
	}

	// tests with Foo7
	{
		Foo7 arr[20];
		
		arr[4] = (Foo7){ 69 };
		
		ptrdiff_t test0 = foo0_Foo7(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_Foo7 (got %zu; expected 4)\n", test0); abort(); }
		
		Foo7* test1 = foo1_Foo7(arr, 4);
		Foo7 test1_deref = *test1;
		if (test1_deref.data[0] != 69) { printf("test fail: foo1_Foo7 (got %zu; expected 69)\n", (size_t)test1_deref.data[0]); abort(); }
		
	}

	// tests with Foo8
	{
		Foo8 arr[20];
		
		arr[4] = (Foo8){ 69 };
		
		ptrdiff_t test0 = foo0_Foo8(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_Foo8 (got %zu; expected 4)\n", test0); abort(); }
		
		Foo8* test1 = foo1_Foo8(arr, 4);
		Foo8 test1_deref = *test1;
		if (test1_deref.data[0] != 69) { printf("test fail: foo1_Foo8 (got %zu; expected 69)\n", (size_t)test1_deref.data[0]); abort(); }
		
	}

	// tests with Foo14
	{
		Foo14 arr[20];
		
		arr[4] = (Foo14){ 69 };
		
		ptrdiff_t test0 = foo0_Foo14(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_Foo14 (got %zu; expected 4)\n", test0); abort(); }
		
		Foo14* test1 = foo1_Foo14(arr, 4);
		Foo14 test1_deref = *test1;
		if (test1_deref.data[0] != 69) { printf("test fail: foo1_Foo14 (got %zu; expected 69)\n", (size_t)test1_deref.data[0]); abort(); }
		
	}

	// tests with Foo24
	{
		Foo24 arr[20];
		
		arr[4] = (Foo24){ 69 };
		
		ptrdiff_t test0 = foo0_Foo24(&arr[8], &arr[4]);
		if (test0 != 4) { printf("test fail: foo0_Foo24 (got %zu; expected 4)\n", test0); abort(); }
		
		Foo24* test1 = foo1_Foo24(arr, 4);
		Foo24 test1_deref = *test1;
		if (test1_deref.data[0] != 69) { printf("test fail: foo1_Foo24 (got %zu; expected 69)\n", (size_t)test1_deref.data[0]); abort(); }
		
	}

	printf("success!\n");
	return 0;
}