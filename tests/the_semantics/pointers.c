#include <stddef.h>
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


// tests with char
char* foo1(char* a, char* b) { return a - b; }
char* foo2(char* a, intptr_t b) { return &a[b]; }
char* foo3(char* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo4(char* a, char* b) { return &a[b - a] == b; }
char foo5(char**** a) { return ****a; }
char foo6(char*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
char foo7(char* a, size_t b) { return *((char*) ((uintptr_t) (b + a))); }
uintptr_t foo8(char* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with short
short* foo9(short* a, short* b) { return a - b; }
short* foo10(short* a, intptr_t b) { return &a[b]; }
short* foo11(short* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo12(short* a, short* b) { return &a[b - a] == b; }
short foo13(short**** a) { return ****a; }
short foo14(short*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
short foo15(short* a, size_t b) { return *((short*) ((uintptr_t) (b + a))); }
uintptr_t foo16(short* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with int
int* foo17(int* a, int* b) { return a - b; }
int* foo18(int* a, intptr_t b) { return &a[b]; }
int* foo19(int* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo20(int* a, int* b) { return &a[b - a] == b; }
int foo21(int**** a) { return ****a; }
int foo22(int*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
int foo23(int* a, size_t b) { return *((int*) ((uintptr_t) (b + a))); }
uintptr_t foo24(int* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with long long
long long* foo25(long long* a, long long* b) { return a - b; }
long long* foo26(long long* a, intptr_t b) { return &a[b]; }
long long* foo27(long long* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo28(long long* a, long long* b) { return &a[b - a] == b; }
long long foo29(long long**** a) { return ****a; }
long long foo30(long long*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
long long foo31(long long* a, size_t b) { return *((long long*) ((uintptr_t) (b + a))); }
uintptr_t foo32(long long* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo7
Foo7* foo33(Foo7* a, Foo7* b) { return a - b; }
Foo7* foo34(Foo7* a, intptr_t b) { return &a[b]; }
Foo7* foo35(Foo7* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo36(Foo7* a, Foo7* b) { return &a[b - a] == b; }
Foo7 foo37(Foo7**** a) { return ****a; }
Foo7 foo38(Foo7*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
Foo7 foo39(Foo7* a, size_t b) { return *((Foo7*) ((uintptr_t) (b + a))); }
uintptr_t foo40(Foo7* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo8
Foo8* foo41(Foo8* a, Foo8* b) { return a - b; }
Foo8* foo42(Foo8* a, intptr_t b) { return &a[b]; }
Foo8* foo43(Foo8* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo44(Foo8* a, Foo8* b) { return &a[b - a] == b; }
Foo8 foo45(Foo8**** a) { return ****a; }
Foo8 foo46(Foo8*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
Foo8 foo47(Foo8* a, size_t b) { return *((Foo8*) ((uintptr_t) (b + a))); }
uintptr_t foo48(Foo8* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo14
Foo14* foo49(Foo14* a, Foo14* b) { return a - b; }
Foo14* foo50(Foo14* a, intptr_t b) { return &a[b]; }
Foo14* foo51(Foo14* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo52(Foo14* a, Foo14* b) { return &a[b - a] == b; }
Foo14 foo53(Foo14**** a) { return ****a; }
Foo14 foo54(Foo14*** a, size_t b, size_t c, size_t d) { return a[b][c][d]; }
Foo14 foo55(Foo14* a, size_t b) { return *((Foo14*) ((uintptr_t) (b + a))); }
uintptr_t foo56(Foo14* a, size_t b[2], int c, size_t d[2], int e) { return (uintptr_t)&a[b[c] - d[e]]; }

// tests with Foo24
Foo24* foo57(Foo24* a, Foo24* b) { return a - b; }
Foo24* foo58(Foo24* a, intptr_t b) { return &a[b]; }
Foo24* foo59(Foo24* a, intptr_t* b, intptr_t c) { return &a[b[c]]; }
bool foo60(Foo24* a, Foo24* b) { return &a[b - a] == b; }
Foo24 foo61(Foo24**** a) { return ****a; }
