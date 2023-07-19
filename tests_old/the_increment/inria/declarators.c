extern int a1[];

void f1(int [*]);

char ((((*X))));

void (*signal(int, void (*)(int)))(int);

int aaaa, ***C, * const D, B(int);

int *A;

struct str;

void test2(int *P, int A) {
  struct str;
  int Array[*(int*)P+A];
}

struct xyz { int y; };
enum myenum { ASDFAS };
struct test10 { int a; } static test10x;
struct test11 { int a; } const test11x;
struct test13 { int a; } (test13x);


struct EnumBitfield {
  enum E2 { e2 } : 4; // ok
};

enum E11 { A1 = 1,};

int PR20634 = sizeof(struct { int n; } [5]);
