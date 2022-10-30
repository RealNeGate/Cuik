// local_typedef.c
typedef int T1;      // Declaration of type T1 as int
void f(void) {
  typedef int *T2;   // Declaration of type T2 as pointer to int
  T1 x1;             // Declaration of x1 of type T1
  T2 x2;             // Declaration of x2 of type T2
  x1 = 0;
  x2 = 0;
}
