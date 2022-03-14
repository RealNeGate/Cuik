// namespaces.c
typedef int S, T, U;
struct S { int T; };
union U { int x; };
void f(void) {
  // The following uses of S, T, U are correct, and have no
  // effect on the visibility of S, T, U as typedef names.
  struct S s = { .T = 1 };
  T: s.T = 2;
  union U u = { 1 };
  goto T;
  // S, T and U are still typedef names:
  S ss = 1; T tt = 1; U uu = 1;
}
