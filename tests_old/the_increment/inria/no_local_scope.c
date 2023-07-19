// no_local_scope.c
typedef int T, U, V;
int x;
int f(void) {
  x = sizeof(enum {T});
  label: x = sizeof(enum {U});
  return sizeof(enum {V});
  // T, U and V now denote enumeration constants:
  x = T + U + V;
}
