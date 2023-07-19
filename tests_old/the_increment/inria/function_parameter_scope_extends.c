// function_parameter_scope_extends.c
typedef long T, U;
enum {V} (*f(T T, enum {U} y, int x[T+U]))(T t) {
  // The last T on the previous line denotes a type!
  // Here, V, T, U, y, x denote variables:
  long l = T+U+V+x[0]+y;
  return 0;
}
