// function_parameter_scope.c
typedef long T, U;
enum {V} (*f(T T, enum {U} y, int x[T+U]))(T t);
  // The above declares a function f of type:
  // (long, enum{U}, ptr(int)) -> ptr (long -> enum{V})
T x[(U)V+1]; // T and U again denote types; V remains visible
