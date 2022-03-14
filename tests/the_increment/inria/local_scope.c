// local_scope.c
typedef int T;
void f(void) {
  T y = 1; // T is a type
  if(1) {
    int T;
    T = 1; // T is a variable
  }
  T x = 1; // T is a type again
}
