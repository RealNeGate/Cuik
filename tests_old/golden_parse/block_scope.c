// block_scope.c
typedef int T;
int x;
void f(void) {
  { T T;
    T = 1;
    typedef int x;
  }
  x = 1; // x as a type is no longer visible
  T u;   // T as a variable is no longer visible
}
