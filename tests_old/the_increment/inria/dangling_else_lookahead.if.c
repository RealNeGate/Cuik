// dangling_else_lookahead.c
typedef int T;
void f(void) {
  if(sizeof(enum { T }) == 0);
  T x; // T should be resolved outside of the scope of "if"
  x = 0;
}
