// dangling_else_lookahead.c
typedef int T;
void f(void) {
  for(int T; ;)
    if(1);
  // T must be resolved outside of the scope of the
  // "for" statement, hence denotes a typedef name:
  T x;
  x = 0;
}
