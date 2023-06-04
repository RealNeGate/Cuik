// dangling_else_misleading.fail.c
typedef int T;
void f(void) {
  if(1)
    for(int T; ;)
      if(1) {}
      else {
        T x;
      }
}
