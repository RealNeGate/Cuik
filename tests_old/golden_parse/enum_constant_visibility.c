// enum_constant_visibility.c
typedef int T;
void f(void) {
  int x;
  x = (enum {T, U = T+1})1 + T;
  int y = U - T;
}
