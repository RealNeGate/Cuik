// enum_shadows_typedef.c
typedef int T;
void f(void) {
  int x = (int)(enum {T})1;
  // T now denotes an enumeration constant,
  // and behaves syntactically like a variable:
  x = (int)T;
}
