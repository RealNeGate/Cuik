// if_scopes.c
typedef int T, U;
int x;
void f(void) {
  if(sizeof(enum {T}))
    // The declaration of T as an enumeration constant is
    // visible in both branches:
    x = sizeof(enum {U}) + T;
  else {
    // Here, the declaration of U as an enumeration constant
    // is no longer visible, but that of T still is.
    U u = (int)T;
  }
  switch(sizeof(enum {U})) x = U;
  // Here, T and U are typedef names again:
  T t; U u;
}
