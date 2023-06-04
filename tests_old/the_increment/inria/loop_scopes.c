// loop_scopes.c
typedef int T, U;
int x;
void f(void) {
  for(int T = 0; sizeof(enum {U}); ) x = U+T;
  for(sizeof(enum {U}); ; ) x = U + sizeof(enum {T});
  while(sizeof(enum {U})) x = U;
  // A declaration in the body of a do ... while loop
  // is not visible in the loop condition.
  do x = sizeof(enum {U}) + U;
  while((U)1 + sizeof(enum {U}));
  // The above declarations of T and U took place in inner scopes
  // and are no longer visible.
  T u3; U u4;
}
