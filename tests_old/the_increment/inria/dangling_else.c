// dangling_else.c
int f(void) {
  if(0)
    if(1) return 1;
   else return 0;
  return 1;
}
