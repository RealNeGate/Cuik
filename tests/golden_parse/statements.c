void test1() {
  { ; {  ;;}} ;;
}

void test2() {
  if (0) { if (1) {} } else { }
  do { } while (0);
  while (0) while(0) do ; while(0);
  for ((void)0;0;(void)0)
    for (;;)
      for ((void)9;0;(void)2)
        ;
  for (int X = 0; 0; (void)0);
}

void test3() {
    switch (0) {
    case 4:
      if (0) {
    case 6: ;
      }
    default:
      ;
  }
}

void test4() {
  if (0);
  int X;
foo:  if (0);
}

typedef int t;
void test5() {
  if (0);
  t x = 0;
  if (0);
}
