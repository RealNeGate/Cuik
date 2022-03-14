void test1() {
  if (sizeof (int){ 1}) {}   // sizeof compound literal
  if (sizeof (int)) {}       // sizeof type

  (void)(int)4;   // cast.
  (void)(int){4}; // compound literal.

  int A = (struct{ int a;}){ 1}.a;
}

int test2(int a, int b) {
  return a ? (void)a,b : a;
}

int test3(int a, int b, int c) {
  return a = b = c;
}

int test4() {
  test4();
  return 0;
}

struct X0 { struct { struct { int c[10][9]; } b; } a; };

void test_sizeof(){
  int arr[10];
  (void)sizeof arr[0];
  (void)sizeof(arr[0]);
  (void)sizeof(arr)[0];
}
