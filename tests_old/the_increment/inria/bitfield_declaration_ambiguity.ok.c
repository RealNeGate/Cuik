typedef signed int T;
struct S {
  unsigned T:3;
};


int f(struct S s) {
  return s.T;
}
