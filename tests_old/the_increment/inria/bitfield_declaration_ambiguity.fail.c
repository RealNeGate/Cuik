typedef signed int T;
struct S {
  const T:3;
};


int f(struct S s) {
  return s.T;
}
