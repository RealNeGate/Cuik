// bitfield_declaration_ambiguity.c
typedef signed int T;
struct S {
  unsigned T:3; // bit-field named T with type unsigned
  const T:3;    // anonymous bit-field with type const T
};
