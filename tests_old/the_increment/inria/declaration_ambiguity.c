// declaration_ambiguity.c
typedef int T;
void f (void) {
  unsigned int;   // declares zero variables of type "unsigned int"
  const T;        // declares zero variables of type "const T"
  T x;            // T is still visible as a typedef name
  unsigned T;     // declares a variable "T" of type "unsigned"
  T = 1;
}
