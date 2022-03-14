// enum-trick.c
#include <stdio.h>
enum { a = 42 } x = a;
int main(int argc, char *argv[]) {
  enum { a = a + 1 } y = a;
  printf("%d, %d\n", x, y); // prints: 42, 43
}
// Each enumeration constant has scope that begins just after the
// appearance of its defining enumerator in an enumerator list.
