// enum.c
typedef enum { a, b = a } foo;
// Each enumeration constant has scope that begins just after the
// appearance of its defining enumerator in an enumerator list.
