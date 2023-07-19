// parameter_declaration_ambiguity.c
typedef int T;
void f(int(x), int(T), int T);
// First parameter: named x, of type int
// Second parameter: anonymous, of type int(T) (i.e., T -> int)
// Third parameter: named T, of type int
