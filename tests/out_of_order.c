// it's top level, T must be a decl
// T isn't defined yet, make an entry on the unresolved typenames
T* da_global;
int I = 4*3 + 2*1;

// enum is a known typename, this is an
// unnamed declaration but it adds A,B,C
// to the enum value namespace
enum { A, B, C };

// we don't parse the body in this phase so just
// skip over it
//
// defined foo in the symbol table tho
void foo() {
	T a = 16, b = 5;
	T (*func)(T a, T b) = bar;
}

T bar(T a, T b);

// backpatch all uses of T with int
typedef int T;
