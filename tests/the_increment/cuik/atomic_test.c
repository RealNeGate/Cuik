#include <stdatomic.h>

atomic_int counter;
_Atomic double lmao;

int foo() {
	return counter++;
}

int bar() {
	return counter--;
}

float baz(_Atomic float* a) {
    return (*a)++;
}

void this_is_legal() {
    lmao += 1.5f;
}

// a = b can just return b
int this_is_slightly_weird(_Atomic int* a, int b) {
    return *a = b;
}

// a += b has to return the final value
int this_is_slightly_odd(_Atomic int* a, int b) {
    return *a += b;
}
