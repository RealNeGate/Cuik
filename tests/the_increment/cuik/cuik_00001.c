// when loading the long in this example, Cuik will use the implicit
// cast type and instead load a bool which means a byte sized value
// instead of loading then casting to bool.
#include <stdio.h>
#include <stdbool.h>

bool foo(long val) {
	return val && 1;
}

int main() {
	long bar = 0x600;
	printf("%d", foo(bar));
	return 0;
}
