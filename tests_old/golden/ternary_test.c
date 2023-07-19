#include <stdio.h>
#include <stddef.h>

int foo(int a) {
    return (a == 0 ? 16 :
			a == 1 ? 32 :
			a == 2 ? 64 :
			128);
}

int main() {
    printf("%d ", foo(-168));
    printf("%d ", foo(0));
    printf("%d ", foo(1));
    printf("%d ", foo(2));
    printf("%d ", foo(3));
    printf("%d", foo(168));
	
    return 0;
}
