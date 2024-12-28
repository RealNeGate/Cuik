#include <stdio.h>

typedef struct { int x; int y; } s;

s v;

int
main()
{
	v.x = 1;
	v.y = 2;
	printf("%d\n", 3 - v.x - v.y);
}
