#include <stdio.h>

int
main()
{
	int arr[2];
	int *p;

	p = &arr[1];
	*p = 0;
	printf("%d\n", arr[1]);
}
