#include <stdio.h>

_Thread_local int counter = 1;
int normal = 1;

int main(void)
{
    for(int n = 0; n < 100; ++n)
		counter++;
	
    printf("counter is %u\n", counter);
	return 0;
}
