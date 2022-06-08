#include <stdalign.h>
#include <stdio.h>

int main()
{
	size_t alignment = alignof(short);
	printf("alignof(short) = %d\n", alignment); // 2
	printf("alignof(int) = %d\n", alignof(int)); // 4
	printf("alignof(long) = %d\n", alignof(long)); // 4
	printf("alignof(float) = %d\n", alignof(float)); // 4
	printf("alignof(double) = %d\n", alignof(double)); // 8

	typedef struct
	{
		int a;
		double b;
	} test;
	
	printf("alignof(test) = %d\n", alignof(test)); // 8 because that is the alignment of the largest element in the structure
	
	/* output
		alignof(short) = 2
		alignof(int) = 4
		alignof(long) = 4
		alignof(float) = 4
		alignof(double) = 8
		alignof(test) = 8
	*/
}