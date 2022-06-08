#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#ifdef __CUIKC__
typedef int(*CompareFunction)(const void*, const void*);
typedef const char*(*Lmaoify)(const char*);
typedef int BinaryOp(int a, int b);

int main(void) {
    int ints[] = { -2, 99, 0, -743, 2, INT_MIN, 4 };
    int size = sizeof ints / sizeof *ints;
	
	// fresh to death
	Lmaoify lmao = @(const char*(const char* input)){
		return input ? "lmao" : "not_lmao";
	};
	printf("%s ", lmao("lol"));
	printf("%s ", lmao(NULL));
	
	// should we be able to inherit names from the original typedef?
	BinaryOp* adder = @{ return a + b; };
	printf("%d ", adder(5738, 1593)); // 7331
	
	// this is the yassification of man...
	printf("%d ", @(BinaryOp){ return a - b; }(5738, 1593)); // 4145
	
	// we might wanna use function typenames instead of function pointer
	// typenames... this is going to require some odd stuff in the parsing
	BinaryOp* mult = @(int(int a, int b)){
		return a * b;
	};
	printf("%d ", mult(12, 12)); // 144
	
	int(*divide)(int a, int b) = @{
		return a / b;
	};
	printf("%d ", divide(144, 12)); // 12
	
	qsort(ints, size, sizeof(int), @(int(const void* a, const void* b)){
			  int arg1 = *(const int*)a;
			  int arg2 = *(const int*)b;
			  
			  if (arg1 < arg2) return -1;
			  if (arg1 > arg2) return 1;
			  return 0;
		  });
	
    for (int i = 0; i < size; i++) {
        printf("%d ", ints[i]);
    }
	
    printf("\n");
}
#else
int compare_ints(const void* a, const void* b) {
    int arg1 = *(const int*)a;
    int arg2 = *(const int*)b;
	
    if (arg1 < arg2) return -1;
    if (arg1 > arg2) return 1;
    return 0;
}

int main(void) {
    int ints[] = { -2, 99, 0, -743, 2, INT_MIN, 4 };
    int size = sizeof ints / sizeof *ints;
	
    qsort(ints, size, sizeof(int), compare_ints);
	
    for (int i = 0; i < size; i++) {
        printf("%d ", ints[i]);
    }
	
	return 0;
}
#endif
