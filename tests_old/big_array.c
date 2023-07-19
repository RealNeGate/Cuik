#include <stddef.h>

//                   0x1fffffff is smaller than
//                   0x7fffffff  
typedef char BigBoy[0x1fffffff][1];

typedef struct SmallStruct {
	int a, b, c, d;
} SmallStruct;

typedef SmallStruct Mapping[65536][65536];

size_t f()
{
    return sizeof(BigBoy);
}
