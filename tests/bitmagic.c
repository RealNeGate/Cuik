#include <stdint.h>

uint32_t foo(uint32_t x) {
	return __builtin_clz(x);
}
