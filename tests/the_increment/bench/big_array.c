#include <stdint.h>

struct Bar {
	uint8_t foo[4096];
};

int da_func() {
	struct Bar b = { 0 };
	return 0;
}
