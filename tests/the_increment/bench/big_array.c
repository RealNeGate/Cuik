#include <stdint.h>

struct Bar {
	uint8_t foo[4096];
};

int da_func() {
	struct Bar b = { };
	return 0;
}
