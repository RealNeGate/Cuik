#include <stdint.h>
#include <stdio.h>

static uint32_t crc32_tab[256];
static uint32_t crc32_context = 0xFFFFFFFFUL;

void 
crc32_gentab (void)
{
	uint32_t crc;
	const uint32_t poly = 0xEDB88320UL;
	int i, j;
	
	for (i = 0; i < 256; i++) {
		crc = i;
		for (j = 8; j > 0; j--) {
			if (crc & 1) {
				crc = (crc >> 1) ^ poly;
			} else {
				crc >>= 1;
			}
		}
		crc32_tab[i] = crc;
	}
}

int main() {
	crc32_gentab();
	
	for (size_t i = 0; i < 64; i++) {
		size_t j = i * 4;
		printf("%08x %08x %08x %08x\n", crc32_tab[j+0], crc32_tab[j+1], crc32_tab[j+2], crc32_tab[j+3]);
	}

	return 0;
}
