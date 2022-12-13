#include <stddef.h>

void* memcpy(void* restrict s1, const void* restrict s2, size_t n) {
	const unsigned char* restrict c2 = s2;
	unsigned char* restrict c1 = s1;

	while (n--) *c1++ = *c2++;
	return s1;
}

int memcmp(const void *s1, const void *s2, size_t n) {
	const unsigned char* u1 = s1;
	const unsigned char* u2 = s2;

    for (; n--; u1++, u2++) {
		if (*u1 != *u2) return *u1 - *u2;
    }

    return 0;
}

void* memset(void* s, int c, size_t n) {
	unsigned char* restrict buf = s;
	while (n--) *buf++ = c;
	return s;
}
