#pragma once

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>
#include <stdbool.h>

typedef struct string { const unsigned char* data; size_t length; } string; 

#define KILOBYTES(x) ((x) << 10ull)
#define MEGABYTES(x) ((x) << 20ull)
#define GIGABYTES(x) ((x) << 30ull)

#define panic(...) do { printf(__VA_ARGS__); abort(); } while(0)

#define swap(a, b) do { \
typeof(a) temp = a; \
a = b; \
b = temp; \
} while(0)

void tls_init();
void* tls_push(size_t size);
void* tls_pop(size_t size);
void* tls_save();
void tls_restore(void* p);
void* tls_peek(size_t distance);

inline static bool cstr_equals(const unsigned char* str1, const unsigned char* str2) {
	return strcmp((const char*)str1, (const char*)str2) == 0;
}

// NOTE(NeGate): This isn't correct just a shitty workaround
#ifdef CUIK_NEEDS_SAFE_FUNCTIONS
#define memcpy_s(dst, dstsz, src, count) memcpy(dst, src, count)
#define sprintf_s(str, num, fmt, ...) sprintf(str, fmt, __VA_ARGS__)
#endif
