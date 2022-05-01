#pragma once
#define __STDC_WANT_LIB_EXT1__ 1

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>
#include <stdbool.h>
#include <stdarg.h>
#include "cstrings_are_weird.h"

typedef struct { const unsigned char* data; size_t length; } string; 

#define KILOBYTES(x) ((x) << 10ull)
#define MEGABYTES(x) ((x) << 20ull)
#define GIGABYTES(x) ((x) << 30ull)

#define STR2(x) #x
#define STR(x) STR2(x)

#ifndef MAX_PATH
#define MAX_PATH 260
#endif

#define Pair(A, B) struct { A _0; B _1; }

// just because we use a threads fallback layer which can include windows
// and such which is annoying... eventually need to modify that out or something
#ifndef thread_local
#define thread_local _Thread_local
#endif

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

inline static bool cstr_equals(const unsigned char* str1, const unsigned char* str2) {
	return strcmp((const char*)str1, (const char*)str2) == 0;
}

// returns the number of bytes written
inline static size_t cstr_copy(size_t len, char* dst, const char* src) {
	size_t i = 0;
	while (src[i]) {
		assert(i < len);
		
		dst[i] = src[i];
		i += 1;
	}
	return i;
}

#if _WIN32
typedef wchar_t* OS_String;
typedef wchar_t OS_Char;

#define OS_STR(x) L##x
#define OS_STR_FMT "S"
#else
typedef char* OS_String;
typedef char OS_Char;

#define OS_STR(x) x
#define OS_STR_FMT "s"

// non-windows platforms generally just don't have the safe functions so
// let's provide them
inline static int sprintf_s(char* buffer, size_t len, const char* format, ...) {
	if (buffer == NULL || len == 0) return -1;
	
	va_list args;
	va_start(args, format);
	int result = vsnprintf(buffer, len, format, args);
	va_end(args);
	
	if (result < 0 && result >= len) {
		fprintf(stderr, "error: buffer overflow on sprintf_s!\n");
		abort();
	}
	return result;
}

#endif
