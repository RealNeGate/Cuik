#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <stdbool.h>

typedef struct string { const char* data; size_t length; } string; 

#define KILOBYTES(x) ((x) << 10ull)
#define MEGABYTES(x) ((x) << 20ull)
#define GIGABYTES(x) ((x) << 30ull)

#define cast(type, expr) ((type)(expr))
#define panic(...) do { printf(__VA_ARGS__); abort(); } while(0)

void tls_init();
void* tls_push(size_t size);
void* tls_pop(size_t size);
void* tls_save();
void tls_restore(void* p);
void* tls_peek(size_t distance);
