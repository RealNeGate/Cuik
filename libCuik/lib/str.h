#pragma once
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

typedef struct String {
    const unsigned char* data;
    size_t length;
} String;

String string_from_range(const unsigned char* start, const unsigned char* end);
bool string_equals(const String* a, const String* b);
