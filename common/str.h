#pragma once
#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <cuik_lex.h>

String string_cstr(const char* str);
String string_from_range(const unsigned char* start, const unsigned char* end);
bool string_equals(const String* a, const String* b);
bool string_equals_cstr(const String* a, const char* b);
