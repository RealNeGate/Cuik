#include "str.h"

String string_cstr(const char* str) {
    return (String){ .length = strlen(str), .data = (const unsigned char*) str };
}

String string_from_range(const unsigned char* start, const unsigned char* end) {
    return (String){ .length = end-start, .data = start };
}

bool string_equals(const String* a, const String* b) {
    return a->length == b->length && memcmp(a->data, b->data, a->length) == 0;
}

bool string_equals_cstr(const String* a, const char* b) {
    return a->length == strlen(b) && memcmp(a->data, b, a->length) == 0;
}
