#include "str.h"

// just implement it here since i don't care
#define STB_DS_IMPLEMENTATION
#include <stb_ds.h>

String string_from_range(const unsigned char* start, const unsigned char* end) {
    return (String){ .length = end-start, .data = start };
}
