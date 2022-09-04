#include "str.h"

#define STB_DS_IMPLEMENTATION
#include <stb_ds.h>

String string_from_range(const unsigned char* start, const unsigned char* end) {
    return (String){ .length = end-start, .data = start };
}
