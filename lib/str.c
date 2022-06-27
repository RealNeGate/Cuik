#include "str.h"

String string_from_range(const unsigned char* start, const unsigned char* end) {
    return (String){ .length = end-start, .data = start };
}
