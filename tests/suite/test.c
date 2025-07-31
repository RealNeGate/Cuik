#include <stdint.h>

struct String {
    int64_t count;
    char *data;
};

struct String sv_from_parts(const char *data, int64_t count) {
    struct String sv;
    sv.count = count;
    sv.data = (char *)data;
    return sv;
}

int main() {
    return 0;
}

