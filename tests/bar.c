#include <stdio.h>

int main(int argc, char** argv) {
    static const char program[] = {
        #embed "hello_linux"
    };

    _Static_assert((sizeof(program) / sizeof(*program)) >= 4,
        "There should be at least 4 elements in this array.");

    // verify ELF header (at run-time)
    /*assert(program[0] == 0x7F);
    assert(program[1] == 'E');
    assert(program[2] == 'L');
    assert(program[3] == 'F');*/

    const char* p = program;
    // printf("%s", p);
    return 0;
}
