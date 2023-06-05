#include <cuik.h>
#include <common.h>
#include <log.h>

static int dummy;

static void add_libraries(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* l) {
}

static void set_preprocessor(void* ctx, const Cuik_DriverArgs* args, Cuik_CPP* cpp) {
    cuikpp_add_include_directory(cpp, true, "/usr/lib/gcc/x86_64-linux-gnu/9/include/");
    cuikpp_add_include_directory(cpp, true, "/usr/include/x86_64-linux-gnu/");
    cuikpp_add_include_directory(cpp, true, "/usr/local/include/");
    cuikpp_add_include_directory(cpp, true, "/usr/include/");

    // things we don't handle yet so we just remove them
    cuikpp_define_empty_cstr(cpp, "__THROWNL");
    cuikpp_define_empty_cstr(cpp, "__USER_LABEL_PREFIX__");

    cuikpp_define_cstr(cpp, "_LP64", "1");
    cuikpp_define_cstr(cpp, "__C99_MACRO_WITH_VA_ARGS", "1");
    cuikpp_define_cstr(cpp, "__ELF__", "1");
    cuikpp_define_cstr(cpp, "__LP64__", "1");
    cuikpp_define_cstr(cpp, "__SIZEOF_DOUBLE__", "8");
    cuikpp_define_cstr(cpp, "__SIZEOF_FLOAT__", "4");
    cuikpp_define_cstr(cpp, "__SIZEOF_INT__", "4");
    cuikpp_define_cstr(cpp, "__SIZEOF_LONG_DOUBLE__", "8");
    cuikpp_define_cstr(cpp, "__SIZEOF_LONG_LONG__", "8");
    cuikpp_define_cstr(cpp, "__SIZEOF_LONG__", "8");
    cuikpp_define_cstr(cpp, "__SIZEOF_POINTER__", "8");
    cuikpp_define_cstr(cpp, "__SIZEOF_PTRDIFF_T__", "8");
    cuikpp_define_cstr(cpp, "__SIZEOF_SHORT__", "2");
    cuikpp_define_cstr(cpp, "__SIZEOF_SIZE_T__", "8");
    cuikpp_define_cstr(cpp, "__SIZE_TYPE__", "unsigned long");
    cuikpp_define_cstr(cpp, "__STDC_HOSTED__", "1");
    cuikpp_define_cstr(cpp, "__STDC_NO_COMPLEX__", "1");
    cuikpp_define_cstr(cpp, "__STDC_UTF_16__", "1");
    cuikpp_define_cstr(cpp, "__STDC_UTF_32__", "1");
    cuikpp_define_cstr(cpp, "__STDC_VERSION__", "201112L");
    cuikpp_define_cstr(cpp, "__STDC__", "1");
    cuikpp_define_cstr(cpp, "__alignof__", "_Alignof");
    cuikpp_define_cstr(cpp, "__amd64", "1");
    cuikpp_define_cstr(cpp, "__amd64__", "1");
    cuikpp_define_cstr(cpp, "__const__", "const");
    cuikpp_define_cstr(cpp, "__gnu_linux__", "1");
    cuikpp_define_cstr(cpp, "__inline__", "inline");
    cuikpp_define_cstr(cpp, "__linux", "1");
    cuikpp_define_cstr(cpp, "__linux__", "1");
    cuikpp_define_cstr(cpp, "__signed__", "signed");
    cuikpp_define_cstr(cpp, "__typeof__", "typeof");
    cuikpp_define_cstr(cpp, "__unix", "1");
    cuikpp_define_cstr(cpp, "__unix__", "1");
    cuikpp_define_cstr(cpp, "__volatile__", "volatile");
    cuikpp_define_cstr(cpp, "linux", "1");
    cuikpp_define_cstr(cpp, "unix", "1");
}

static bool invoke_link(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename) {
    return false;
}

static void* init(void) {
    return NULL;
}

Cuik_Toolchain cuik_toolchain_gnu(void) {
    return (Cuik_Toolchain){
        .init = init,
        .set_preprocessor = set_preprocessor,
        .add_libraries = add_libraries,
        .invoke_link = invoke_link
    };
}
