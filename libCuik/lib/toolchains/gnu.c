#include <cuik.h>
#include <common.h>

static void add_libraries(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* l) {
}

static void set_preprocessor(void* ctx, const Cuik_DriverArgs* args, Cuik_CPP* cpp) {
    cuikpp_add_include_directory(cpp, true, "/usr/lib/gcc/x86_64-linux-gnu/9/include/");
    cuikpp_add_include_directory(cpp, true, "/usr/include/x86_64-linux-gnu/");
    cuikpp_add_include_directory(cpp, true, "/usr/local/include/");
    cuikpp_add_include_directory(cpp, true, "/usr/include/");

    // things we don't handle yet so we just remove them
    cuikpp_define_empty_cstr(cpp, "__THROWNL");

    #if 1
    cuikpp_define_cstr(cpp, "__uint8_t", "unsigned char");
    cuikpp_define_cstr(cpp, "__uint16_t", "unsigned short");
    cuikpp_define_cstr(cpp, "__uint32_t", "unsigned int");
    cuikpp_define_cstr(cpp, "__uint64_t", "unsigned long long");

    cuikpp_define_cstr(cpp, "__int8_t", "char");
    cuikpp_define_cstr(cpp, "__int16_t", "short");
    cuikpp_define_cstr(cpp, "__int32_t", "int");
    cuikpp_define_cstr(cpp, "__int64_t", "long long");
    #endif

    // pretend to be GCC
    cuikpp_define_cstr(cpp, "__inline", "inline");
    cuikpp_define_cstr(cpp, "__restrict", "restrict");
    cuikpp_define_cstr(cpp, "__gnuc_va_list", "char*");
    cuikpp_define_empty_cstr(cpp, "__extension__");
    cuikpp_define_empty_cstr(cpp, "__asm__()");
    // cuikpp_define_cstr(cpp, "_ISOC11_SOURCE", "1");
    // cuikpp_define_cstr(cpp, "__USE_ISOC11", "1");

    cuikpp_define_cstr(cpp, "__GNUC_MINOR__", "2");
    cuikpp_define_cstr(cpp, "__GNUC_PATCHLEVEL__", "1");
    cuikpp_define_cstr(cpp, "__GNUC_STDC_INLINE__", "1");
    cuikpp_define_cstr(cpp, "__GNUC__", "4");
    cuikpp_define_cstr(cpp, "__GXX_ABI_VERSION", "1002");

    // cuikpp_define_cstr(cpp, "__GNUC__", "9");
    // cuikpp_define_empty_cstr(cpp, "_GNU_SOURCE");
}

static bool invoke_link(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename) {
    return false;
}

Cuik_Toolchain cuik_toolchain_gnu(void) {
    return (Cuik_Toolchain){
        // .ctx = result,
        .set_preprocessor = set_preprocessor,
        .add_libraries = add_libraries,
        .invoke_link = invoke_link
    };
}
