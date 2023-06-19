#include <cuik.h>
#include <common.h>

static void add_libraries(void* ctx, bool nocrt, Cuik_Linker* l) {
}

static void set_preprocessor(void* ctx, bool nocrt, Cuik_CPP* cpp) {
    cuikpp_define_cstr(cpp, "__APPLE__", "1");
    cuikpp_define_cstr(cpp, "__MACH__" , "1");
    cuikpp_define_cstr(cpp, "__weak", "// __attribute__((objc_gc(weak))");
    cuikpp_define_cstr(cpp, "__apple_build_version__", "14000029");
}

static bool invoke_link(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename) {
    return false;
}

static void* init(void) {
    return NULL;
}

Cuik_Toolchain cuik_toolchain_darwin(void) {
    return (Cuik_Toolchain){
        .init = init,
        .set_preprocessor = set_preprocessor,
        .add_libraries = add_libraries,
        .invoke_link = invoke_link
    };
}
