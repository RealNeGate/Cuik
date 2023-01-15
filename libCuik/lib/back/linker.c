#include <cuik.h>
#include <common.h>
#include "../cstrings_are_weird.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define SLASH "\\"
#define strdup(x) _strdup(x)
#else
#define SLASH "/"
#endif

void cuiklink_deinit(Cuik_Linker* l) {
    dyn_array_destroy(l->inputs);
    dyn_array_destroy(l->libpaths);
}

void cuiklink_apply_toolchain_libs(Cuik_Linker* l, Cuik_CompilerArgs* args) {
    args->toolchain.add_libraries(args->toolchain.ctx, args, l);
}

void cuiklink_add_libpath(Cuik_Linker* l, const char filepath[]) {
    char* out = strdup(filepath);
    for (char* s = out; *s; s++) {
        if (*s == '\\') *s = '/';
    }
    dyn_array_put(l->libpaths, out);
}

void cuiklink_add_libpathf(Cuik_Linker* l, const char* fmt, ...) {
    char* out = malloc(FILENAME_MAX);

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(out, FILENAME_MAX, fmt, ap);
    va_end(ap);

    for (char* s = out; *s; s++) {
        if (*s == '\\') *s = '/';
    }
    dyn_array_put(l->libpaths, out);
}

void cuiklink_add_input_file(Cuik_Linker* l, const char* filepath) {
    dyn_array_put(l->inputs, strdup(filepath));
}

bool cuiklink_invoke(Cuik_Linker* l, Cuik_CompilerArgs* args, const char* filename) {
    return args->toolchain.invoke_link(args->toolchain.ctx, args, l, filename);
}
