#include "cuik.h"
#include "settings.h"
#include "targets/targets.h"
#include "timer.h"

#ifdef _WIN32
#include "back/microsoft_craziness.h"
#define SLASH "\\"
#else
#define SLASH "/"
#endif

struct Cuik_SystemLibs {
#ifdef _WIN32
    MicrosoftCraziness_Find_Result vswhere;
#endif

    char include_dir[FILENAME_MAX];
};

Warnings warnings = {
    //.data_loss = true,
    .unused_funcs = true,
};

TargetDescriptor target_desc;

static void set_defines(Cuik_CPP* cpp, Cuik_SystemLibs* libs) {
    // find target descriptor
    target_desc = get_x64_target_descriptor();

#ifdef _WIN32
    if (libs != NULL && libs->vswhere.windows_sdk_include == NULL) {
        printf("warning: could not automatically find WinSDK include path\n");
    }

    if (libs != NULL && libs->vswhere.vs_include_path == NULL) {
        printf("warning: could not automatically find VS include path\n");
    }
#endif

    // DO NOT REMOVE THESE, IF THEY'RE MISSING THE PREPROCESSOR WILL NOT DETECT THEM
    cuikpp_define_empty(cpp, "__FILE__");
    cuikpp_define_empty(cpp, "L__FILE__");
    cuikpp_define_empty(cpp, "__LINE__");

    // CuikC specific
    cuikpp_define(cpp, "__CUIKC__", STR(CUIK_COMPILER_MAJOR));
    cuikpp_define(cpp, "__CUIKC_MINOR__", STR(CUIK_COMPILER_MINOR));

    // C23/Cuik bool being available without stdbool.h
    cuikpp_define_empty(cpp, "__bool_true_false_are_defined");
    cuikpp_define(cpp, "bool", "_Bool");
    cuikpp_define(cpp, "false", "0");
    cuikpp_define(cpp, "true", "1");

    // GNU C
    cuikpp_define(cpp, "__BYTE_ORDER__", "1");
    cuikpp_define(cpp, "__ORDER_LITTLE_ENDIAN", "1");
    cuikpp_define(cpp, "__ORDER_BIG_ENDIAN", "2");

    // Standard C macros
    cuikpp_define(cpp, "__STDC__", "1");
    cuikpp_define(cpp, "__STDC_VERSION__", "201112L"); // C11

    // currently there's no freestanding mode but if there was this would be
    // turned off for it
    bool freestanding = false;

    cuikpp_define(cpp, "__STDC_HOSTED__", freestanding ? "0" : "1");
    cuikpp_define(cpp, "__STDC_NO_COMPLEX__", "1");
    cuikpp_define(cpp, "__STDC_NO_VLA__", "1");
    cuikpp_define(cpp, "__STDC_NO_THREADS__", "1");

    {
        // The time of translation of the preprocessing translation unit
        static const char mon_name[][4] = {
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

        time_t rawtime;
        time(&rawtime);

        struct tm* timeinfo = localtime(&rawtime);

        // Mmm dd yyyy
        char date_str[20];
        snprintf(date_str, 20, "\"%.3s%3d %d\"", mon_name[timeinfo->tm_mon], timeinfo->tm_mday, 1900 + timeinfo->tm_year);
        cuikpp_define(cpp, "__DATE__", date_str);

        // hh:mm:ss
        char time_str[20];
        snprintf(time_str, 20, "\"%.2d:%.2d:%.2d\"", timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
        cuikpp_define(cpp, "__TIME__", time_str);
    }

    cuikpp_define(cpp, "static_assert", "_Static_assert");
    cuikpp_define(cpp, "typeof", "_Typeof");

    cuikpp_add_include_directory(cpp, libs->include_dir);

    // platform specific stuff
    if (target_system == TB_SYSTEM_WINDOWS) {
#ifdef _WIN32
        // WinSDK includes
        char filepath[FILENAME_MAX];

        if (!settings.freestanding && libs != NULL) {
            if (snprintf(filepath, FILENAME_MAX, "%S\\um\\", libs->vswhere.windows_sdk_include) > FILENAME_MAX) {
                printf("internal compiler error: WinSDK include directory too long!\n");
                abort();
            }
            cuikpp_add_include_directory(cpp, filepath);

            if (snprintf(filepath, FILENAME_MAX, "%S\\shared\\", libs->vswhere.windows_sdk_include) > FILENAME_MAX) {
                printf("internal compiler error: WinSDK include directory too long!\n");
                abort();
            }
            cuikpp_add_include_directory(cpp, filepath);

            // VS include
            if (snprintf(filepath, FILENAME_MAX, "%S\\", libs->vswhere.vs_include_path) > FILENAME_MAX) {
                printf("internal compiler error: VS include directory too long!\n");
                abort();
            }
            cuikpp_add_include_directory(cpp, filepath);
        }

        if (!settings.nostdlib) {
            if (snprintf(filepath, FILENAME_MAX, "%S\\ucrt\\", libs->vswhere.windows_sdk_include) > FILENAME_MAX) {
                printf("internal compiler error: WinSDK include directory too long!\n");
                abort();
            }
            cuikpp_add_include_directory(cpp, filepath);
        }
#endif

        cuikpp_define_empty(cpp, "_MT");
        if (!settings.static_crt) {
            cuikpp_define_empty(cpp, "_DLL");
        }

        //cuikpp_define_empty(cpp, "_NO_CRT_STDIO_INLINE");
        //cuikpp_define_empty(cpp, "_CRT_NONSTDC_NO_WARNINGS");
        //cuikpp_define_empty(cpp, "_CRT_SECURE_NO_WARNINGS");

        // we support MSVC extensions
        cuikpp_define(cpp, "_MSC_EXTENSIONS", "1");
        cuikpp_define(cpp, "_INTEGRAL_MAX_BITS", "64");

        cuikpp_define(cpp, "_USE_ATTRIBUTES_FOR_SAL", "0");

        // pretend to be MSVC
        if (true) {
            cuikpp_define(cpp, "_MSC_BUILD", "1");
            cuikpp_define(cpp, "_MSC_FULL_VER", "192930137");
            cuikpp_define(cpp, "_MSC_VER", "1929");
        }

        // wrappers over MSVC based keywords and features
        cuikpp_define(cpp, "__int8", "char");
        cuikpp_define(cpp, "__int16", "short");
        cuikpp_define(cpp, "__int32", "int");
        cuikpp_define(cpp, "__int64", "long long");
        cuikpp_define(cpp, "__pragma(x)", "_Pragma(#x)");
        cuikpp_define(cpp, "__inline", "inline");
        cuikpp_define(cpp, "__forceinline", "inline");
        cuikpp_define(cpp, "__signed__", "signed");
        cuikpp_define(cpp, "__alignof", "_Alignof");
        cuikpp_define(cpp, "__CRTDECL", "__cdecl");

        // things we don't handle yet so we just remove them
        cuikpp_define_empty(cpp, "_Frees_ptr_");
        cuikpp_define_empty(cpp, "__unaligned");
        cuikpp_define_empty(cpp, "__analysis_noreturn");
        cuikpp_define_empty(cpp, "__ptr32");
        cuikpp_define_empty(cpp, "__ptr64");
    } else {
        // TODO(NeGate): Automatically detect these somehow...
#ifdef __linux__
        cuikpp_add_include_directory(cpp, "/usr/lib/gcc/x86_64-linux-gnu/10/include/");
        cuikpp_add_include_directory(cpp, "/usr/include/x86_64-linux-gnu/");
        cuikpp_add_include_directory(cpp, "/usr/local/include/");
        cuikpp_add_include_directory(cpp, "/usr/include/");
#endif
    }

    if (target_desc.set_defines != NULL) {
        target_desc.set_defines(cpp);
    }
}

CUIK_API TokenStream cuik_preprocess_simple(Cuik_CPP* restrict out_cpp, const char* filepath, Cuik_SystemLibs* libs, size_t include_count, const char* includes[]) {
    cuikpp_init(out_cpp);
    set_defines(out_cpp, libs);

    // add extra include paths
    for (size_t i = 0; i < include_count; i++) {
        cuikpp_add_include_directory(out_cpp, includes[i]);
    }

    TokenStream tokens = cuikpp_run(out_cpp, filepath);
    cuikpp_finalize(out_cpp);

    return tokens;
}

CUIK_API Cuik_SystemLibs* cuik_get_system_includes(const char* cuik_crt_directory) {
    Cuik_SystemLibs* libs = malloc(sizeof(Cuik_SystemLibs));

#ifdef _WIN32
    libs->vswhere = MicrosoftCraziness_find_visual_studio_and_windows_sdk();
#endif

    sprintf_s(libs->include_dir, FILENAME_MAX, "%s"SLASH"crt"SLASH"include"SLASH, cuik_crt_directory);
    return libs;
}

CUIK_API void cuik_visit_top_level(TranslationUnit* restrict tu, void* user_data, Cuik_TopLevelVisitor* visitor) {
    for (size_t i = 0, count = arrlen(tu->top_level_stmts); i < count; i++) {
        visitor(tu, tu->top_level_stmts[i], user_data);
    }
}
