#include <cuik.h>

#include "settings.h"
#include "targets/targets.h"
#include "timer.h"

#ifdef _WIN32
#include "back/microsoft_craziness.h"
#define SLASH "\\"
#else
#define SLASH "/"
#endif

Warnings warnings = {
    .unused_funcs = true,
};

// internal globals to Cuik
char cuik__include_dir[FILENAME_MAX];
MicrosoftCraziness_Find_Result cuik__vswhere;

CUIK_API void cuik_init(void) {
    init_report_system();
}

CUIK_API void cuik_find_system_deps(const char* cuik_crt_directory) {
    #ifdef _WIN32
    cuik__vswhere = cuik__find_visual_studio_and_windows_sdk();
    #endif

    sprintf_s(cuik__include_dir, FILENAME_MAX, "%s"SLASH"crt"SLASH"include"SLASH, cuik_crt_directory);
}

CUIK_API bool cuik_lex_is_keyword(size_t length, const char* str) {
    return classify_ident((const unsigned char*)str, length) != TOKEN_IDENTIFIER;
}

static void set_defines(Cuik_CPP* cpp, const Cuik_TargetDesc* target_desc, bool system_libs) {
    #ifdef _WIN32
    if (cuik__vswhere.windows_sdk_include == NULL) {
        printf("warning: could not automatically find WinSDK include path\n");
    }

    if (cuik__vswhere.vs_include_path == NULL) {
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
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
        };

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

    cuikpp_add_include_directory(cpp, cuik__include_dir);

    // platform specific stuff
    if (target_system == TB_SYSTEM_WINDOWS) {
        #ifdef _WIN32
        // WinSDK includes
        char filepath[FILENAME_MAX];

        if (!settings.freestanding) {
            if (snprintf(filepath, FILENAME_MAX, "%S\\um\\", cuik__vswhere.windows_sdk_include) > FILENAME_MAX) {
                printf("internal compiler error: WinSDK include directory too long!\n");
                abort();
            }
            cuikpp_add_include_directory(cpp, filepath);

            if (snprintf(filepath, FILENAME_MAX, "%S\\shared\\", cuik__vswhere.windows_sdk_include) > FILENAME_MAX) {
                printf("internal compiler error: WinSDK include directory too long!\n");
                abort();
            }
            cuikpp_add_include_directory(cpp, filepath);

            // VS include
            if (snprintf(filepath, FILENAME_MAX, "%S\\", cuik__vswhere.vs_include_path) > FILENAME_MAX) {
                printf("internal compiler error: VS include directory too long!\n");
                abort();
            }
            cuikpp_add_include_directory(cpp, filepath);
        }

        if (!settings.nostdlib) {
            if (snprintf(filepath, FILENAME_MAX, "%S\\ucrt\\", cuik__vswhere.windows_sdk_include) > FILENAME_MAX) {
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

    if (target_desc != NULL) {
        target_desc->set_defines(cpp);
    }
}

CUIK_API TokenStream cuik_preprocess_simple(Cuik_CPP* restrict out_cpp, const char* filepath, const Cuik_TargetDesc* target_desc, bool system_libs, size_t include_count, const char* includes[]) {
    cuikpp_init(out_cpp);
    set_defines(out_cpp, target_desc, system_libs);

    // add extra include paths
    for (size_t i = 0; i < include_count; i++) {
        cuikpp_add_include_directory(out_cpp, includes[i]);
    }

    return cuikpp_run(out_cpp, filepath);
}

CUIK_API void cuik_visit_top_level(TranslationUnit* restrict tu, void* user_data, Cuik_TopLevelVisitor* visitor) {
    size_t count = arrlen(tu->top_level_stmts);
    CUIK_TIMED_BLOCK("top level visitor (%zu statements)", count) {
        for (size_t i = 0; i < count; i++) {
            visitor(tu, tu->top_level_stmts[i], user_data);
        }
    }
}

CUIK_API const char* cuik_get_location_file(TokenStream* restrict s, SourceLocIndex loc) {
    SourceLoc* l = &s->locations[SOURCE_LOC_GET_DATA(loc)];
    return l->line->filepath;
}

CUIK_API int cuik_get_location_line(TokenStream* restrict s, SourceLocIndex loc) {
    SourceLoc* l = &s->locations[SOURCE_LOC_GET_DATA(loc)];
    return l->line->line;
}

CUIK_API TokenStream* cuik_get_token_stream_from_tu(TranslationUnit* restrict tu) {
    return &tu->tokens;
}

CUIK_API Token* cuik_get_tokens(TokenStream* restrict s) {
    return &s->tokens[0];
}

CUIK_API size_t cuik_get_token_count(TokenStream* restrict s) {
    return arrlen(s->tokens);
}
