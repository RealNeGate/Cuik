// This is the build script for TB, real simple imo
// just call it with the C11 compiler of your choice
//
// It's inspired by nobuild but different
#define USE_DA_ASAN 0
#include "compile.h"

static const char* INPUT_FILES[] = {
    "src/main_driver.c",
    "src/tls.c",
    "src/timer.c",
    "src/diagnostic.c",
    "src/crash_handler.c",
    "src/big_array.c",
    "src/arena.c",
    "src/settings.c",
    "src/driver_utils.c",
    "src/compilation_unit.c",
    "src/ext/threadpool.c",

    // C preprocessor
    "src/preproc/lexer.c",
    "src/preproc/nopp.c",
    "src/preproc/cpp.c",

    // C frontend
    "src/front/parser.c",
    "src/front/sema.c",
    "src/front/atoms.c",
    "src/front/const_eval.c",
    "src/front/types.c",
    "src/front/ast_dump.c",

    // Target specific stuff
    "src/targets/x64.c",

    // Optional analysis
    "src/anal/analysis.c",

    // Backend
    "src/back/ir_gen.c",
    "src/back/linker.c",
    "src/linker/tblink.c",

#if defined(_WIN32)
    "src/back/microsoft_craziness.cpp",
    "src/ext/threads_msvc.c",
#else
    "src/ext/threads_posix.c"
#endif
};
enum { INPUT_FILE_COUNT = sizeof(INPUT_FILES) / sizeof(INPUT_FILES[0]) };

int tests_working = 0;
int number_of_tests = 0;

void expect_return_value(const char* path, int expected) {
    number_of_tests++;

    printf("Attempt %-80s", path);

    int code;
    char cmd[1024];

    // Compile
    snprintf(cmd, 1024, "cuik %s.c", path);
    code = system(cmd);
    if (code != 0) {
        printf("Fail to compile! (code: %d)\n", code);
        return;
    }

    // Run
    snprintf(cmd, 1024, "%s.exe", path);
    code = system(cmd);
    if (code != expected) {
        printf("Fail to execute! (code: %d)\n", code);
        return;
    }

    // Success!
    printf("Success!\n");
    tests_working++;
}

void expect_stdout(const char* path, const char* expected) {
    number_of_tests++;

    printf("Attempt %-80s", path);

    int code;
    char cmd[1024];

    // Compile
    snprintf(cmd, 1024, "cuik %s.c", path);
    code = system(cmd);
    if (code != 0) {
        printf("Fail to compile! (code: %d)\n", code);
        return;
    }

    // Run
    snprintf(cmd, 1024, "%s.exe", path);
    FILE* stream = popen(cmd, "r");

    char data[1024];
    size_t length = fread(data, 1, sizeof(data), stream);

    code = pclose(stream);
    if (code) {
        printf("Fail to execute! (code: %d)\n", code);
        return;
    }

    if (length == strlen(expected) && memcmp(data, expected, length) == 0) {
        // Success!
        printf("Success!\n");
        tests_working++;
        return;
    }

    printf("Results don't match!\n");
}


void differential(const char* path) {
    number_of_tests++;

    printf("Attempt %-80s", path);

    int code;
    char cmd[1024];

    // Compile clang
    snprintf(cmd, 1024, "clang %s.c -o %s_clang.exe", path, path);
    code = system(cmd);
    if (code != 0) {
        printf("Fail to compile for Clang! (code: %d)\n", code);
        return;
    }

    // Compile cuik
    snprintf(cmd, 1024, "cuik %s.c", path);
    code = system(cmd);
    if (code != 0) {
        printf("Fail to compile for Cuik! (code: %d)\n", code);
        return;
    }

    // Run
    snprintf(cmd, 1024, "%s.exe", path);
    FILE* stream = popen(cmd, "r");

    snprintf(cmd, 1024, "%s_clang.exe", path);
    FILE* baseline_stream = popen(cmd, "r");

    while (true) {
        char data[1024], data2[1024];
        size_t length = fread(data, 1, sizeof(data), stream);
        size_t length2 = fread(data2, 1, sizeof(data2), baseline_stream);

        // we compare lengths before checking completion in
        // case it mismatches right at the end and one stream
        // is just longer
        if (length != length2) {
            printf("Results don't match!\n");
            return;
        }

        // is it complete?
        if (length == 0) break;

        if (memcmp(data, data2, length) != 0) {
            printf("Results don't match!\nGOT:\n%.*s\nEXPECTED:\n%.*s\n", (int)length, data, (int)length, data2);
            return;
        }
    }

    code = pclose(stream);

    int code2 = pclose(baseline_stream);
    if (code || code2) {
        printf("Fail to execute! (code: %d)\n", code);
        return;
    }

    printf("Success!\n");
    tests_working++;
}

void try_compile(const char* path) {
    number_of_tests++;

    printf("Attempt %-80s", path);

    int code;
    char cmd[1024];

    // Compile
    snprintf(cmd, 1024, "cuik -c %s.c", path);
    code = system(cmd);
    if (code != 0) {
        printf("Fail to compile! (code: %d)\n", code);
        return;
    }

    // Success!
    printf("Success!\n");
    tests_working++;
}

// delete .obj, .pdb and .exe
void delete_crap_in_dir(const char* dir_path) {
    char temp[PATH_MAX];

    ITERATE_FILES(it, dir_path) {
        if (str_ends_with(it.path, ".obj") ||
            str_ends_with(it.path, ".exe") ||
            str_ends_with(it.path, ".pdb") ||
            str_ends_with(it.path, ".o")) {
            snprintf(temp, PATH_MAX, "%s%s", dir_path, it.path);
            remove(temp);
        }
    }
}

int main(int argc, char** argv) {
    nbuild_init();
    create_dir_if_not_exists("bin"SLASH);

#ifdef RELEASE_BUILD
    printf("Compiling a release build!\n");
#endif

    CC_Options options = {
        .output_dir = "bin"SLASH,

#       ifdef RELEASE_BUILD
        .opt = CC_Ox,
#       else
        .opt = CC_O0,
#       endif

        // there's some SSE42 stuff in the lexer
        .vector_ext = CC_VECTOR_SSE42,

        // warning stuff
        .warnings = {
            .mode = CC_WARN_ALL,
            .as_errors = true,
            .missing_declarations = true,
            .unused_declarations = true,
        },

        .debug_info = true
    };

    for (int i = 0; i < INPUT_FILE_COUNT; i++) {
        cc_invoke(&options, INPUT_FILES[i], NULL);
    }

    cmd_wait_for_all();

    static const char* LINKER_INPUTS[] = {
        "bin"SLASH"*.obj", "tb"SLASH"tildebackend.lib",
    };

    static const char* EXTERNALS[] = {
#       if ON_WINDOWS
        "ole32.lib", "Advapi32.lib", "OleAut32.lib", "DbgHelp.lib"
#       else
        "c", "m", "pthread"
#       endif
    };

    printf("Linking...\n");
    ld_invoke("bin"SLASH"cuik",
              COUNTOF(LINKER_INPUTS), LINKER_INPUTS,
              COUNTOF(EXTERNALS), EXTERNALS);

    clean("bin"SLASH);

    if (argc > 1) {
        if (strcmp(argv[1], "test") == 0) {
            printf("\n\n\n");
            printf("Running tests...\n");

            differential("tests"SLASH"the_increment"SLASH"cuik"SLASH"promotions");

            expect_return_value("tests"SLASH"the_increment"SLASH"iso"SLASH"program_termination", 42);
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"printf_test", "Hello Hel Goodb 127 63 0 254 63 0 32000 32767 4 17 65532 65530 4 16 32000 32767 4 17 65532 65530 4 16 4294967295 6731943 2147483646 16 123456789 57486731943 985429 9123456 1.000000 123000.000000 0.100 0.234 3.000000");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"crc32_test", "691daa2f");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"ternary_test", "128 16 32 64 128 128");
            //expect_stdout("tests"SLASH"the_increment"SLASH"cuik"SLASH"function_literal", "lmao not_lmao 7331 4145 144 12 -2147483648 -743 -2 0 2 4 99\n");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"fibonacci_test", "1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"clang_17781", "1\n");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"regression_1", "0 1 1 1 1 1 1 ");
            expect_stdout("tests"SLASH"the_increment"SLASH"cuik"SLASH"meme", "7 0 1 2 3 4 5 6 ");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"initializers", "Table (4 entries):\n[0] = { 1, 2, 3, 4 }\n[1] = { 0, 0, 0, 0 }\n[2] = { 0, 0, 0, 0 }\n[3] = { 5, 6, 7, 8 }\n");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"initializers_2", "14 1\n");
            expect_stdout("tests"SLASH"the_increment"SLASH"cuik"SLASH"cuik_00001", "1");
            expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"generic", "Hello, World!\n1 2 2 3 4 OK\n");

            // Inria tests
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"argument_scope");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"atomic");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"c11-noreturn");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"c1x-alignas");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"char-literal-printing");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"c-namespace");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"control-scope");
            //try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"declarators");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"designator");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"expressions");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"long-long-struct");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"function-decls");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"statements");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"struct-recursion");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"types");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"local_typedef");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"declaration_ambiguity");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"declarator_visibility");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"enum_shadows_typedef");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"enum_constant_visibility");
            //try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"namespaces");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"local_scope");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"block_scope");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"if_scopes");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"loop_scopes");
            //try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"no_local_scope");
            //try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"function_parameter_scope");
            //try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"function_parameter_scope_extends");
            try_compile("tests"SLASH"the_increment"SLASH"inria"SLASH"dangling_else");

            printf("===============   Tests (%d succeeded out of %d)   ===============\n", tests_working, number_of_tests);

            delete_crap_in_dir("tests"SLASH"the_increment"SLASH"iso"SLASH);
            delete_crap_in_dir("tests"SLASH"the_increment"SLASH"cuik"SLASH);
            delete_crap_in_dir("tests"SLASH"the_increment"SLASH"inria"SLASH);
        } else if (strcmp(argv[1], "self") == 0) {
            printf("\n\n\n");
            printf("Running phase 1 self-host tests...\n");

            int successes = 0;

            char cmd[1024];
            for (size_t i = 0; i < INPUT_FILE_COUNT; i++) {
                if (str_ends_with(INPUT_FILES[i], ".c")) {
                    snprintf(cmd, 1024, "cuik -o bin/%s -I src/ --threads 1 -t %s", INPUT_FILES[i], INPUT_FILES[i]);

                    if (system(cmd) == 0) {
                        printf("Success with %s!\n", INPUT_FILES[i]);
                        successes++;
                    } else {
                        printf("`-Failure with %s!\n\n", INPUT_FILES[i]);
                    }
                } else {
                    printf("not a C file but sure! %s\n", INPUT_FILES[i]);
                    successes++;
                }
            }

            printf("===============   Tests (%d succeeded out of %d)   ===============\n", successes, INPUT_FILE_COUNT);
        } else if (strcmp(argv[1], "self2") == 0) {
            printf("\n\n\n");
            printf("Running phase 2 self-host tests...\n");

            int successes = 0;

            char cmd[1024];
            for (size_t i = 0; i < INPUT_FILE_COUNT; i++) {
                if (str_ends_with(INPUT_FILES[i], ".c")) {
                    snprintf(cmd, 1024, "cuik -o bin/%s -I src/ --threads 1 --ir %s", INPUT_FILES[i], INPUT_FILES[i]);

                    if (system(cmd) == 0) {
                        printf("Success with %s!\n", INPUT_FILES[i]);
                        successes++;
                    } else {
                        printf("`-Failure with %s!\n\n", INPUT_FILES[i]);
                    }
                } else {
                    printf("not a C file but sure! %s\n", INPUT_FILES[i]);
                    successes++;
                }
            }

            printf("===============   Tests (%d succeeded out of %d)   ===============\n", successes, INPUT_FILE_COUNT);
        } else if (strcmp(argv[1], "self3") == 0) {
            printf("\n\n\n");
            printf("Running phase 3 self-host tests...\n");

            int successes = 0;

            char cmd[1024];
            for (size_t i = 0; i < INPUT_FILE_COUNT; i++) {
                if (str_ends_with(INPUT_FILES[i], ".c")) {
                    int r = snprintf(cmd, 1024, "cuik -o bin/ -I src --threads 1 -c %s", INPUT_FILES[i]);
                    assert(r >= 0 && r < 1024);

                    int code = system(cmd);
                    if (code == 0) {
                        printf("Success with %s!\n", INPUT_FILES[i]);
                        successes++;
                    } else {
                        printf("`-Failure with %s! %d\n\n", INPUT_FILES[i], code);
                    }
                } else {
                    printf("not a C file but sure! %s\n", INPUT_FILES[i]);
                    successes++;
                }
            }

            printf("===============   Tests (%d succeeded out of %d)   ===============\n", successes, INPUT_FILE_COUNT);
        }

    }

    return 0;
}
