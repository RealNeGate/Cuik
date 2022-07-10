//
// MIT License
//
// Copyright (c) 2022 Yasser Arguelles Snape
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// This is my "build script library"-inator for C
// It's inspired by nobuild but different
//
// The way that it works is that you can invoke the compiler or other tools via C code
// and thus just implement that using all the provided utils.
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NONSTDC_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <sys/stat.h>

#ifdef _WIN32
#define WIN32_MEAN_AND_LEAN
#include <windows.h>

#define strdup _strdup
#define popen _popen
#define pclose _pclose

#define strtok_r(a, b, c) strtok_s(a, b, c)
#define SLASH "\\"
#define PATH_MAX MAX_PATH

#define ON_WINDOWS 1
#define ON_POSIX   0
#else
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>

#ifdef __APPLE__
#define PATH_MAX 1024
#endif

#define SLASH "/"

#define ON_POSIX   1
#define ON_WINDOWS 0
#endif

#ifndef USE_DA_ASAN
#define USE_DA_ASAN 0
#endif

#if defined(_MSC_VER) && !defined(__clang__)
#define UNIX_STYLE 0
#else
#define UNIX_STYLE 1
#endif

#if defined(__clang__)
#define ON_CLANG 1
#define ON_GCC   0
#define ON_MSVC  0
#define ON_CUIK  0
#elif defined(__GNUC__)
#define ON_CLANG 0
#define ON_GCC   1
#define ON_MSVC  0
#define ON_CUIK  0
#elif defined(_MSC_VER)
#define ON_CLANG 0
#define ON_GCC   0
#define ON_MSVC  1
#define ON_CUIK  0
#elif defined(__CUIKC__)
#define ON_CLANG 0
#define ON_GCC   0
#define ON_MSVC  0
#define ON_CUIK  1
#endif

#define MAX_COMPILE_WORKERS 6

#ifndef COUNTOF
#define COUNTOF(...) (sizeof(__VA_ARGS__) / sizeof((__VA_ARGS__)[0]))
#endif

typedef enum {
    BUILD_MODE_EXECUTABLE,
    BUILD_MODE_STATIC_LIB,
} BuildMode;

static char command_buffer[4096];
static size_t command_length = 0;

static bool str_ends_with(const char* cstr, const char* postfix) {
    const size_t cstr_len = strlen(cstr);
    const size_t postfix_len = strlen(postfix);

    return postfix_len <= cstr_len && strcmp(cstr + cstr_len - postfix_len, postfix) == 0;
}

static char* str_gimme_good_slashes(const char* str) {
    char bad_slash = ON_WINDOWS ? '/' : '\\';
    char cool_slash = ON_WINDOWS ? '\\' : '/';

    char* dst = strdup(str);
    for (char* i = dst; *i; i++) {
        if (*i == bad_slash) *i = cool_slash;
    }

    return dst;
}

static const char* str_filename(const char* path) {
    const char* slash = path;
    for (; *path; path++) {
        if (*path == '/') slash = path+1;
        else if (*path == '\\') slash = path+1;
    }

    return slash;
}

static char* str_no_ext(const char* path) {
    size_t n = strlen(path);
    while (n > 0 && path[n - 1] != '.') n--;

    if (n > 0) {
        char* result = malloc(n);
        memcpy(result, path, n);
        result[n - 1] = '\0';

        return result;
    } else {
        return strdup(path);
    }
}

#ifdef _WIN32
typedef struct FileIter {
    char* path;

    // private
    HANDLE find_handle;
    WIN32_FIND_DATA find_data;
} FileIter;

static void create_dir_if_not_exists(const char* path) {
    if (CreateDirectory(path, NULL)) {
        if (GetLastError() == ERROR_PATH_NOT_FOUND) {
            printf("Could not create directory '%s'\n", path);
            abort();
        }
    }
}

static FileIter file_iter_open(const char* directory) {
    char buffer[PATH_MAX];
    snprintf(buffer, PATH_MAX, "%s\\*", directory);

    FileIter iter = { 0 };
    iter.find_handle = FindFirstFile(buffer, &iter.find_data);
    if (iter.find_handle == INVALID_HANDLE_VALUE) {
        printf("File iterator failed to open!!!\n");
        abort();
    }

    return iter;
}

static bool file_iter_next(FileIter* iter) {
    if (!FindNextFile(iter->find_handle, &iter->find_data)) {
        if (GetLastError() != ERROR_NO_MORE_FILES) {
            printf("File iterator failed to iterate!!!\n");
            abort();
        }

        if (!FindClose(iter->find_handle)) {
            printf("File iterator failed to close!!!\n");
            abort();
        }

        iter->path = NULL;
        return false;
    }

    iter->path = iter->find_data.cFileName;
    return true;
}
#else
typedef struct {
    char* path;

    // private
    DIR* dir;
} FileIter;

static void create_dir_if_not_exists(const char* path) {
    mkdir(path, 0777);
}

static FileIter file_iter_open(const char* directory) {
    return (FileIter){ NULL, opendir(directory) };
}

static bool file_iter_next(FileIter* iter) {
    struct dirent* dp = readdir(iter->dir);
    if (dp == NULL) {
        closedir(iter->dir);
        iter->path = NULL;
        return false;
    }

    iter->path = dp->d_name;
    return true;
}
#endif

#define ITERATE_FILES(_name, _path) \
for (FileIter _name = file_iter_open(_path); file_iter_next(&_name);)

static void nbuild_init() {
    // don't wanna buffer stdout
    setvbuf(stdout, NULL, _IONBF, 0);

    if (ON_WINDOWS) {
        // sets environment vars for compiler, if not already set
        // NOTE(bumbread): pretty sure this environment variable is a good
        // indicator of vcvars present.
        char *env = getenv("VSINSTALLDIR");
        if(env == NULL) {
            int vcvars_ret = system("call vcvars64");
            if(vcvars_ret != 0) {
                fprintf(stderr, "ERROR: vcvars64.bat wasn't found. "
                    "Please add it shell PATH or run the build script in "
                    "Visual Studio Developer's Command Prompt\n");
                exit(69420);
            }
        }
    }

    #if defined(__clang__)
    printf("Compiling with Clang %d.%d.%d...\n", __clang_major__, __clang_minor__, __clang_patchlevel__);
    #elif defined(__GNUC__)
    printf("Compiling with GCC %d.%d...\n", __GNUC__, __GNUC_MINOR__);
    #elif defined(_MSC_VER)
    printf("Compiling with MSVC %d.%d...\n", _MSC_VER / 100, _MSC_VER % 100);
    #elif defined(__CUIKC__)
    printf("Compiling with Cuik %d.%d...\n", __CUIKC__, __CUIKC_MINOR__);
    #endif
}

////////////////////////////////
// Process pool
////////////////////////////////
#ifndef PROCESS_POOL_SIZE
#define PROCESS_POOL_SIZE 6
#endif

static FILE* process_pool[PROCESS_POOL_SIZE];
static int some_slot = 0;

static void cmd_append(const char* str) {
    size_t l = strlen(str);
    assert(command_length + l + 1 < sizeof(command_buffer));

    memcpy(&command_buffer[command_length], str, l + 1);
    command_length += l;
}

// Print out whatever was on that file stream
static int cmd_dump(FILE** p) {
    FILE* f = *p;
    *p = NULL;

    char buffer[4096];
    while (fread(buffer, sizeof(buffer), 1, f)) {
        printf("%s", buffer);
    }

    return pclose(f);
}

static FILE** cmd_run() {
    //printf("CMD: %s\n", command_buffer);
    //cmd_append(" 2>&1");

    // Find available slots
    int slot = -1;
    for (size_t i = 0; i < PROCESS_POOL_SIZE; i++) {
        if (process_pool[i] == NULL) {
            //printf("Used empty slot! %d\n", i);
            slot = i;
            break;
        }
    }

    if (slot < 0) {
        // if they're used up... wait
        int end = some_slot + 1 % PROCESS_POOL_SIZE;
        for (size_t i = some_slot; i != end; i = (i + 1) % PROCESS_POOL_SIZE) {
            if (process_pool[i] != NULL) {
                // wait for it to finish
                //printf("Wait for an empty slot! %d\n", i);
                int exit_code = cmd_dump(&process_pool[i]);
                if (exit_code != 0) {
                    fprintf(stderr, "process exited with code %d\n", exit_code);
                    exit(exit_code);
                }

                // it completed so we can steal that slot
                process_pool[i] = NULL;
                slot = i;
                break;
            }
        }

        some_slot = (some_slot + 1) % PROCESS_POOL_SIZE;
    }

    assert(slot >= 0);
    process_pool[slot] = popen(command_buffer, "r");

    command_buffer[0] = 0;
    command_length = 0;
    return &process_pool[slot];
}

void cmd_wait_for_all() {
    for (size_t i = 0; i < PROCESS_POOL_SIZE; i++) {
        if (process_pool[i] != NULL) {
            // wait for it to finish
            int exit_code = cmd_dump(&process_pool[i]);
            if (exit_code != 0) {
                fprintf(stderr, "process exited with code %d\n", exit_code);
                exit(exit_code);
            }
        }
    }
}

////////////////////////////////
// C compiler interface
////////////////////////////////
typedef struct {
    const char* output_dir;
    const char* extra_options;

    enum {
        CC_O0, // no optimizations
        CC_Og, // minimal optimizations, for debugging
        CC_Os, // optimize for size
        CC_Ox  // optimize for speed
    } opt;

    enum {
        CC_VECTOR_DEFAULT,

        // x86 specific
        CC_VECTOR_SSE,
        CC_VECTOR_SSE2,
        CC_VECTOR_SSE3,
        CC_VECTOR_SSE4,
        CC_VECTOR_SSE41,
        CC_VECTOR_SSE42,
        CC_VECTOR_AVX,
        CC_VECTOR_AVX2,
        CC_VECTOR_AVX512,

        // ARM specific
        CC_VECTOR_NEON
    } vector_ext;

    struct {
        enum {
            CC_WARN_NONE,
            CC_WARN_NORMAL,
            CC_WARN_ALL,
            CC_WARN_EVERYTHING
        } mode;

        bool as_errors : 1;

        // some common compiler warnings
        bool trigraphs : 1;
        bool missing_declarations : 1;
        bool unused_functions : 1;
        bool unused_declarations : 1;
    } warnings;

    // address sanitizer
    bool use_asan;
    bool debug_info;
} CC_Options;

// if output_name is NULL it'll use the same name as input_path
static void cc_invoke(const CC_Options* options, const char* input_path, const char* output_name) {
    #if UNIX_STYLE
    const char* cc_command;
    if (ON_CLANG) cc_command = "clang";
    else if (ON_GCC) cc_command = "gcc";
    else cc_command = "cc";

    cmd_append(cc_command);

    static const char* vector_ext_table[] = {
        [CC_VECTOR_DEFAULT] = "",
        [CC_VECTOR_SSE]     = "-msse",
        [CC_VECTOR_SSE2]    = "-msse2",
        [CC_VECTOR_SSE3]    = "-msse3",
        [CC_VECTOR_SSE4]    = "-msse4",
        [CC_VECTOR_SSE41]   = "-msse4.1",
        [CC_VECTOR_SSE42]   = "-msse4.2",
        [CC_VECTOR_AVX]     = "-mavx",
        [CC_VECTOR_AVX2]    = "-mavx2",
        [CC_VECTOR_AVX512]  = "-mavx512",
        [CC_VECTOR_NEON]    = "-mneon",
    };

    cmd_append(" ");
    cmd_append(vector_ext_table[options->vector_ext]);
    cmd_append(" -maes");

    if (options->warnings.as_errors) cmd_append(" -Werror");

    const char* warn_option = "";
    switch (options->warnings.mode) {
        case CC_WARN_NONE:       warn_option = " -Wno-everything"; break;
        case CC_WARN_NORMAL:     break;
        case CC_WARN_ALL:        warn_option = " -Wall"; break;
        case CC_WARN_EVERYTHING: warn_option = " -Weverything"; break;
    }
    cmd_append(warn_option);

    if (!options->warnings.trigraphs) cmd_append(" -Wno-trigraphs");
    if (!options->warnings.missing_declarations) cmd_append(" -Wno-missing-declarations");
    if (!options->warnings.unused_functions) cmd_append(" -Wno-unused-function");
    if (!options->warnings.unused_declarations) cmd_append(" -Wno-unused-declarations");

    switch (options->opt) {
        case CC_O0: cmd_append(" -O0"); break;
        case CC_Og: cmd_append(" -Og"); break;
        case CC_Os: cmd_append(" -Os -DNDEBUG"); break;
        case CC_Ox: cmd_append(" -O2 -DNDEBUG"); break;
    }

    if (ON_CLANG) cmd_append(" -Wno-microsoft-enum-forward-reference -Wno-microsoft-anon-tag -Wno-gnu-designator");
    if (options->use_asan) cmd_append(" -fsanitize=address");

    if (options->extra_options) {
        cmd_append(" ");
        cmd_append(options->extra_options);
    }

    cmd_append(" -I deps -I lib -I include");
    cmd_append(" -c -o ");
    cmd_append(options->output_dir);

    char* output = str_no_ext(output_name ? output_name : str_filename(input_path));
    cmd_append(output);
    free(output);

    if (ON_WINDOWS) {
        cmd_append(".obj -D_CRT_SECURE_NO_WARNINGS");
    } else {
        cmd_append(".o");
    }

    if (options->debug_info) cmd_append(" -g ");
    else cmd_append(" ");

    cmd_append(input_path);
    cmd_run();
    #else
    #error "TODO"
    #endif
}

static void ar_invoke(const char* output_path, size_t count, const char* inputs[]) {
    if (ON_WINDOWS) {
        cmd_append("lib /nologo /out:");
        cmd_append(output_path);
        cmd_append(".lib ");
    } else {
		#ifndef NO_LLVMAR
        if (ON_CLANG) cmd_append("llvm-ar rc ");
        else cmd_append("ar -rcs ");
		#else
		cmd_append("ar -rcs ");
		#endif

        cmd_append(output_path);
        if (ON_WINDOWS) cmd_append(".lib ");
        else cmd_append(".a ");
    }

    for (size_t i = 0; i < count; i++) {
        cmd_append(inputs[i]);
        cmd_append(" ");
    }

    int ar_exit = cmd_dump(cmd_run());
    if (ar_exit != 0) {
        fprintf(stderr, "archiver exited with code %d\n", ar_exit);
        exit(ar_exit);
    }
}

static void ld_invoke(const char* output_path, size_t count, const char* inputs[], size_t external_count, const char* external_inputs[]) {
    if (0 /* ON_WINDOWS */) {
        cmd_append("link /ltcg /defaultlib:libcmt /debug /out:");
        cmd_append(str_gimme_good_slashes(output_path));
        cmd_append(".exe");

        for (size_t i = 0; i < external_count; i++) {
            cmd_append(" ");
            cmd_append(external_inputs[i]);
        }

        for (size_t i = 0; i < count; i++) {
            cmd_append(" ");
            cmd_append(inputs[i]);
        }
    } else if (ON_CLANG) {
        // Link with clang instead so it's easier
		#ifndef NO_LLVMAR
        cmd_append("clang -fuse-ld=lld -flto -O2 -g -o ");
		#else
        cmd_append("clang -O2 -g -o ");
		#endif
        cmd_append(str_gimme_good_slashes(output_path));
        if (ON_WINDOWS) cmd_append(".exe");
        else cmd_append(" -Wl,--export-dynamic");

        for (size_t i = 0; i < external_count; i++) {
            cmd_append(" -l");
            cmd_append(external_inputs[i]);
        }

        for (size_t i = 0; i < count; i++) {
            cmd_append(" ");
            cmd_append(inputs[i]);
        }
    } else if (ON_GCC) {
        // TODO(NeGate): Fix this garbage up...
        cmd_append("gcc -rdynamic -o ");
        cmd_append(str_gimme_good_slashes(output_path));
        cmd_append(" -Wl,--export-dynamic ");

        for (size_t i = 0; i < count; i++) {
            cmd_append(inputs[i]);
            cmd_append(" ");
        }

        for (size_t i = 0; i < external_count; i++) {
            cmd_append("-l");
            cmd_append(external_inputs[i]);
            cmd_append(" ");
        }
    } else {
        assert(0 && "TODO");
    }

    int linker_exit = cmd_dump(cmd_run());
    if (linker_exit != 0) {
        fprintf(stderr, "linker exited with code %d\n", linker_exit);
        exit(linker_exit);
    }
}

static void clean(const char* directory) {
    // delete any intermediates
    char temp[PATH_MAX];
    ITERATE_FILES(it, directory) {
        if (str_ends_with(it.path, ".obj") ||
            str_ends_with(it.path, ".o")) {
            snprintf(temp, PATH_MAX, "%s%s", directory, it.path);
            remove(temp);
        }
    }
}
