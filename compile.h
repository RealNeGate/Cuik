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
#include "windows.h"

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

#ifndef RELEASE_BUILD
#define RELEASE_BUILD 0
#endif

#ifndef USE_DA_ASAN
#define USE_DA_ASAN 0
#endif

#if defined(_MSC_VER) && !defined(__clang__)
#define UNIX_STYLE 0
#else
#define UNIX_STYLE 1
#endif

#include "subprocess.h"

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
		if (*path == '/') slash = path;
		else if (*path == '\\') slash = path;
	}

	return slash;
}

static const char* str_ext(const char* path) {
	const char* dot = path;
	for (; *path; path++) {
		if (*path == '.') dot = path;
	}

	return dot;
}

static const char* str_no_ext(const char* path) {
	size_t n = strlen(path);
    while (n > 0 && path[n - 1] != '.') n--;

    if (n > 0) {
        char* result = malloc(n);
        memcpy(result, path, n);
        result[n - 1] = '\0';

        return result;
    } else {
        return path;
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

static struct subprocess_s cmd_run() {
    size_t cmd_length = 0;
    const char** cmds = malloc(sizeof(const char*) * 1000);

    // Split up commands
    char* ctx;
    char* i = strtok_r(command_buffer, " ", &ctx);
    while (i != NULL) {
        assert(cmd_length < 999);
        cmds[cmd_length++] = i;

        i = strtok_r(NULL, " ", &ctx);
    }
    cmds[cmd_length++] = NULL;

    // Spin up the process
    struct subprocess_s process;
    subprocess_create(cmds, subprocess_option_inherit_environment | subprocess_option_combined_stdout_stderr, &process);

    command_buffer[0] = 0;
	command_length = 0;

    return process;
}

static void dump_file(FILE* f) {
}

// Print out whatever was on that file stream
static int cmd_dump(struct subprocess_s p) {
    char buffer[4096];
	int length = 0;
    while ((length = subprocess_read_stdout(&p, buffer, sizeof(buffer)))) {
		printf("%.*s", length, buffer);
	}

    int code;
    subprocess_join(&p, &code);
    return code;
}

#define ITERATE_FILES(_name, _path) \
for (FileIter _name = file_iter_open(_path); file_iter_next(&_name);)

static void cmd_append(const char* str) {
	size_t l = strlen(str);
	assert(command_length + l + 1 < sizeof(command_buffer));

	memcpy(&command_buffer[command_length], str, l + 1);
	command_length += l;
}

static void builder_init() {
	// don't wanna buffer stdout
	setvbuf(stdout, NULL, _IONBF, 0);

	if (ON_WINDOWS) {
		// sets environment vars for compiler
		system("call vcvars64");
	}

	create_dir_if_not_exists("build/");

#if defined(__clang__)
	printf("Compiling on Clang %d.%d.%d...\n", __clang_major__, __clang_minor__, __clang_patchlevel__);
#elif defined(__GNUC__)
	printf("Compiling on GCC %d.%d...\n", __GNUC__, __GNUC_MINOR__);
#elif defined(_MSC_VER)
	printf("Compiling on MSVC %d.%d...\n", _MSC_VER / 100, _MSC_VER % 100);
#elif defined(__CUIKC__)
	printf("Compiling on Cuik %d.%d...\n", __CUIKC__, __CUIKC_MINOR__);
#endif

	if (RELEASE_BUILD) {
		printf("And it's a release build!\n");
	}
}

static void builder_compile_cuik(size_t count, const char* filepaths[], const char* output_path, const char* extra_libraries) {
	cmd_append("cuik --include src/ -o ");
	cmd_append(output_path);
	cmd_append(" build ");

	for (size_t i = 0; i < count; i++) {
		cmd_append(filepaths[i]);
		cmd_append(" ");
	}

	printf("CMD: %s\n", command_buffer);
	cmd_dump(cmd_run());
}

static void builder_compile_msvc(BuildMode mode, size_t count, const char* filepaths[], const char* output_path, const char* extra_libraries) {
	cmd_append("cl /MP /arch:AVX /D_CRT_SECURE_NO_WARNINGS /I:src ");

	if (mode == BUILD_MODE_EXECUTABLE) {
		cmd_append("/Fe:");
		cmd_append(output_path);
	} else {
		cmd_append("/Fo:build\\");
	}

	if (RELEASE_BUILD) {
		cmd_append("/Ox /WX /GS- /DNDEBUG ");
	} else {
		cmd_append("/MTd /Od /WX /Zi /D_DEBUG /RTC1 ");
	}

	for (size_t i = 0; i < count; i++) {
		cmd_append(filepaths[i]);
		cmd_append(" ");
	}

	printf("CMD: %s\n", command_buffer);
	cmd_dump(cmd_run());

	if (mode == BUILD_MODE_STATIC_LIB) {
		cmd_append("lib /out:");
		cmd_append(output_path);
		cmd_append(" build\\*.obj");
		cmd_run();
	}
}

static void builder_compile_cc(BuildMode mode, size_t count, const char* filepaths[], const char* output_path, const char* extra_libraries) {
	const char* cc_command = ON_CLANG ? "clang" : "gcc";

	// compile object files
    for (size_t i = 0; i < count; i += MAX_COMPILE_WORKERS) {
        struct subprocess_s streams[MAX_COMPILE_WORKERS];

        size_t end = (i >= ((count / MAX_COMPILE_WORKERS) * MAX_COMPILE_WORKERS) ? count % MAX_COMPILE_WORKERS : MAX_COMPILE_WORKERS);
        for (size_t j = 0; j < end; j++) {
            const char* input = filepaths[i+j];
            const char* name = str_no_ext(str_filename(input));

            cmd_append(cc_command);
            cmd_append(" -march=haswell -maes -Werror -Wall -Wno-trigraphs -Wno-unused-function -Wno-missing-declarations ");

            if (RELEASE_BUILD) {
                cmd_append("-O2 -DNDEBUG ");
            }

            if (ON_CLANG) {
                cmd_append("-Wno-gnu-designator -Wno-microsoft-anon-tag -fno-spell-checking ");
                if (USE_DA_ASAN) cmd_append("-fsanitize=address ");
            } else if (ON_GCC) {
                cmd_append("-fms-extensions ");
            }

            cmd_append("-I src ");
            cmd_append("-c -o build");
            cmd_append(name);

#ifdef NO_DEBUG_INFO
            if (ON_WINDOWS) {
                cmd_append(".obj -D_CRT_SECURE_NO_WARNINGS ");
            } else {
                cmd_append(".o ");
            }
#else
            if (ON_WINDOWS) {
                cmd_append(".obj -D_CRT_SECURE_NO_WARNINGS -g -gcodeview ");
            } else {
                cmd_append(".o -g ");
            }
#endif

            cmd_append(filepaths[i+j]);
            streams[j] = cmd_run();

            printf("Compiling '%s'...\n", filepaths[i+j]);
        }

        bool success = true;
        for (size_t j = 0; j < end; j++) {
            int code = cmd_dump(streams[j]);

            if (code) {
                printf("Failed to compile %s! %d\n", filepaths[i+j], code);
                success = false;
            }
        }

        if (!success) {
            fprintf(stderr, "Compilation errors... fix em\n");
            exit(69420);
        }
    }

    // Do linker work
    if (mode == BUILD_MODE_EXECUTABLE) {
        if (ON_WINDOWS) {
            cmd_append("link /ltcg /defaultlib:libcmt /debug /out:");
            cmd_append(str_gimme_good_slashes(output_path));
            cmd_append(".exe ");
            cmd_append("build\\*.obj ole32.lib Advapi32.lib OleAut32.lib DbgHelp.lib ");
            cmd_append(extra_libraries);
        } else if (ON_CLANG) {
            // Link with clang instead so it's easier
            cmd_append("clang ");
#ifndef NO_DEBUG_INFO
            cmd_append("-g ");
#endif
            cmd_append("-o ");
            cmd_append(output_path);
            if (ON_WINDOWS) cmd_append(".exe");
            cmd_append(" ");

            if (ON_WINDOWS) {
                cmd_append("tildebackend.lib build/*.obj -lole32 -lAdvapi32 -lOleAut32 -lDbgHelp ");
            } else {
                cmd_append("./tildebackend.a build/*.o -lc -lm -lpthread ");
            }

            cmd_append(extra_libraries);
            cmd_append(" ");

            if (USE_DA_ASAN) {
                cmd_append("-fsanitize=address ");
                printf("Using address sanitizer :p\n");
            }
        } else if (ON_GCC) {
            cmd_append("ld -o ");
            cmd_append(output_path);
            if (ON_WINDOWS) cmd_append(".exe");
            cmd_append(" build/*.o ./tildebackend.a -lc -lm -lpthread ");
            cmd_append("/usr/lib/x86_64-linux-gnu/crt1.o ");
            cmd_append("/usr/lib/x86_64-linux-gnu/crti.o ");
            cmd_append("/usr/lib/x86_64-linux-gnu/crtn.o ");
            cmd_append("/usr/lib/x86_64-linux-gnu/libc_nonshared.a ");
            cmd_append("--dynamic-linker /lib64/ld-linux-x86-64.so.2 ");
            cmd_append(extra_libraries);
        } else {
            assert(0 && "TODO");
        }
    } else if (mode == BUILD_MODE_STATIC_LIB) {
        if (ON_MSVC) {
            cmd_append("lib /out:");
            cmd_append(output_path);
            cmd_append(".lib build/*.obj ");
            cmd_append(extra_libraries);
        } else {
            if (ON_CLANG) cmd_append("llvm-ar rc ");
            else cmd_append("ar -rcs ");

            cmd_append(output_path);
            if (ON_WINDOWS) cmd_append(".lib");
            else cmd_append(".a");

            cmd_append(" ");
            cmd_append("build/*.obj ");
            cmd_append(extra_libraries);
        }
    } else {
        printf("unknown build mode\n");
        abort();
    }

    printf("CMD: %s\n", command_buffer);
    cmd_dump(cmd_run());
}

static void builder_compile(BuildMode mode, size_t count, const char* filepaths[], const char* output_path, const char* extra_libraries) {
    if (UNIX_STYLE) {
        builder_compile_cc(mode, count, filepaths, output_path, extra_libraries);
    } else {
        builder_compile_msvc(mode, count, filepaths, output_path, extra_libraries);
    }

    // delete any intermediates
    char temp[PATH_MAX];
    ITERATE_FILES(it, "build/") {
        if (str_ends_with(it.path, ".obj") ||
            str_ends_with(it.path, ".o")) {
            snprintf(temp, PATH_MAX, "build/%s", it.path);
            remove(temp);
        }
    }
}
