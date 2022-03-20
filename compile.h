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
#define SLASH "\\"
#define PATH_MAX MAX_PATH
#else
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>

#define SLASH "/"
#endif

#if defined(_MSC_VER) && !defined(__clang__)
#define UNIX_STYLE 0
#else
#define UNIX_STYLE 1
#endif

static char command_buffer[4096];
static size_t command_length = 0;

inline static bool str_ends_with(const char* cstr, const char* postfix) {
    const size_t cstr_len = strlen(cstr);
    const size_t postfix_len = strlen(postfix);
	
    return postfix_len <= cstr_len && strcmp(cstr + cstr_len - postfix_len, postfix) == 0;
}

inline static const char* str_no_ext(const char* path) {
	size_t n = strlen(path);
    while (n > 0 && path[n - 1] != '.') {
        n -= 1;
    }
	
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
inline static void create_dir_if_not_exists(const char* path) {
	if (CreateDirectory(path, NULL)) {
		if (GetLastError() == ERROR_PATH_NOT_FOUND) {
			printf("Could not create directory '%s'\n", path);
			abort();
		}
	}
}
#else
inline static void create_dir_if_not_exists(const char* path) {
	mkdir(path, 0666);
}
#endif

#ifdef _WIN32
// https://bobobobo.wordpress.com/2009/02/02/getlasterror-and-getlasterrorasstring/
inline static LPSTR GetLastErrorAsString(void) {
	LPSTR buf = NULL;
	
	int result = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, 
							   0, GetLastError(), 0, (LPSTR)&buf, 0, 0);
	
	return buf;
}

typedef struct FileIter {
    HANDLE find_handle;
    WIN32_FIND_DATA find_data;
} FileIter;

inline static FileIter file_iter_open(const char* directory) {
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

inline static char* file_iter_next(FileIter* iter) {
	if(!FindNextFile(iter->find_handle, &iter->find_data)) {
		if (GetLastError() != ERROR_NO_MORE_FILES) {
			printf("File iterator failed to iterate!!!\n");
			abort();
		}
		
		return NULL;
	}
	
	return iter->find_data.cFileName;
}

inline static void file_iter_close(FileIter* iter) {
    if (!FindClose(iter->find_handle)) {
		printf("File iterator failed to close!!!\n");
		abort();
	}
}
#else
typedef struct FileIter {
    DIR* dir;
} FileIter;

inline static FileIter file_iter_open(const char* directory) {
    return (FileIter){ opendir(directory) };
}

inline static char* file_iter_next(FileIter* iter) {
	struct dirent* dp = readdir(iter->dir);
	return dp ? dp->d_name : NULL;
}

inline static void file_iter_close(FileIter* iter) {
    closedir(iter->dir);
}
#endif

inline static void cmd_append(const char* str) {
	size_t l = strlen(str);
	assert(command_length + l + 1 < sizeof(command_buffer));
	
	memcpy(&command_buffer[command_length], str, l + 1);
	command_length += l;
}

// return the child process stdout
inline static FILE* cmd_run() {
	FILE* file = popen(command_buffer, "r");
	if (file == NULL) {
		printf("command failed to execute! %s (%s)\n", command_buffer, strerror(errno));
		abort();
	}
	
	command_buffer[0] = 0;
	command_length = 0;
	return file;
}

// Print out whatever was on that file stream
inline static int cmd_dump(FILE* stream) {
	char buffer[4096];
	while (fread(buffer, sizeof(buffer), sizeof(char), stream)) {
		printf("%s", buffer);
	}
	return pclose(stream);
}

inline static void builder_init() {
	// don't wanna buffer stdout
	setvbuf(stdout, NULL, _IONBF, 0);
	
#if defined(_WIN32)
	// sets environment vars for compiler
	system("call vcvars64");
#endif
	
	create_dir_if_not_exists("build/");
	
#if defined(__clang__)
	printf("Compiling on Clang %d.%d.%d...\n", __clang_major__, __clang_minor__, __clang_patchlevel__);
#elif defined(__GNUC__)
	printf("Compiling on GCC %d.%d...\n", __GNUC__, __GNUC_MINOR__);
#elif defined(_MSC_VER)
	printf("Compiling on MSVC %d.%d...\n", _MSC_VER / 100, _MSC_VER % 100);
#endif
}

inline static void builder_compile_msvc(size_t count, const char* filepaths[], const char* output_path) {
	cmd_append("cl /Fo:build\\ /MP /arch:AVX /D_CRT_SECURE_NO_WARNINGS ");
	cmd_append("/I:src /Fe:");
	cmd_append(output_path);
	
#   if defined(RELEASE_BUILD)
	cmd_append("/GL /Ox /WX /GS- /DNDEBUG ");
#   else
	cmd_append("/MTd /Od /WX /Zi /D_DEBUG /RTC1 ");
#   endif
	
	for (size_t i = 0; i < count; i++) {
		cmd_append(filepaths[i]);
		cmd_append(" ");
	}
	
	printf("CMD: %s\n", command_buffer);
	cmd_dump(cmd_run());
}

inline static void builder_compile_cc(size_t count, const char* filepaths[], const char* output_path) {
#if defined(__clang__)
	const char* cc_command = "clang";
#elif defined(__GNUC__)
	const char* cc_command = "gcc";
#endif
	
	// compile object files
	FILE** streams = malloc(count * sizeof(FILE*));
	
	for (size_t i = 0; i < count; i++) {
		const char* input = filepaths[i];
		
		// Find last dot, if we can't default to end
		const char* ext = strrchr(input, '.');
		if (!ext) ext = input + strlen(input);
		
		// Find last slash, if we can't default to start
		const char* slash = strrchr(input, '\\');
		if (!slash) slash = strrchr(input, '/');
		if (!slash) slash = input;
		
		char output[PATH_MAX];
		snprintf(output, PATH_MAX, "%.*s", (int)(ext - slash), slash);
		
		cmd_append(cc_command);
		cmd_append(" -march=haswell -maes -Werror -Wall -Wno-trigraphs -Wno-unused-function ");
		
#if defined(__clang__)
		cmd_append("-Wno-gnu-designator ");
#       if USE_DA_ASAN
		cmd_append("-fsanitize=address ");
#       endif
#endif
		
		cmd_append("-I src ");
		
#if 1 // 0 if you want to emit preprocessed output for all files
		cmd_append("-c -o build");
		cmd_append(output);
		
#       if _WIN32
		cmd_append(".obj -D_CRT_SECURE_NO_WARNINGS -g -gcodeview ");
#       else
		cmd_append(".o -g ");
#       endif
		
		cmd_append(filepaths[i]);
#else
		cmd_append(" -E ");
		cmd_append(filepaths[i]);
		cmd_append(" > build");
		cmd_append(output);
		cmd_append(".c");
#endif
		
		streams[i] = cmd_run();
	}
	
	bool success = true;
	for (size_t i = 0; i < count; i++) {
		char buffer[4096];
		if (fread(buffer, sizeof(buffer), sizeof(char), streams[i])) {
			printf("~~~ ERRORS '%s' ~~~\n", filepaths[i]);
			
			do {
				printf("%s", buffer);
			} while (fread(buffer, 1, sizeof(buffer), streams[i]));
			
			int code = pclose(streams[i]);
			printf("\n");
			
			if (code) {
				printf("%s: exited with code %d\n", filepaths[i], code);
				success = false;
			}
		}
	}
	
	if (!success) {
		fprintf(stderr, "Compilation errors... fix em\n");
		exit(69420);
	}
	
#if _WIN32
	// Link everything together
	cmd_append("lld-link ");
	cmd_append("tildebackend.lib build\\*.obj ");
	
	cmd_append("/nologo /machine:amd64 /subsystem:console /debug:full /defaultlib:libcmt ");
	cmd_append("/out:build\\");
	cmd_append(output_path);
	
	cmd_append(".exe ");
	cmd_append("/pdb:build\\");
	cmd_append(output_path);
	cmd_append(".pdb ");
	cmd_append("ole32.lib Advapi32.lib OleAut32.lib DbgHelp.lib");
#elif defined(__clang__)
	// It's just significantly more convenient to just use clang as the
	// linker, plus we can enable the address sanitizers
	cmd_append("clang -o build/cuik ");
	cmd_append("build/*.o ./tildebackend.a -lc -lm -lpthread ");
#   if USE_DA_ASAN
	cmd_append("-fsanitize=address ");
	
	printf("Using address sanitizer :p\n");
#   endif
#else
	cmd_append("ld -o build/cuik ");
	cmd_append("build/*.o ./tildebackend.a -lc -lm -lpthread ");
	cmd_append("/usr/lib/x86_64-linux-gnu/crt1.o ");
	cmd_append("/usr/lib/x86_64-linux-gnu/crti.o ");
	cmd_append("/usr/lib/x86_64-linux-gnu/crtn.o ");
	cmd_append("/usr/lib/x86_64-linux-gnu/libc_nonshared.a ");
	cmd_append("--dynamic-linker /lib64/ld-linux-x86-64.so.2 ");
#endif
	
	cmd_dump(cmd_run());
}

inline static void builder_compile(size_t count, const char* filepaths[], const char* output_path) {
#if UNIX_STYLE
	builder_compile_cc(count, filepaths, output_path);
#else
	builder_compile_msvc(count, filepaths, output_path);
#endif
	
	char temp[PATH_MAX];
	
	const char* path;
	FileIter it = file_iter_open("build/");
	while ((path = file_iter_next(&it))) {
		if (str_ends_with(path, ".o") ||
			str_ends_with(path, ".obj")) {
			snprintf(temp, PATH_MAX, "build/%s", path);
			remove(temp);
		}
	}
	file_iter_close(&it);
}
