// This is the build script for TB, real simple imo
// just call it with the C11 compiler of your choice
//
// It's inspired by nobuild but different
#include "compile.h"

#if defined(_WIN32)
#define OUTPUT_EXEC_NAME "build" SLASH "cuik.exe"
#else
#define OUTPUT_EXEC_NAME "build" SLASH "cuik"
#endif

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
	
	"src/targets/x64.c",
	
	"src/front/lexer.c",
	"src/front/preproc.c",
	"src/front/parser.c",
	"src/front/atoms.c",
	"src/front/const_eval.c",
	"src/front/types.c",
	"src/front/sema.c",
	"src/front/ast_dump.c",
	
	"src/back/ir_gen.c",
	"src/back/linker.c",
	"src/back/microsoft_craziness.cpp",
	
	"src/ext/stb_ds.c",
	"src/ext/threadpool.c",
#if defined(_WIN32)
	"src/ext/threads_msvc.c",
#else
	"src/ext/threads_posix.c"
#endif
};
enum { INPUT_FILE_COUNT = sizeof(INPUT_FILES) / sizeof(INPUT_FILES[0]) };

#if defined(__GNUC__) || defined(__clang__)
#define UNIX_STYLE 1
#elif defined(_MSC_VER)
#define UNIX_STYLE 0
#else
#define UNIX_STYLE 1
#endif

int tests_working = 0;
int number_of_tests = 0;

void expect_return_value(const char* path, int expected) {
	number_of_tests++;
	
	printf("Attempt \"%s\"...   ", path);
	
	int code;
	char cmd[1024];
	
	// Compile
	snprintf(cmd, 1024, "cuik build %s.c", path);
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
	
	printf("Attempt \"%s\"...   ", path);
	
	int code;
	char cmd[1024];
	
	// Compile
	snprintf(cmd, 1024, "cuik build %s.c", path);
	code = system(cmd);
	if (code != 0) {
		printf("Fail to compile! (code: %d)\n", code);
		return;
	}
	
	// Run
	snprintf(cmd, 1024, "%s.exe", path);
	FILE* stream = _popen(cmd, "rb");
	
	char data[1024];
	size_t length = fread(data, sizeof(data), sizeof(char), stream);
	fclose(stream);
	
	if (length == strlen(expected) && memcmp(data, expected, length) != 0) {
		printf("Fail to execute! (string: %s)\n", data);
		return;
	}
	
	// Success!
	printf("Success!\n");
	tests_working++;
}

int main(int argc, char** argv) {
	// don't wanna buffer stdout
	setvbuf(stdout, NULL, _IONBF, 0);
	
#if defined(_WIN32)
	// sets environment vars for compiler
	system("call vcvars64");
#endif
	
#if defined(__GNUC__)
	printf("Compiling on GCC %d.%d...\n", __GNUC__, __GNUC_MINOR__);
	
	cmd_append("gcc -march=haswell -maes -Werror -Wall -Wno-trigraphs -Wno-gnu-designator -Wno-unused-function ");
	cmd_append("-I src -o " OUTPUT_EXEC_NAME " ");
#elif defined(__clang__)
	printf("Compiling on Clang %d.%d.%d...\n", __clang_major__, __clang_minor__, __clang_patchlevel__);
	
	cmd_append("clang -march=haswell -maes -Werror -Wall -Wno-trigraphs -Wno-gnu-designator -Wno-unused-function ");
	cmd_append("-I src -o " OUTPUT_EXEC_NAME " ");
#elif defined(_MSC_VER)
	printf("Compiling on MSVC %d.%d...\n", _MSC_VER / 100, _MSC_VER % 100);
	
	cmd_append("cl /Fo:build\\ /MP /arch:AVX /D_CRT_SECURE_NO_WARNINGS ");
	cmd_append("/I:src /Fe:" OUTPUT_EXEC_NAME " ");
#endif
	
#if UNIX_STYLE
#  if defined(RELEASE_BUILD)
	cmd_append("-O2 -DNDEBUG ");
#  else
	cmd_append("-O0 ");
#  endif
	
#  if defined(_WIN32)
	cmd_append("-D_CRT_SECURE_NO_WARNINGS -g -gcodeview -lole32 -lAdvapi32 -lOleAut32 -lDbgHelp ");
#else
	cmd_append("-g");
#  endif
#else
#  if defined(RELEASE_BUILD)
	cmd_append("/GL /Ox /WX /GS- /DNDEBUG ");
#  else
	cmd_append("/MTd /Od /WX /Zi /D_DEBUG /RTC1 ");
#  endif
#endif
	
	cmd_append("tildebackend.lib ");
	
	// all the source files
	for (size_t i = 0; i < INPUT_FILE_COUNT; i++) {
		cmd_append(INPUT_FILES[i]);
		cmd_append(" ");
	}
	
	printf("CMD: %s\n", command_buffer);
	cmd_run();
	printf("Outputting cuik to: " OUTPUT_EXEC_NAME "...\n");
	
	if (argc > 1 && strcmp(argv[1], "test") == 0) {
		printf("\n\n\n");
		printf("Running tests...\n");
		
		expect_return_value("tests"SLASH"the_increment"SLASH"iso"SLASH"program_termination", 42);
		expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"printf_test", "Hello Hel Goodb 127 63 0 254 63 0 32000 32767 4 17 65532 65530 4 16 32000 32767 4 17 65532 65530 4 16 4294967295 6731943 2147483646 16 123456789 57486731943 985429 9123456 1.000000 123000.000000 0.100 0.234 3.000000");
		expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"crc32_test", "691daa2f");
		expect_stdout("tests"SLASH"the_increment"SLASH"iso"SLASH"ternary_test", "128 16 32 64 128 128");
		expect_stdout("tests"SLASH"the_increment"SLASH"cuik"SLASH"function_literal", "-2147483648 -743 -2 0 2 4 99\n");
		
		printf("===============   Tests (%d succeeded out of %d)   ===============\n", tests_working, number_of_tests);
	} else {
		
	}
	
	return 0;
}
