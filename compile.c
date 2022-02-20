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
	"src/main.c",
	"src/tls.c",
	"src/diagnostic.c",
	"src/crash_handler.c",
	"src/big_array.c",
	"src/arena.c",
	
	"src/targets/x64.c",
	
	"src/front/lexer.c",
	"src/front/preproc.c",
	"src/front/parser.c",
	"src/front/atoms.c",
	"src/front/const_eval.c",
	"src/front/types.c",
	
	"src/mid/sema.c",
	
	"src/back/ir_gen.c",
	"src/back/linker.c",
	"src/back/microsoft_craziness.cpp",
	
	"src/ext/stb_ds.c",
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
	
	printf("Outputting cuik to: build/cuik.exe...\n");
	return 0;
}
