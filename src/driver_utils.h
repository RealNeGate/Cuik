#include <common.h>
#include <timer.h>
#include <settings.h>
#include <crash_handler.h>
#include <targets/targets.h>
#include <compilation_unit.h>

// Frontend
#include <front/preproc.h>
#include <front/parser.h>
#include <front/sema.h>

// Backend
#include <back/ir_gen.h>
#include <back/linker.h>
#include <ext/threads.h>

#if _WIN32
// This is used to detect VS and WinSDK includes and library folders
#include <back/microsoft_craziness.h>
#endif

#define CUIK_COMPILER_MAJOR 0
#define CUIK_COMPILER_MINOR 1

// %CUIK%/crt/include/
extern char cuik_include_directory[MAX_PATH];
// %CUIK%/crt/src/
extern char cuik_library_directory[MAX_PATH];

void cuik_detect_crt_include();
void cuik_set_cpp_defines(CPP_Context* cpp);
bool cuik_find_include_file(char output[MAX_PATH], const char* path);

// NOTE(NeGate): this is thread-safe, just call it from any thread
// and it should be fine :p
TranslationUnit* cuik_compile_file(CompilationUnit* cu, const char* path, bool frontend_only);
