#include "driver_utils.h"

#define NL_STRING_MAP_IMPL
#include "string_map.h"

#ifndef _WIN32
#include <unistd.h>
#endif

char cuik_include_directory[MAX_PATH];
char cuik_library_directory[MAX_PATH];

void cuik_detect_crt_include() {
#ifdef _WIN32
	char compiler_path[MAX_PATH];
	GetModuleFileNameA(NULL, compiler_path, MAX_PATH);
	
	// cuik/build/exe
	// cuik/
	// cuik/crt
	int slashes_hit = 0;
	const char* end = compiler_path + strlen(compiler_path);
	while (slashes_hit < 2 && end-- != compiler_path) {
		if (*end == '/') slashes_hit++;
		else if (*end == '\\') slashes_hit++;
	}
	
	if (slashes_hit < 2) {
		fprintf(stderr, "Could not locate Cuik include directory from %s\n", compiler_path);
		abort();
	}
	
	sprintf_s(cuik_include_directory, MAX_PATH, "%.*s\\crt\\include\\", (int)(end - compiler_path), compiler_path);
	sprintf_s(cuik_library_directory, MAX_PATH, "%.*s\\crt\\src\\", (int)(end - compiler_path), compiler_path);
#else
	char compiler_path[PATH_MAX];
	if (getcwd(compiler_path, PATH_MAX) == NULL) {
		fprintf(stderr, "error: could not locate Cuik include directory\n");
		abort();
	}
	
	// cuik/build/exe
	// cuik/
	// cuik/crt
	int slashes_hit = 0;
	const char* end = compiler_path + strlen(compiler_path);
	while (slashes_hit < 1 && end-- != compiler_path) {
		if (*end == '/') slashes_hit++;
		else if (*end == '\\') slashes_hit++;
	}
	
	if (slashes_hit < 1) {
		fprintf(stderr, "Could not locate Cuik include directory from %s\n", compiler_path);
		abort();
	}
	
	sprintf_s(cuik_include_directory, 260, "%.*s/crt/include/", (int)(end - compiler_path), compiler_path);
	sprintf_s(cuik_library_directory, 260, "%.*s/crt/src/", (int)(end - compiler_path), compiler_path);
#endif
}

void cuik_set_cpp_defines(CPP_Context* cpp) {
#ifdef _WIN32
	if (s_vswhere.windows_sdk_include == NULL) {
		printf("warning: could not automatically find WinSDK include path\n");
	}
	
	if (s_vswhere.vs_include_path == NULL) {
		printf("warning: could not automatically find VS include path\n");
	}
#endif
	
	// CuikC specific
	cpp_define(cpp, "__CUIKC__", STR(CUIK_COMPILER_MAJOR));
	cpp_define(cpp, "__CUIKC_MINOR__", STR(CUIK_COMPILER_MINOR));

	// GNU C
	cpp_define(cpp, "__BYTE_ORDER__", "1");
	cpp_define(cpp, "__ORDER_LITTLE_ENDIAN", "1");
	cpp_define(cpp, "__ORDER_BIG_ENDIAN", "2");

	// DO NOT REMOVE THESE, IF THEY'RE MISSING THE PREPROCESSOR
	// WILL NOT DETECT THEM
	cpp_define_empty(cpp, "__FILE__");
	cpp_define_empty(cpp, "L__FILE__");
	cpp_define_empty(cpp, "__LINE__");
	
	// Standard C macros
	cpp_define(cpp, "__STDC__", "1");
	cpp_define(cpp, "__STDC_VERSION__", "201112L"); // C11
	
	// currently there's no freestanding mode but if there was this would be
	// turned off for it
	cpp_define(cpp, "__STDC_HOSTED__", settings.freestanding ? "0" : "1");
	cpp_define(cpp, "__STDC_NO_COMPLEX__", "1");
	cpp_define(cpp, "__STDC_NO_VLA__", "1");
	cpp_define(cpp, "__STDC_NO_THREADS__", "1");
	
	{
		// The time of translation of the preprocessing translation unit
		static const char mon_name[][4] = {
			"Jan", "Feb", "Mar", "Apr", "May", "Jun",
			"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
		};
		
		time_t rawtime;
		time(&rawtime);
		
		struct tm* timeinfo = localtime(&rawtime);
		
		// Mmm dd yyyy
		char date_str[20];
		snprintf(date_str, 20, "\"%.3s%3d %d\"", mon_name[timeinfo->tm_mon], timeinfo->tm_mday, 1900 + timeinfo->tm_year);
		cpp_define(cpp, "__DATE__", date_str);
		
		// hh:mm:ss
		char time_str[20];
		snprintf(time_str, 20, "\"%.2d:%.2d:%.2d\"", timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
		cpp_define(cpp, "__TIME__", time_str);
	}
	
	if (settings.is_debug_build) {
		cpp_define_empty(cpp, "_DEBUG");
	} else {
		cpp_define_empty(cpp, "NDEBUG");
	}
	
	cpp_define(cpp, "static_assert", "_Static_assert");
	cpp_define(cpp, "typeof", "_Typeof");
	
	cpp_add_include_directory(cpp, cuik_include_directory);
	
	// platform specific stuff
	if (target_system == TB_SYSTEM_WINDOWS) {
#ifdef _WIN32
		// WinSDK includes
		char filepath[MAX_PATH];
		
		if (snprintf(filepath, 260, "%S\\ucrt\\", s_vswhere.windows_sdk_include) > MAX_PATH) {
			printf("internal compiler error: WinSDK include directory too long!\n");
			abort();
		}
		cpp_add_include_directory(cpp, filepath);
		
		if (snprintf(filepath, 260, "%S\\um\\", s_vswhere.windows_sdk_include) > MAX_PATH) {
			printf("internal compiler error: WinSDK include directory too long!\n");
			abort();
		}
		cpp_add_include_directory(cpp, filepath);
		
		if (snprintf(filepath, 260, "%S\\shared\\", s_vswhere.windows_sdk_include) > MAX_PATH) {
			printf("internal compiler error: WinSDK include directory too long!\n");
			abort();
		}
		cpp_add_include_directory(cpp, filepath);
		
		// VS include
		if (s_vswhere.vs_include_path) {
			if (snprintf(filepath, 260, "%S\\", s_vswhere.vs_include_path) > MAX_PATH) {
				printf("internal compiler error: VS include directory too long!\n");
				abort();
			}
			cpp_add_include_directory(cpp, filepath);
		} else {
			// NOTE(NeGate): hacky but apparently the address sanitizer stops me from
			// getting the VS include path so i'll be passing some not-so-reasonable-default
			cpp_add_include_directory(cpp, "W:\\Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.29.30133\\include\\");
		}
#endif
		
		cpp_define_empty(cpp, "_MT");
		cpp_define_empty(cpp, "_CRT_NONSTDC_NO_WARNINGS");
		cpp_define_empty(cpp, "_CRT_SECURE_NO_WARNINGS");
		cpp_define_empty(cpp, "_NO_CRT_STDIO_INLINE");
		
		// we pretend to be a modern MSVC compiler
		cpp_define(cpp, "_WIN32", "1");
		cpp_define(cpp, "_MSC_EXTENSIONS", "1");
		cpp_define(cpp, "_MSC_VER", "1929");
		cpp_define(cpp, "_MSC_FULL_VER", "192930133");
		cpp_define(cpp, "_WIN32_WINNT", "0x0A00");
		cpp_define(cpp, "NTDDI_VERSION", "0x0A000008");
		
		// wrappers over MSVC based keywords and features
		cpp_define(cpp, "__int8", "char");
		cpp_define(cpp, "__int16", "short");
		cpp_define(cpp, "__int32", "int");
		cpp_define(cpp, "__int64", "long long");
		cpp_define(cpp, "__pragma(x)", "_Pragma(#x)");
		cpp_define(cpp, "__inline", "inline");
		cpp_define(cpp, "__forceinline", "inline");
		cpp_define(cpp, "__signed__", "signed");
		cpp_define(cpp, "__alignof", "_Alignof");
		cpp_define(cpp, "__CRTDECL", "__cdecl");
		
		// things we don't handle yet so we just remove them
		cpp_define_empty(cpp, "_Frees_ptr_");
		cpp_define_empty(cpp, "__unaligned");
		cpp_define_empty(cpp, "__analysis_noreturn");
		cpp_define_empty(cpp, "__ptr32");
		cpp_define_empty(cpp, "__ptr64");
	} else {
		// TODO(NeGate): Automatically detect these somehow...
#ifdef __linux__
		cpp_add_include_directory(cpp, "/usr/lib/gcc/x86_64-linux-gnu/10/include/");
		cpp_add_include_directory(cpp, "/usr/include/x86_64-linux-gnu/");
		cpp_add_include_directory(cpp, "/usr/local/include/");
		cpp_add_include_directory(cpp, "/usr/include/");
#endif
	}
	
	target_desc.set_defines(cpp);
}

TranslationUnit* cuik_compile_file(CompilationUnit* cu, const char* path, 
								   size_t include_count, const char** includes,
								   bool frontend_only) {
	TranslationUnit* tu = calloc(1, sizeof(TranslationUnit));
	tu->hack.name = settings.hack_type_printer_name;
	
	CPP_Context cpp_ctx;
	timed_block("preprocess: %s", path) {
		cpp_init(&cpp_ctx);
		cuik_set_cpp_defines(&cpp_ctx);
		
		// add extra include paths
		for (size_t i = 0; i < include_count; i++) {
			cpp_add_include_directory(&cpp_ctx, includes[i]);
		}
		
		tu->tokens = cpp_process(&cpp_ctx, path);
		
		cpp_finalize(&cpp_ctx);
	}
	
	timed_block("parse %s", path) {
		translation_unit_parse(tu, path);
		crash_if_reports(REPORT_ERROR);
	}
	
	// Semantics pass
	timed_block("sema %s", path) {
		sema_pass(cu, tu, frontend_only);
		crash_if_reports(REPORT_ERROR);
	}
	
	// pass off to the compilation unit
	compilation_unit_append(cu, tu);
	
	// free any TU resources (including any cached file refs)
	// TODO(NeGate): actually delete any cached files...
	arrfree(tu->tokens.tokens);
	cpp_deinit(&cpp_ctx);
	return tu;
}
