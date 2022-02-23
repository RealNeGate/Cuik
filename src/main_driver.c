// main_driver.c is the entrypoint to the compiler and should also
// act as a tutorial to writing a custom driver. Currently it doesn't 
// support multiple input files but that'll be next.
#include <common.h>
#include <timer.h>
#include <settings.h>
#include <crash_handler.h>
#include <targets/targets.h>

// Frontend
#include <front/preproc.h>
#include <front/parser.h>

// TODO(NeGate): remove the mid/ and just put it into the front/
#include "mid/sema.h"

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

typedef enum CompilerMode {
	COMPILER_MODE_NONE,
	
	// print version
	COMPILER_MODE_VERSION,
	
	// continuously compiled when changed are made to the file
	// from there it's disassembled and shown in the terminal.
	COMPILER_MODE_LIVE,
	
	// only does preproc and returns a text form of that annotation
	COMPILER_MODE_PREPROC,
	
	// only does preproc, lex, parse and type checking without
	// emitting any output other than errors
	COMPILER_MODE_CHECK,
	
	// compiles an executable unless -c is marked
	COMPILER_MODE_BUILD,
	
	// compile & run
	COMPILER_MODE_RUN,
} CompilerMode;

static TranslationUnit translation_unit;

// %CUIK%/crt/include/
static char cuik_include_directory[MAX_PATH];

////////////////////////////////
// Standard compilation
////////////////////////////////
// Simple thread pool
static thrd_t threads[TB_MAX_THREADS];

static atomic_size_t tasks_reserved;
static atomic_size_t tasks_complete;
static atomic_bool is_running;
static size_t tasks_count;
static size_t tasks_munch_size;

// signalled when it's about to finish to notify the main
// thread to wake up because it's about to finish with the
// parallel dispatch
static cnd_t tasks_condition;
static mtx_t tasks_mutex;

static const char* cuik_source_file;
static char cuik_file_no_ext[MAX_PATH];

static int task_thread(void* param) {
	while (is_running) {
		size_t munch = tasks_munch_size;
		size_t t = atomic_fetch_add(&tasks_reserved, munch);
		
		if (t+(munch-1) >= tasks_count) {
			if (t < tasks_count) {
				for (size_t i = t; i < tasks_count; i++) {
					irgen_top_level_stmt(&translation_unit, translation_unit.top_level_stmts[i]);
				}
				
				tasks_complete += (tasks_count - t);
			}
			
			if (t <= tasks_count) {
				cnd_signal(&tasks_condition);
			}
			
			while (tasks_reserved >= tasks_count) { 
				thrd_yield();
				if (!is_running) return 0;
			}
			continue;
		}
		
		for (size_t i = 0; i < munch; i++) {
			irgen_top_level_stmt(&translation_unit, translation_unit.top_level_stmts[t+i]);
		}
		tasks_complete += munch;
	}
	
	arena_free();
	return 0;
}

static void set_preprocessor_info(CPP_Context* cpp) {
	// CuikC specific
	cpp_define(cpp, "__CUIKC__", STR(CUIK_COMPILER_MAJOR));
	cpp_define(cpp, "__CUIKC_MINOR__", STR(CUIK_COMPILER_MINOR));
	
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
	cpp_define(cpp, "__STDC_HOSTED__", "1");
	
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
	
	cpp_define_empty(cpp, "__STDC_NO_COMPLEX__");
	cpp_define_empty(cpp, "__STDC_NO_VLA__");
	cpp_define_empty(cpp, "__STDC_NO_THREADS__");
	
	cpp_add_include_directory(cpp, cuik_include_directory);
	
	// platform specific stuff
	if (target_system == TB_SYSTEM_WINDOWS) {
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
		if (snprintf(filepath, 260, "%S\\", s_vswhere.vs_include_path) > MAX_PATH) {
			printf("internal compiler error: VS include directory too long!\n");
			abort();
		}
		cpp_add_include_directory(cpp, filepath);
		
		cpp_define_empty(cpp, "_DEBUG");
		cpp_define_empty(cpp, "_MT");
		cpp_define_empty(cpp, "_CRT_NONSTDC_NO_WARNINGS");
		cpp_define_empty(cpp, "_CRT_SECURE_NO_WARNINGS");
		
		// we pretend to be a modern MSVC compiler
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
		cpp_add_include_directory(cpp, "/usr/lib/gcc/x86_64-linux-gnu/10/include/");
		cpp_add_include_directory(cpp, "/usr/local/include/");
		cpp_add_include_directory(cpp, "/usr/include/");
	}
	
	target_desc.set_defines(cpp);
}

// essentially a parallel_for which performs IR gen on top level statements
static void dispatch_tasks(size_t count) {
	tasks_count = count;
	tasks_complete = 0;
	tasks_reserved = 0;
	
	// wait until it's completed
	cnd_wait(&tasks_condition, &tasks_mutex);
    while (tasks_complete < tasks_count) {
		thrd_yield();
	}
}

static void compile_project(TB_Arch arch, TB_System sys, bool is_multithreaded) {
	// Preprocess file
	CPP_Context cpp_ctx;
	cpp_init(&cpp_ctx);
	set_preprocessor_info(&cpp_ctx);
	
	TokenStream s;
	timed_block("preprocess") {
		s = ir_gen_tokens = cpp_process(&cpp_ctx, cuik_source_file);
	}
	
	cpp_finalize(&cpp_ctx);
	
	// Parse
	timed_block("parse") {
		atoms_init();
		
		translation_unit = parse_file(&s);
		crash_if_reports(REPORT_ERROR);
	}
	
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(arch, sys, &features);
	irgen_init();
	
	// Semantics pass
	timed_block("semantics") {
		sema_remove_unused(&translation_unit);
		
		for (size_t i = 0, count = arrlen(translation_unit.top_level_stmts); i < count; i++) {
			sema_check(&translation_unit, translation_unit.top_level_stmts[i]);
		}
		crash_if_reports(REPORT_ERROR);
	}
	
	if (settings.print_ast) {
		ast_dump(&translation_unit, stdout);
	}
	
	// Generate IR
	// TODO(NeGate): Globals amirite... yea maybe i'll maybe move the TB_Module from the
	// globals but at the same time, it's a thread safe interface for the most part you're
	// supposed to keep a global one and use it across multiple threads.
	if (!is_multithreaded) {
		timed_block("ir gen & compile") {
			TB_FeatureSet features = { 0 };
			mod = tb_module_create(arch, sys, &features);
			
			// IR generation
			for (size_t i = 0, count = arrlen(translation_unit.top_level_stmts); i < count; i++) {
				irgen_top_level_stmt(&translation_unit, translation_unit.top_level_stmts[i]);
			}
		}
	} else {
		timed_block("ir gen & compile") {
			is_running = true;
			cnd_init(&tasks_condition);
			mtx_init(&tasks_mutex, mtx_plain);
			
			// How many statements should each thread even grab
			tasks_munch_size = (arrlen(translation_unit.top_level_stmts) / settings.num_of_worker_threads) + 1;
			if (tasks_munch_size < 5) tasks_munch_size = 5;
			
			// Divide and conquer!!!
			for (int i = 0; i < settings.num_of_worker_threads; i++) {
				thrd_create(&threads[i], task_thread, NULL);
			}
			
			dispatch_tasks(arrlen(translation_unit.top_level_stmts));
			
			arrfree(s.tokens);
			arrfree(translation_unit.top_level_stmts);
			
			// Just let's the threads know that 
			// they might need to die now
			tasks_count = 0;
			tasks_complete = 0;
			
			is_running = false;
			for (int i = 0; i < settings.num_of_worker_threads; i++) {
				thrd_join(threads[i], NULL);
			}
			mtx_destroy(&tasks_mutex);
			cnd_destroy(&tasks_condition);
		}
	}
	
	irgen_deinit();
	cpp_deinit(&cpp_ctx);
	arena_free();
	atoms_deinit();
	
	// Compile
	if (!tb_module_compile(mod)) abort();
}

////////////////////////////////
// Cuik live compilation
////////////////////////////////
#include <signal.h>
#include <setjmp.h>

#if _WIN32
static bool disassemble_object_file() {
	clock_t t1 = clock();
	
	static wchar_t cmd_line[260];
	swprintf(cmd_line, 260, L"%s\\dumpbin.exe /nologo /disasm:nobytes %S.obj", s_vswhere.vs_exe_path, cuik_file_no_ext);
	
	STARTUPINFOW si = {
		.cb = sizeof(STARTUPINFOW),
		.dwFlags = STARTF_USESTDHANDLES,
		.hStdInput = GetStdHandle(STD_INPUT_HANDLE),
		.hStdError = GetStdHandle(STD_ERROR_HANDLE),
		.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE)
	};
	
	PROCESS_INFORMATION pi = {};
	if (!CreateProcessW(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
		printf("Disassembly failed!\n");
		return false;
	}
	
	// Wait until child process exits.
	WaitForSingleObject(pi.hProcess, INFINITE);
	
	// Close process and thread handles. 
	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);
	
	clock_t t2 = clock();
	double delta_ms = (t2 - t1) / (double)CLOCKS_PER_SEC;
	printf("disassembly took %.03f seconds\n", delta_ms);
	return true;
}

static uint64_t get_last_write_time(const char filepath[]) {
	WIN32_FIND_DATA data;
	HANDLE handle = FindFirstFile(filepath, &data);
	
	ULARGE_INTEGER i;
	i.LowPart = data.ftLastWriteTime.dwLowDateTime;
	i.HighPart = data.ftLastWriteTime.dwHighDateTime;
	
	FindClose(handle);
	return i.QuadPart;
}

// NOTE(NeGate): This is lowkey a mess but essentially if
// the live-compiler crashes we wanna just ignore it and continue
static jmp_buf live_compiler_savepoint;
static void live_compiler_abort(int signo) {
	longjmp(live_compiler_savepoint, 1);
}

static bool live_compile() {
	char obj_output_path[MAX_PATH];
	snprintf(obj_output_path, 260, "%s.obj", cuik_file_no_ext);
	
	uint64_t original_last_write = get_last_write_time(cuik_source_file);
	while (true) {
		system("cls");
		
		// bootlegy exception handler...
		if (signal(SIGABRT, live_compiler_abort) == SIG_ERR) {
			printf("Failed to set signal handler.\n");
			return false;
		}
		
		if (setjmp(live_compiler_savepoint) == 0) {
			compile_project(target_arch, target_system, true);
			
			if (!tb_module_export(mod, obj_output_path, false)) abort();
			tb_module_destroy(mod);
			
			disassemble_object_file();
		}
		
		clear_any_reports();
		
		// Wait for the user to save again
		while (true) {
			uint64_t current_last_write = get_last_write_time(cuik_source_file);
			if (original_last_write != current_last_write) {
				original_last_write = current_last_write;
				
				// wait for it to finish writing before trying to compile
				int ticks = 0;
				while (GetFileAttributesA(cuik_source_file) == INVALID_FILE_ATTRIBUTES) {
					SleepEx(1, FALSE);
					
					if (ticks++ > 100) {
						printf("Live compiler timeout!");
						return false;
					}
				}
				break;
			}
			
			SleepEx(100, FALSE);
		}
	}
}
#else
static bool disassemble_object_file() {
	return false;
}

static bool live_compile() {
	printf("No live compiler supported");
    return false;
}
#endif

////////////////////////////////
// Preprocessor dump
////////////////////////////////
static bool dump_tokens() {
	char output_path[MAX_PATH];
	snprintf(output_path, 260, "%s.i", cuik_file_no_ext);
	
	// Preprocess file
	uint64_t t1 = timer_now();
	TokenStream s;
	{
		CPP_Context cpp_ctx;
		cpp_init(&cpp_ctx);
		set_preprocessor_info(&cpp_ctx);
		
		s = cpp_process(&cpp_ctx, cuik_source_file);
		
		cpp_finalize(&cpp_ctx);
	}
	uint64_t t2 = timer_now();
	double elapsed = (t2 - t1) * timer_freq;
	printf("preprocessor took %.03f seconds\n", elapsed);
	
	FILE* f = fopen(output_path, "w");
	if (!f) {
		printf("Could not open file a.txt\n");
		return false;
	}
	
	const unsigned char* last_file = NULL;
	int last_line = 0;
	
	for (size_t i = 0, cc = arrlen(s.tokens); i < cc; i++) {
		Token* t = &s.tokens[i];
		SourceLoc* loc = &s.line_arena[t->location];
		
		if (last_file != loc->file) {
			char str[MAX_PATH];
			
			// TODO(NeGate): Kinda shitty but i just wanna duplicate
			// the backslashes to avoid them being treated as an escape
			const char* in = (const char*)loc->file;
			char* out = str;
			
			while (*in) {
				if (*in == '\\') {
					*out++ = '\\';
					*out++ = '\\';
					in++;
				} else {
					*out++ = *in++;
				}
			}
			*out++ = '\0';
			
			fprintf(f, "\n#line %d \"%s\"\t", loc->line, str);
			last_file = loc->file;
		}
		
		if (last_line != loc->line) {
			fprintf(f, "\n/* line %3d */\t", loc->line);
			last_line = loc->line;
		}
		
		fprintf(f, "%.*s ", (int)(t->end - t->start), t->start);
	}
	
	fclose(f);
	return true;
}

////////////////////////////////
// Entry & CLI
////////////////////////////////
static void print_help(const char* executable_path) {
	printf("Usage:\n"
		   "  %s [command] [filepath] [flags]\n"
		   "  \n"
		   "Commands:\n"
		   "  build      Compiles the input file/directory\n"
		   "  check      Check for errors in the input file/directory.\n"
		   "  preproc    Runs the preprocessor on a file\n"
		   "  run        Compiles & runs the input file/directory\n"
		   "  live       Live recompilation of C into the terminal as you edit\n"
		   "  version    Prints the installed compiler's version\n"
		   "  bindgen    Generate API binding descriptions\n"
		   "  help       More detailed help for the input command\n"
		   "  \n",
		   executable_path);
}

static void print_version(const char* install_dir) {
	printf("cuik version %d.%d\n",         CUIK_COMPILER_MAJOR, CUIK_COMPILER_MINOR);
	printf("install directory: %s\n",      install_dir);
	printf("cuik include directory: %s\n", cuik_include_directory);
	printf("windows sdk include: %S\n",    s_vswhere.windows_sdk_include);
	printf("visual studio include: %S\n",  s_vswhere.vs_include_path);
}

int main(int argc, char* argv[]) {
	// We hook the crash handler to create crash dumps
	hook_crash_handler();
	
	if (argc == 1) {
		printf("Expected command!\n");
		print_help(argv[0]);
		return 1;
	}
	
#ifdef _WIN32
	// This is used to detect includes for the preprocessor
	// and library paths for the linker
	s_vswhere = MicrosoftCraziness_find_visual_studio_and_windows_sdk();
#endif
	
	// parse command
	CompilerMode mode = COMPILER_MODE_NONE;
	
	const char* cmd = argv[1];
	if (strcmp(cmd, "live") == 0) mode = COMPILER_MODE_LIVE;
	else if (strcmp(cmd, "--version") == 0) mode = COMPILER_MODE_VERSION;
	else if (strcmp(cmd, "-v") == 0) mode = COMPILER_MODE_VERSION;
	else if (strcmp(cmd, "version") == 0) mode = COMPILER_MODE_VERSION;
	else if (strcmp(cmd, "preproc") == 0) mode = COMPILER_MODE_PREPROC;
	else if (strcmp(cmd, "check") == 0) mode = COMPILER_MODE_CHECK;
	else if (strcmp(cmd, "build") == 0) mode = COMPILER_MODE_BUILD;
	else if (strcmp(cmd, "run") == 0) mode = COMPILER_MODE_RUN;
	else {
		printf("Unknown command: %s\n", cmd);
		print_help(argv[0]);
		return 1;
	}
	
	if (mode == COMPILER_MODE_VERSION) {
		print_version(argv[0]);
		return 0;
	}
	
#ifdef _WIN32
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	
	// Just kinda guesses something that seems ok ish for now
	// eventually we'll wanna use all cores but it's not honestly
	// helpful currently since code gen is the only parallel stage.
	settings.num_of_worker_threads = sysinfo.dwNumberOfProcessors - 4;
	if (settings.num_of_worker_threads <= 0) settings.num_of_worker_threads = 1;
	
	target_system = TB_SYSTEM_WINDOWS;
	settings.is_windows_long = true;
#else
	settings.num_of_worker_threads = 1;
	
	target_system = TB_SYSTEM_LINUX;
	settings.is_windows_long = false;
#endif
	
	// Defaults to the host arch as the target
#if defined(_AMD64_) || defined(__amd64__)
	target_arch = TB_ARCH_X86_64;
#elif defined(__aarch64__)
	target_arch = TB_ARCH_AARCH64;
#else
#error "Unsupported host compiler... for now"
#endif
	
	for (size_t i = 2; i < argc; i++) {
		if (argv[i][0] != '-') {
			if (cuik_source_file) {
				printf("main_driver does not support multiple input files... yet...\n");
				return 1;
			}
			
			cuik_source_file = argv[i];
			continue;
		}
		
		char* key = &argv[i][1];
		char* value = key;
		
		// i'll support both : and = for
		// the values on compiler options
		for (; *value; value++) {
			if (*value == '=') break;
			if (*value == ':') break;
		}
		
		// split the key and value
		if (*value) {
			value[0] = '\0';
			value++;
		}
		
		if (strcmp(key, "arch") == 0) {
			if (strcmp(value, "x64") == 0) {
				target_arch = TB_ARCH_X86_64;
			} else {
				printf("unsupported architecture: %s\n", value);
				printf("Supported archs:\n");
				printf("\t-arch=x64\n");
				printf("\n");
				return 1;
			}
		} else if (strcmp(key, "sys") == 0) {
			if (strcmp(value, "windows") == 0) {
				target_system = TB_SYSTEM_WINDOWS;
			} else if (strcmp(value, "linux") == 0) {
				target_system = TB_SYSTEM_LINUX;
			} else {
				printf("unsupported architecture: %s\n", value);
				printf("Supported archs:\n");
				printf("\t-sys=windows\n");
				printf("\t-sys=linux\n");
				printf("\n");
				return 1;
			}
		} else if (strcmp(key, "threads") == 0) {
			int num;
			int matches = sscanf(value, "%d", &num);
			if (matches != 1) {
				printf("expected integer for thread count\n");
				return 1;
			}
			
			if (num < 1 || num > TB_MAX_THREADS) {
				printf("expected thread count between 1-%d\n", TB_MAX_THREADS);
				return 1;
			}
			
			settings.num_of_worker_threads = num;
		} else if (strcmp(key, "emit-ast") == 0) {
			settings.print_ast = true;
		} else if (strcmp(key, "emit-ir") == 0) {
			settings.print_tb_ir = true;
		} else if (strcmp(key, "opt") == 0) {
			settings.optimization_level = TB_OPT_O1;
		} else if (strcmp(key, "obj") == 0) {
			settings.is_object_only = true;
		} else if (strcmp(key, "time") == 0) {
			settings.is_time_report = true;
		} else if (strcmp(key, "debug") == 0) {
			settings.debug_info = true;
		} else if (strcmp(key, "thin-errors") == 0) {
			report_using_thin_errors = true;
		} else if (strcmp(key, "out") == 0) {
			if (*value == '\0') {
				printf("expected path after -out option\n");
				return 1;
			}
			
			settings.output_path = value;
		} else if (strcmp(key, "pedantic") == 0) {
			settings.pedantic = true;
		} else {
			printf("unsupported option: %s\n", key);
			print_help(argv[0]);
			return 1;
		}
	}
	
	if (cuik_source_file == NULL) {
		printf("Expected filename\n");
		return 1;
	}
	
	// Get filename without extension
	{
		const char* ext = strrchr(cuik_source_file, '.');
		if (!ext) ext = cuik_source_file + strlen(cuik_source_file);
		
		memcpy_s(cuik_file_no_ext, 260, cuik_source_file, ext - cuik_source_file);
		cuik_file_no_ext[ext - cuik_source_file] = '\0';
	}
	
	switch (target_arch) {
		case TB_ARCH_X86_64: 
		target_desc = get_x64_target_descriptor();
		break;
		
		default:
		printf("Cannot compile to your target machine");
		return 1;
	}
	
	switch (mode) {
		case COMPILER_MODE_PREPROC: {
			dump_tokens();
			break;
		}
		case COMPILER_MODE_CHECK:
		case COMPILER_MODE_BUILD:
		case COMPILER_MODE_RUN: {
			char obj_output_path[MAX_PATH];
			if (target_system == TB_SYSTEM_WINDOWS) {
				snprintf(obj_output_path, 260, "%s.obj", cuik_file_no_ext);
			} else if (target_system == TB_SYSTEM_LINUX) {
				snprintf(obj_output_path, 260, "%s.o", cuik_file_no_ext);
			} else if (mode != COMPILER_MODE_CHECK) {
				printf("Unsupported object file\n");
				abort();
			}
			
			if (settings.print_tb_ir) {
				// Get filename as *.tbir
				char tbir_filename[MAX_PATH];
				snprintf(tbir_filename, 260, "%s.tbir", cuik_file_no_ext);
				
				tbir_output_file = fopen(tbir_filename, "wb");
			}
			
			timer_init();
			
			// Open profiler file stream
			if (settings.is_time_report) {
				char report_filename[MAX_PATH];
				snprintf(report_filename, 260, "%s.json", cuik_file_no_ext);
				
				timer_open(report_filename);
			}
			
			// Build project
			uint64_t compile_start = timer_now();
			
			timed_block("total") {
				compile_project(target_arch, target_system, true);
				
				timed_block("export") {
					if (settings.print_tb_ir) {
						fclose(tbir_output_file);
					} else if (mode != COMPILER_MODE_CHECK) {
						if (!tb_module_export(mod, obj_output_path, settings.debug_info)) abort();
					}
					
					tb_module_destroy(mod);
				}
			}
			
			uint64_t compile_end = timer_now();
			double elapsed = (compile_end - compile_start) * timer_freq;
			printf("compiled in %f seconds\n", elapsed);
			
			// Close out profiler output (it doesn't include the linking)
			timer_close();
			
			if (!settings.is_object_only && mode != COMPILER_MODE_CHECK && !settings.print_tb_ir) {
				Linker l;
				if (linker_init(&l)) {
					// Add system libpaths
					linker_add_default_libpaths(&l);
					
					// Add input libraries
					linker_add_input_file(&l, "kernel32.lib");
					linker_add_input_file(&l, "user32.lib");
					linker_add_input_file(&l, "Gdi32.lib");
					linker_add_input_file(&l, "Onecore.lib");
					linker_add_input_file(&l, "Onecoreuap.lib");
					linker_add_input_file(&l, "opengl32.lib");
					
					// Add Cuik output
					linker_add_input_file(&l, obj_output_path);
					
					linker_invoke(&l, cuik_file_no_ext, settings.using_winmain ? SUBSYSTEM_WINDOWS : SUBSYSTEM_CONSOLE, true);
					linker_deinit(&l);
					
					if (mode == COMPILER_MODE_RUN) {
						char exe_path[MAX_PATH];
						snprintf(exe_path, 260, "%s.exe", cuik_file_no_ext);
						
						printf("\n\nRunning: %s...\n", exe_path);
						int exit_code = system(exe_path);
						printf("Exit code: %d\n", exit_code);
					}
				}
			}
			break;
		}
		case COMPILER_MODE_LIVE: {
			if (argc < 2) {
				printf("Expected filename\n");
				return 1;
			}
			
			live_compile();
			break;
		}
		default: {
			printf("Not ready yet sorry!\n");
			return 1;
		}
	}
	
	return 0;
}
