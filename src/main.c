// TODO(NeGate): This code would love some eyeballs but like those with skills
// in fixing it not judging it because it's self-aware and would rather people
// not hurt it's poor shit-box code feelings.
#include "common.h"
#include "timer.h"
#include "front/preproc.h"
#include "front/parser.h"
#include "front/atoms.h"
#include "mid/sema.h"
#include "back/ir_gen.h"
#include "back/linker.h"
#include "ext/threads.h"
#include "ext/stb_ds.h"
#include <stdatomic.h>
#include "settings.h"

#if _WIN32
#include "back/microsoft_craziness.h"
#endif

// used for the weird stuff in the live compiler
#include <signal.h>
#include <setjmp.h>

#define COMPILER_VERSION "v0.01"

// this is how many IR gen tasks it tries to grab at any one time
#define MAX_MUNCH 8192

static thrd_t threads[TB_MAX_THREADS];

static _Atomic size_t tasks_reserved;
static _Atomic size_t tasks_complete;
static _Atomic bool is_running;
static size_t tasks_count;

// signalled when it's about to finish to notify the main
// thread to wake up because it's about to finish with the
// parallel dispatch
static cnd_t tasks_condition;
static mtx_t tasks_mutex;

// Frontend IR Gen has two stages, one places all
// 0 semantic pass & forward decls
// 1 function bodies
static int frontend_stage;
static TopLevel top_level;

CompilerSettings settings;

static int task_thread(void* param) {
	while (is_running) {
		size_t t = atomic_fetch_add(&tasks_reserved, MAX_MUNCH);
		
		if (t+(MAX_MUNCH-1) >= tasks_count) {
			if (t < tasks_count) {
				for (size_t i = t; i < tasks_count; i++) {
					switch (frontend_stage) {
						case 0: sema_check(top_level, i); break;
						case 1: gen_ir(top_level, i); break;
					}
				}
				
				tasks_complete += (tasks_count - t);
				cnd_signal(&tasks_condition);
			}
			
			while (tasks_reserved >= tasks_count) { 
				thrd_yield();
				if (!is_running) return 0;
			}
			continue;
		}
		
		for (size_t i = 0; i < MAX_MUNCH; i++) {
			switch (frontend_stage) {
				case 0: sema_check(top_level, t+i); break;
				case 1: gen_ir(top_level, t+i); break;
			} 
		}
		tasks_complete += MAX_MUNCH;
	}
	
	arena_free();
	return 0;
}

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

static void print_help(const char* executable_path) {
	printf("Usage:\n"
		   "  %s [command] [arguments] [filepath]\n"
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

static void set_preprocessor_info(CPP_Context* cpp_ctx) {
	// TODO(NeGate): We should implement cross-compilation features, it just
	// means we somehow expose this stuff so that the user can provide it.
	
#ifdef _WIN32
	// TODO(NeGate): Automatically detect these somehow...
	cpp_add_include_directory(cpp_ctx, "W:\\Windows Kits\\10\\Include\\10.0.19041.0\\ucrt\\");
	cpp_add_include_directory(cpp_ctx, "W:\\Windows Kits\\10\\Include\\10.0.19041.0\\um\\");
	cpp_add_include_directory(cpp_ctx, "W:\\Windows Kits\\10\\Include\\10.0.19041.0\\shared\\");
	cpp_add_include_directory(cpp_ctx, "W:\\Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.29.30133\\include\\");
	
	cpp_define_empty(cpp_ctx, "_X86_");
	cpp_define_empty(cpp_ctx, "_WIN32");
	cpp_define_empty(cpp_ctx, "_WIN64");
	cpp_define_empty(cpp_ctx, "_M_X64");
	cpp_define_empty(cpp_ctx, "_M_AMD64");
	cpp_define_empty(cpp_ctx, "_DEBUG");
	cpp_define_empty(cpp_ctx, "_MT");
	cpp_define_empty(cpp_ctx, "_CRT_NONSTDC_NO_WARNINGS");
	cpp_define_empty(cpp_ctx, "_CRT_SECURE_NO_WARNINGS");
	
	// NOTE(NeGate): Hack to make these trigger the preprocessor
	cpp_define_empty(cpp_ctx, "__FILE__");
	cpp_define_empty(cpp_ctx, "__LINE__");
	
	cpp_define(cpp_ctx, "__int8", "char");
	cpp_define(cpp_ctx, "__int16", "short");
	cpp_define(cpp_ctx, "__int32", "int");
	cpp_define(cpp_ctx, "__int64", "long long");
	cpp_define(cpp_ctx, "__pragma(x)", "_Pragma(#x)");
	cpp_define(cpp_ctx, "__inline", "inline");
#else
	// TODO(NeGate): Automatically detect these somehow...
	cpp_add_include_directory(cpp_ctx, "/usr/lib/gcc/x86_64-linux-gnu/10/include/");
	cpp_add_include_directory(cpp_ctx, "/usr/local/include/");
	cpp_add_include_directory(cpp_ctx, "/usr/include/");
#endif
}

static void compile_project(TB_Arch arch, TB_System sys, const char source_file[], bool is_multithreaded) {
	// Preprocess file
	CPP_Context cpp_ctx;
	cpp_init(&cpp_ctx);
	set_preprocessor_info(&cpp_ctx);
	
	TokenStream s = ir_gen_tokens = cpp_process(&cpp_ctx, source_file);
	cpp_finalize(&cpp_ctx);
	
	// Parse
	atoms_init();
	top_level = parse_file(&s);
	
	// Generate IR
	// TODO(NeGate): Globals amirite... yea maybe i'll maybe move the TB_Module from the
	// globals but at the same time, it's a thread safe interface for the most part you're
	// supposed to keep a global one and use it across multiple threads.
	if (!is_multithreaded) {
		TB_FeatureSet features = { 0 };
		mod = tb_module_create(arch, sys, &features);
		
		// Forward decls
		size_t func_count = arrlen(top_level.arr);
		for (size_t i = 0; i < func_count; i++) {
			sema_check(top_level, i);
		}
		
		// IR generation
		for (size_t i = 0; i < func_count; i++) {
			gen_ir(top_level, i);
		}
	} else {
		is_running = true;
		cnd_init(&tasks_condition);
		mtx_init(&tasks_mutex, mtx_plain);
		for (int i = 0; i < settings.num_of_worker_threads; i++) {
			thrd_create(&threads[i], task_thread, NULL);
		}
		
		TB_FeatureSet features = { 0 };
		mod = tb_module_create(arch, sys, &features);
		
		// Forward decls
		frontend_stage = 0;
		dispatch_tasks(arrlen(top_level.arr));
		
		if (sema_error_count) {
			printf("compiled with %d errors\n", sema_error_count);
			abort();
		}
		
		// Generate bodies
		frontend_stage = 1;
		dispatch_tasks(arrlen(top_level.arr));
		
		arrfree(s.tokens);
		arrfree(top_level.arr);
		
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
	
	cpp_deinit(&cpp_ctx);
	arena_free();
	atoms_deinit();
	
	// Compile
	if (!tb_module_compile(mod)) abort();
}

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

// TODO(NeGate): Make some command line options for these
static TB_Arch target_arch = TB_ARCH_X86_64;

#ifdef _WIN32
static TB_System target_sys = TB_SYSTEM_WINDOWS;
#else
static TB_System target_sys = TB_SYSTEM_LINUX;
#endif

static bool live_compile(const char source_file[], const char obj_output_path[], const char filename[]);
static bool dump_tokens(const char source_file[]);

int main(int argc, char* argv[]) {
#if 0
	// Use this code to generate the 10 million line test
	FILE* file = fopen("tests/test5.c", "wb");
	
	fprintf(file, "void print(int n){}\n");
	
	for (int i = 1; i <= 1000000; i++) {
		fprintf(file, "void fibonacci%d() {\n"
				"\tint lo = 0;\n"
				"\tint hi = 1;\n"
				"\twhile (hi < 10000) {\n"
				"\t\tint tmp = hi;\n"
				"\t\thi = hi + lo;\n"
				"\t\tlo = tmp;\n"
				"\t\tprint(lo);\n"
				"\t}\n"
				"}\n\n", i);
	}
	
	fprintf(file, "\nint main() {\n\treturn 0;\n}\n\n");
	fclose(file);
#else
	static char filename[256];
	static char obj_output_path[260];
	
	if (argc == 1) {
		printf("Expected command!\n");
		print_help(argv[0]);
		return -1;
	}
	
	// parse command
	CompilerMode mode = COMPILER_MODE_NONE;
	
	const char* cmd = argv[1];
	if (strcmp(cmd, "live") == 0) mode = COMPILER_MODE_LIVE;
	else if (strcmp(cmd, "version") == 0) mode = COMPILER_MODE_VERSION;
	else if (strcmp(cmd, "preproc") == 0) mode = COMPILER_MODE_PREPROC;
	else if (strcmp(cmd, "check") == 0) mode = COMPILER_MODE_CHECK;
	else if (strcmp(cmd, "build") == 0) mode = COMPILER_MODE_BUILD;
	else if (strcmp(cmd, "run") == 0) mode = COMPILER_MODE_RUN;
	else {
		printf("Unknown command: %s\n", cmd);
		print_help(argv[0]);
		return -1;
	}
	
#ifdef _WIN32
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	
	// Just kinda guesses something that seems ok ish for now
	// eventually we'll wanna use all cores but it's not honestly
	// helpful currently since code gen is the only parallel stage.
	settings.num_of_worker_threads = sysinfo.dwNumberOfProcessors - 4;
	if (settings.num_of_worker_threads <= 0) settings.num_of_worker_threads = 1;
#else
	settings.num_of_worker_threads = 1;
#endif
	
	printf("Starting with %d threads\n", settings.num_of_worker_threads);
	
	int i = 2;
	while (i < argc) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
				case 'c': 
				settings.is_object_only = true;
				break;
				
				case 'O':
				settings.optimization_level = TB_OPT_O1;
				break;
				
				case 'p':
				settings.print_tb_ir = true;
				break;
				
				case 'P':
				settings.pedantic = true;
				break;
				
				case 'o':
				i++;
				if (i >= argc) panic("Expected filename\n");
				settings.output_path = argv[i];
				break;
				
				default:
				printf("Unknown option: %s\n", argv[i]);
				print_help(argv[0]);
				return -1;
			}
		} else break;
		
		i++;
	}
	
	if (mode == COMPILER_MODE_VERSION) {
		printf("%s\n", COMPILER_VERSION);
		return 0;
	}
	
	// all options (well except version) require a file path right after
	if (i >= argc) {
		printf("Expected filename\n");
		return -1;
	}
	
	switch (mode) {
		case COMPILER_MODE_PREPROC: {
			if (argc < 2) panic("Expected filename\n");
			
			const char* source_file = argv[i];
			dump_tokens(source_file);
			break;
		}
		case COMPILER_MODE_CHECK:
		case COMPILER_MODE_BUILD:
		case COMPILER_MODE_RUN: {
			// Get filename without extension
			const char* source_file = argv[i];
			const char* ext = strrchr(source_file, '.');
			if (!ext) ext = source_file + strlen(source_file);
			
			memcpy_s(filename, 260, source_file, ext - source_file);
			filename[ext - source_file] = '\0';
			
#ifdef _WIN32
			sprintf_s(obj_output_path, 260, "%s.obj", filename);
#else
			sprintf_s(obj_output_path, 260, "%s.o", filename);
#endif
			
			// Build project
			timed_block("compilation") {
				compile_project(target_arch, target_sys, source_file, true);
				
				if (mode != COMPILER_MODE_CHECK) {
					if (!tb_module_export(mod, obj_output_path)) abort();
				}
				
				tb_module_destroy(mod);
			}
			
			if (!settings.is_object_only && mode != COMPILER_MODE_CHECK) {
				Linker l;
				if (linker_init(&l)) {
					// Add system libraries
					linker_add_default_libpaths(&l);
					
					// Add input files
					linker_add_input_file(&l, "kernel32.lib");
					linker_add_input_file(&l, "user32.lib");
					linker_add_input_file(&l, "Gdi32.lib");
					linker_add_input_file(&l, obj_output_path);
					
					linker_invoke(&l, filename, true);
					linker_deinit(&l);
					
					if (mode == COMPILER_MODE_RUN) {
						static char exe_path[260];
						sprintf_s(exe_path, 260, "%s.exe", filename);
						
						// im dumb btw, just notifying you
						char* p = exe_path;
						for (; p; p++) {
							if (*p == '/') *p = '\\';
						}
						
						printf("\n\n\n");
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
				return -1;
			}
			
			// Get filename without extension
			const char* source_file = argv[i];
			const char* ext = strrchr(source_file, '.');
			if (!ext) ext = source_file + strlen(source_file);
			
			memcpy_s(filename, 260, source_file, ext - source_file);
			filename[ext - source_file] = '\0';
			
#ifdef _WIN32
			sprintf_s(obj_output_path, 260, "%s.obj", filename);
#else
			sprintf_s(obj_output_path, 260, "%s.o", filename);
#endif
			
			if (!live_compile(source_file, obj_output_path, filename)) {
				printf("Live compiler error!\n");
				return -1;
			}
			break;
		}
		default: {
			printf("Not ready yet sorry!\n");
			return -1;
		}
	}
	
	return 0;
}

////////////////////////////////
// Preprocessor Dump
////////////////////////////////
static bool dump_tokens(const char source_file[]) {
	clock_t t1 = clock();
	
	// Preprocess file
	CPP_Context cpp_ctx;
	cpp_init(&cpp_ctx);
	set_preprocessor_info(&cpp_ctx);
	
	TokenStream s = cpp_process(&cpp_ctx, source_file);
	
	cpp_finalize(&cpp_ctx);
	
	clock_t t2 = clock();
	double delta_ms = (t2 - t1) / (double)CLOCKS_PER_SEC;
	printf("preprocessor took %.03f seconds\n", delta_ms);
	
	FILE* f = fopen("./preprocessed.c", "w");
	if (!f) {
		printf("Could not open file a.txt\n");
		return false;
	}
	
	size_t token_count = arrlen(s.tokens);
	
	const unsigned char* last_file = NULL;
	int last_line = 0;
	
	for (size_t i = 0; i < token_count; i++) {
		Token* t = &s.tokens[i];
		SourceLoc* loc = &s.line_arena[t->location];
		
		if (last_file != loc->file) {
			char str[260];
			
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
#endif
}

////////////////////////////////
// Live compiler functionality
////////////////////////////////
#if _WIN32
static bool disassemble_object_file(const wchar_t vs_exe_path[], const char filename[]) {
	clock_t t1 = clock();
	
	static wchar_t cmd_line[260];
	swprintf(cmd_line, 260, L"%s\\dumpbin.exe /nologo /disasm:nobytes %S.obj", vs_exe_path, filename);
	
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

static bool live_compile(const char source_file[], const char obj_output_path[], const char filename[]) {
	MicrosoftCraziness_Find_Result vswhere = MicrosoftCraziness_find_visual_studio_and_windows_sdk();
    
	uint64_t original_last_write = get_last_write_time(source_file);
	while (true) {
		system("cls");
		
		// bootlegy exception handler...
		if (signal(SIGABRT, live_compiler_abort) == SIG_ERR) {
			printf("Failed to set signal handler.\n");
			return false;
		}
		
		if (setjmp(live_compiler_savepoint) == 0) {
			timed_block("compilation") {
				compile_project(target_arch, target_sys, source_file, true);
				
				if (!tb_module_export(mod, obj_output_path)) abort();
				tb_module_destroy(mod);
			}
			
			disassemble_object_file(vswhere.vs_exe_path, filename);
		}
		
		// Wait for the user to save again
		while (true) {
			uint64_t current_last_write = get_last_write_time(source_file);
			if (original_last_write != current_last_write) {
				original_last_write = current_last_write;
				
				// wait for it to finish writing before trying to compile
				int ticks = 0;
				while (GetFileAttributesA(source_file) == INVALID_FILE_ATTRIBUTES) {
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
static bool live_compile(const char source_file[], const char obj_output_path[], const char filename[]) {
	printf("No live compiler supported");
    return false;
}
#endif
