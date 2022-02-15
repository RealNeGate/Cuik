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
#include "targets/targets.h"

#if _WIN32
#include "back/microsoft_craziness.h"
#endif

// used for the weird stuff in the live compiler
#include <signal.h>
#include <setjmp.h>

#define CUIK_COMPILER_MAJOR 0
#define CUIK_COMPILER_MINOR 1

static thrd_t threads[TB_MAX_THREADS];

static atomic_size_t tasks_reserved;
static atomic_size_t tasks_complete;
static atomic_bool is_running;
static size_t tasks_count;

// this is how many IR gen tasks it tries to grab at any one time
static size_t munch_size;

// signalled when it's about to finish to notify the main
// thread to wake up because it's about to finish with the
// parallel dispatch
static cnd_t tasks_condition;
static mtx_t tasks_mutex;

static TranslationUnit translation_unit;

CompilerSettings settings;

TB_Arch target_arch;
TB_System target_system;
TargetDescriptor target_desc;

// crash_handler.c
void hook_crash_handler();

static int task_thread(void* param) {
	while (is_running) {
		size_t munch = munch_size;
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

static void set_preprocessor_info(CPP_Context* cpp) {
	cpp_define(cpp, "__CUIKC__", STR(CUIK_COMPILER_MAJOR));
	cpp_define(cpp, "__CUIKC_MINOR__", STR(CUIK_COMPILER_MINOR));
	
	// NOTE(NeGate): Hack to make these trigger the preprocessor
	cpp_define_empty(cpp, "__FILE__");
	cpp_define_empty(cpp, "L__FILE__");
	cpp_define_empty(cpp, "__LINE__");
	
	cpp_define(cpp, "__STDC__", "1");
	cpp_define(cpp, "__STDC_VERSION__", "201112L");
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
	
	if (target_system == TB_SYSTEM_WINDOWS) {
		// TODO(NeGate): Automatically detect these somehow...
		cpp_add_include_directory(cpp, "W:\\Workspace\\Cuik\\crt\\include\\");
		cpp_add_include_directory(cpp, "W:\\Windows Kits\\10\\Include\\10.0.19041.0\\ucrt\\");
		cpp_add_include_directory(cpp, "W:\\Windows Kits\\10\\Include\\10.0.19041.0\\um\\");
		cpp_add_include_directory(cpp, "W:\\Windows Kits\\10\\Include\\10.0.19041.0\\shared\\");
		cpp_add_include_directory(cpp, "W:\\Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.29.30133\\include\\");
		
		cpp_define_empty(cpp, "_WIN32");
		cpp_define_empty(cpp, "_WIN64");
		cpp_define_empty(cpp, "_DEBUG");
		cpp_define_empty(cpp, "_MT");
		cpp_define_empty(cpp, "_CRT_NONSTDC_NO_WARNINGS");
		cpp_define_empty(cpp, "_CRT_SECURE_NO_WARNINGS");
		
		// things we don't handle yet so we just remove them
		cpp_define_empty(cpp, "_Frees_ptr_");
		cpp_define_empty(cpp, "__unaligned");
		cpp_define_empty(cpp, "__analysis_noreturn");
		cpp_define_empty(cpp, "__ptr32");
		cpp_define_empty(cpp, "__ptr64");
		
		// we pretend to be a modern MSVC compiler
		cpp_define(cpp, "_MSC_VER", "1929");
		cpp_define(cpp, "_MSC_FULL_VER", "192930133");
		cpp_define(cpp, "_WIN32_WINNT", "0x0A00");
		cpp_define(cpp, "NTDDI_VERSION", "0x0A000008");
		
		cpp_define(cpp, "__int8", "char");
		cpp_define(cpp, "__int16", "short");
		cpp_define(cpp, "__int32", "int");
		cpp_define(cpp, "__int64", "long long");
		cpp_define(cpp, "__pragma(x)", "_Pragma(#x)");
		cpp_define(cpp, "__inline", "inline");
		cpp_define(cpp, "__forceinline", "inline");
		cpp_define(cpp, "__CRTDECL", "__cdecl");
		
		// NOTE(NeGate): We probably shouldn't be defining this...
		// it's a winnt.h thing
		cpp_define(cpp, "ANYSIZE_ARRAY", "1");
	} else {
		// TODO(NeGate): Automatically detect these somehow...
		cpp_add_include_directory(cpp, "/usr/lib/gcc/x86_64-linux-gnu/10/include/");
		cpp_add_include_directory(cpp, "/usr/local/include/");
		cpp_add_include_directory(cpp, "/usr/include/");
	}
	
	target_desc.set_defines(cpp);
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
	
	translation_unit = parse_file(&s);
	crash_if_reports(REPORT_ERROR);
	
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(arch, sys, &features);
	irgen_init();
	
	// Semantics pass
	sema_remove_unused(&translation_unit);
	
	for (size_t i = 0, count = arrlen(translation_unit.top_level_stmts); i < count; i++) {
		sema_check(&translation_unit, translation_unit.top_level_stmts[i]);
	}
	crash_if_reports(REPORT_ERROR);
	
	// Generate IR
	// TODO(NeGate): Globals amirite... yea maybe i'll maybe move the TB_Module from the
	// globals but at the same time, it's a thread safe interface for the most part you're
	// supposed to keep a global one and use it across multiple threads.
	if (!is_multithreaded) {
		TB_FeatureSet features = { 0 };
		mod = tb_module_create(arch, sys, &features);
		
		// IR generation
		for (size_t i = 0, count = arrlen(translation_unit.top_level_stmts); i < count; i++) {
			irgen_top_level_stmt(&translation_unit, translation_unit.top_level_stmts[i]);
		}
	} else {
		is_running = true;
		cnd_init(&tasks_condition);
		mtx_init(&tasks_mutex, mtx_plain);
		
		munch_size = arrlen(translation_unit.top_level_stmts) / settings.num_of_worker_threads;
		if (munch_size < 5) munch_size = 5;
		
		for (int i = 0; i < settings.num_of_worker_threads; i++) {
			thrd_create(&threads[i], task_thread, NULL);
		}
		
		// Generate IR
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
	
	irgen_deinit();
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

static void print_version(const char* install_dir) {
	printf("cuik version %d.%d\n", CUIK_COMPILER_MAJOR, CUIK_COMPILER_MINOR);
	printf("install directory: %s\n", install_dir);
}

int main(int argc, char* argv[]) {
	hook_crash_handler();
	
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
		return -1;
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
	
	target_sys = TB_SYSTEM_WINDOWS;
#else
	settings.num_of_worker_threads = 1;
	target_sys = TB_SYSTEM_LINUX;
#endif
	
	// Defaults to the host arch as the target
#if defined(_AMD64_) || defined(__amd64__)
	target_arch = TB_ARCH_X86_64;
#else
#error "Unsupported host compiler... for now"
#endif
	
	// input path
	// TODO(NeGate): support inputting a directory in which
	// case it'll just compile all the .c files in it together
	if (argc == 2) {
		printf("Expected filename\n");
		return -1;
	}
	
	const char* source_file = argv[2];
	
	for (size_t i = 3; i < argc; i++) {
		if (argv[i][0] != '-') {
			printf("Invalid flag: %s\n", argv[i]);
			return -1;
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
				return -1;
			}
		} else if (strcmp(key, "sys") == 0) {
			if (strcmp(value, "windows") == 0) {
				target_sys = TB_SYSTEM_WINDOWS;
			} else if (strcmp(value, "linux") == 0) {
				target_sys = TB_SYSTEM_LINUX;
			} else {
				printf("unsupported architecture: %s\n", value);
				printf("Supported archs:\n");
				printf("\t-sys=windows\n");
				printf("\t-sys=linux\n");
				printf("\n");
				return -1;
			}
		} else if (strcmp(key, "threads") == 0) {
			int num;
			int matches = sscanf(value, "%d", &num);
			if (matches != 1) {
				printf("expected integer for thread count\n");
				return -1;
			}
			
			if (num < 1 || num > TB_MAX_THREADS) {
				printf("expected thread count between 1-%d\n", TB_MAX_THREADS);
				return -1;
			}
			
			settings.num_of_worker_threads = num;
		} else if (strcmp(key, "emit-ir") == 0) {
			settings.print_tb_ir = true;
		} else if (strcmp(key, "opt") == 0) {
			settings.optimization_level = TB_OPT_O1;
		} else if (strcmp(key, "obj") == 0) {
			settings.is_object_only = true;
		} else if (strcmp(key, "debug") == 0) {
			settings.debug_info = true;
		} else if (strcmp(key, "thin-errors") == 0) {
			report_using_thin_errors = true;
		} else if (strcmp(key, "out") == 0) {
			if (*value == '\0') {
				printf("expected path after -out option\n");
				return -1;
			}
			
			settings.output_path = value;
		} else if (strcmp(key, "pedantic") == 0) {
			settings.pedantic = true;
		} else {
			printf("unsupported option: %s\n", key);
			print_help(argv[0]);
			return -1;
		}
	}
	
	switch (target_arch) {
		case TB_ARCH_X86_64: 
		target_desc = get_x64_target_descriptor();
		break;
		
		default:
		printf("Cannot compile to your target machine");
		return -1;
	}
	
	switch (mode) {
		case COMPILER_MODE_PREPROC: {
			dump_tokens(source_file);
			break;
		}
		case COMPILER_MODE_CHECK:
		case COMPILER_MODE_BUILD:
		case COMPILER_MODE_RUN: {
			printf("Starting with %d threads\n", settings.num_of_worker_threads);
			
			// Get filename without extension
			const char* ext = strrchr(source_file, '.');
			if (!ext) ext = source_file + strlen(source_file);
			
			memcpy_s(filename, 260, source_file, ext - source_file);
			filename[ext - source_file] = '\0';
			
			if (target_sys == TB_SYSTEM_WINDOWS) {
				sprintf_s(obj_output_path, 260, "%s.obj", filename);
			} else if (target_sys == TB_SYSTEM_LINUX) {
				sprintf_s(obj_output_path, 260, "%s.o", filename);
			} else if (mode != COMPILER_MODE_CHECK) {
				printf("Unsupported object file\n");
				abort();
			}
			
			if (settings.print_tb_ir) {
				// Get filename as *.tbir
				char tbir_filename[260];
				sprintf_s(tbir_filename, 260, "%s.tbir", filename);
				
				tbir_output_file = fopen(tbir_filename, "wb");
			}
			
			// Build project
			timed_block("compilation") {
				compile_project(target_arch, target_sys, source_file, true);
				
				if (settings.print_tb_ir) {
					fclose(tbir_output_file);
				} else if (mode != COMPILER_MODE_CHECK) {
					if (!tb_module_export(mod, obj_output_path, settings.debug_info)) abort();
				}
				
				tb_module_destroy(mod);
			}
			
			if (!settings.is_object_only && mode != COMPILER_MODE_CHECK && !settings.print_tb_ir) {
				Linker l;
				if (linker_init(&l)) {
					// Add system libraries
					linker_add_default_libpaths(&l);
					
					// Add input files
					linker_add_input_file(&l, "kernel32.lib");
					linker_add_input_file(&l, "user32.lib");
					linker_add_input_file(&l, "Gdi32.lib");
					linker_add_input_file(&l, "Onecore.lib");
					linker_add_input_file(&l, "Onecoreuap.lib");
					linker_add_input_file(&l, "opengl32.lib");
					linker_add_input_file(&l, obj_output_path);
					
					linker_invoke(&l, filename, settings.using_winmain ? SUBSYSTEM_WINDOWS : SUBSYSTEM_CONSOLE, true);
					linker_deinit(&l);
					
					if (mode == COMPILER_MODE_RUN) {
						static char exe_path[260];
						sprintf_s(exe_path, 260, "%s.exe", filename);
						
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
				return -1;
			}
			
			// Get filename without extension
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
	// Get filename as *.i
	const char* ext = strrchr(source_file, '.');
	char output_path[260];
	if (ext) {
		char filename[260];
		memcpy_s(filename, 260, source_file, ext - source_file);
		filename[ext - source_file] = '\0';
		
		sprintf_s(output_path, 260, "%s.i", filename);
	} else {
		sprintf_s(output_path, 260, "%s.i", source_file);
	}
	
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
	
	FILE* f = fopen(output_path, "w");
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
				
				if (!tb_module_export(mod, obj_output_path, false)) abort();
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
