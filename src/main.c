// TODO(NeGate): This code would love some eyeballs but like those with skills
// in fixing it not judging it because it's self-aware and would rather people
// not hurt it's poor shit-box code feelings.
#include "microsoft_craziness.h"
#include "preproc.h"
#include "parser.h"
#include "ir_gen.h"
#include "atoms.h"
#include "timer.h"
#include "stb_ds.h"
#include <stdatomic.h>
#include "../ext/threads.h"

// the cuik metaprogramming library stuff
#include "../std/include/cuik/meta.h"

// used for the weird stuff in the live compiler
#include <signal.h>
#include <setjmp.h>

#define COMPILER_VERSION ""

// frontend worker threads
#define NUM_THREADS 6

// this is how many IR gen tasks it tries to grab at any one time
#define MAX_MUNCH 256

static thrd_t threads[NUM_THREADS];

static _Atomic size_t tasks_reserved;
static _Atomic size_t tasks_complete;
static _Atomic bool is_running;
static size_t tasks_count;

// Frontend IR Gen has two stages, one places all
// forward decls (0) and one places all function bodies (1)
static int frontend_stage;
static TopLevel top_level;

#if _WIN32
static MicrosoftCraziness_Find_Result vswhere;
#endif

static int task_thread(void* param) {
	while (is_running) {
		size_t t = atomic_fetch_add(&tasks_reserved, MAX_MUNCH);
		
		if (t+(MAX_MUNCH-1) >= tasks_count) {
			if (t < tasks_count) {
				for (size_t i = t; i < tasks_count; i++) {
					if (frontend_stage == 0) gen_ir_stage1(top_level, i);
					else gen_ir_stage2(top_level, i);
				}
				tasks_complete += (tasks_count - t);
			}
			
			while (tasks_reserved >= tasks_count) { 
				thrd_yield();
				if (!is_running) return 0;
			}
			
			continue;
		}
		
		tasks_complete += MAX_MUNCH;
	}
	
	return 0;
}

static void dispatch_tasks(size_t count) {
	tasks_count = count;
	tasks_complete = 0;
	tasks_reserved = 0;
	
	// wait until it's completed
    while (tasks_complete < tasks_count) { 
		thrd_yield();
	}
}

static void print_help(const char* executable_path) {
	printf("Usage:\n"
		   "  %s command [arguments]\n"
		   "  \n"
		   "Commands:\n"
		   "  build      Compiles the input file/directory\n"
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
	// TODO(NeGate): Automatically detect these somehow...
	cpp_add_include_directory(cpp_ctx, "W:\\Windows Kits\\10\\Include\\10.0.19041.0\\ucrt\\");
	cpp_add_include_directory(cpp_ctx, "W:\\Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.29.30133\\include\\");
	
	cpp_define_empty(cpp_ctx, "_X86_");
	cpp_define_empty(cpp_ctx, "_WIN32");
	cpp_define_empty(cpp_ctx, "_WIN64");
	cpp_define_empty(cpp_ctx, "_M_X64");
	cpp_define_empty(cpp_ctx, "_M_AMD64");
	cpp_define_empty(cpp_ctx, "_CRT_SECURE_NO_WARNINGS");
	
	cpp_define(cpp_ctx, "__int64", "long long");
}

static void compile_project(TB_Arch arch, TB_System sys, const char source_file[], bool is_multithreaded) {
	// Preprocess file
	CPP_Context cpp_ctx;
	cpp_init(&cpp_ctx);
	set_preprocessor_info(&cpp_ctx);
	
	TokenStream s = cpp_process(&cpp_ctx, source_file);
	cpp_finalize(&cpp_ctx);
	
	// Parse
	atoms_init();
	top_level = parse_file(&s);
	
	// Generate IR
	// TODO(NeGate): Globals amirite... yea maybe i'll maybe move the TB_Module from the
	// globals but at the same time, it's a thread safe interface for the most part you're
	// supposed to keep a global one and use it across multiple threads.
	if (!is_multithreaded) {
		// NOTE(NeGate): The metaprogram always compiles on one thread for simplicity
		TB_FeatureSet features = { 0 };
		mod = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features, TB_OPT_O0, 1, false);
		
		// Forward decls
		size_t func_count = arrlen(top_level.arr);
		for (size_t i = 0; i < func_count; i++) {
			gen_ir_stage1(top_level, i);
		}
		
		// IR generation
		for (size_t i = 0; i < func_count; i++) {
			gen_ir_stage2(top_level, i);
		}
	} else {
		is_running = true;
		for (int i = 0; i < NUM_THREADS; i++) {
			thrd_create(&threads[i], task_thread, NULL);
		}
		
		TB_FeatureSet features = { 0 };
		mod = tb_module_create(arch, sys, &features, TB_OPT_O0, NUM_THREADS, false);
		
		// Forward decls
		frontend_stage = 0;
		dispatch_tasks(arrlen(top_level.arr));
		
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
		for (int i = 0; i < NUM_THREADS; i++) {
			thrd_join(threads[i], NULL);
		}
	}
	
	cpp_deinit(&cpp_ctx);
	atoms_deinit();
	
	// Compile
	if (!tb_module_compile(mod)) abort();
}

#if _WIN32
// NOTE(NeGate): Windows still a bih, im forcing the 
// W functions because im a bitch too
// TODO(NeGate): Clean up this code and make it so that more
// options are exposed in the frontend.
static void link_object_file(const char filename[]) {
	clock_t t1 = clock();
	
	const char* libraries = "kernel32.lib user32.lib Gdi32.lib";
	
	static wchar_t working_dir[260];
	DWORD working_dir_len = GetCurrentDirectoryW(260, working_dir);
	for (size_t i = 0; i < working_dir_len; i++) {
		if (working_dir[i] == '\\') working_dir[i] = '/';
	}
	working_dir[working_dir_len++] = '/';
	working_dir[working_dir_len] = '\0';
	
	static wchar_t output_file_no_ext[260];
	swprintf(output_file_no_ext, 260, L"%s%S", working_dir, filename);
	
	static wchar_t cmd_line[1024];
	swprintf(cmd_line, 1024,
			 L"%s\\link.exe /nologo /machine:amd64 /subsystem:console"
			 " /debug:full /entry:mainCRTStartup /pdb:%s.pdb /out:%s.exe /libpath:\"%s\""
			 " /libpath:\"%s\" /libpath:\"%s\" /defaultlib:libcmt %S %s.obj",
			 vswhere.vs_exe_path, output_file_no_ext, output_file_no_ext,
			 vswhere.vs_library_path, vswhere.windows_sdk_ucrt_library_path, vswhere.windows_sdk_um_library_path,
			 libraries, output_file_no_ext);
	
	STARTUPINFOW si = {
		.cb = sizeof(STARTUPINFOW),
		.dwFlags = STARTF_USESTDHANDLES,
		.hStdInput = GetStdHandle(STD_INPUT_HANDLE),
		.hStdError = GetStdHandle(STD_ERROR_HANDLE),
		.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE)
	};
	PROCESS_INFORMATION pi = {};
	
	//printf("Linker command:\n%S %S\n", exe_path, cmd_line);
	if (!CreateProcessW(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, working_dir, &si, &pi)) {
		panic("Linker command could not be executed.");
	}
	
	// Wait until child process exits.
	WaitForSingleObject(pi.hProcess, INFINITE);
	
	// Close process and thread handles. 
	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);
	
	clock_t t2 = clock();
	double delta_ms = (t2 - t1) / (double)CLOCKS_PER_SEC;
	printf("linking took %.03f seconds\n", delta_ms);
}

static bool disassemble_object_file(const char filename[]) {
	clock_t t1 = clock();
	
	static wchar_t cmd_line[260];
	swprintf(cmd_line, 260, L"%s\\dumpbin.exe /nologo /disasm:nobytes %S.obj", vswhere.vs_exe_path, filename);
	
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
	FindFirstFile(filepath, &data);
	
	ULARGE_INTEGER i;
	i.LowPart = data.ftLastWriteTime.dwLowDateTime;
	i.HighPart = data.ftLastWriteTime.dwHighDateTime;
	return i.QuadPart;
}
#else
#error "Implement linker & disassembler for this platform"
#endif

static void dump_tokens(const char source_file[]) {
	// Preprocess file
	CPP_Context cpp_ctx;
	cpp_init(&cpp_ctx);
	set_preprocessor_info(&cpp_ctx);
	
	TokenStream s = cpp_process(&cpp_ctx, source_file);
	cpp_finalize(&cpp_ctx);
	
	FILE* f = fopen("./a.txt", "w");
	
	size_t token_count = arrlen(s.tokens);
	size_t last_line = 0;
	for (size_t i = 0; i < token_count; i++) {
		Token* t = &s.tokens[i];
		if (last_line != t->line) {
			fprintf(f, "\n%3d:\t", t->line);
			last_line = t->line;
		}
		
		fprintf(f, "%.*s ", (int)(t->end - t->start), t->start);
	}
	
	fclose(f);
}

// NOTE(NeGate): This is lowkey a mess but essentially if
// the live-compiler crashes we wanna just ignore it and continue
static jmp_buf live_compiler_savepoint;
static void live_compiler_abort(int signo) {
	longjmp(live_compiler_savepoint, 1);
}

static TB_Arch get_host_architecture() {
#if defined(__x86_64__) || defined(_M_X64)
	return TB_ARCH_X86_64;
#elif defined(__aarch64__) || defined(_M_ARM64)
	return TB_ARCH_AARCH64;
#else
	abort();
#endif
}

static TB_System get_host_system() {
#if defined(_WIN32)
	return TB_SYSTEM_WINDOWS;
#elif defined(__MACH__)
	return TB_SYSTEM_MACOS;
#elif defined(unix) || defined(__unix__) || defined(__unix)
	return TB_SYSTEM_LINUX;
#else
	abort();
#endif
}

int main(int argc, char* argv[]) {
	static char filename[260];
	static char obj_output_path[260];
	
	if (argc == 1) {
		printf("Expected command!\n");
		print_help(argv[0]);
		return -1;
	}
	
#if _WIN32
	vswhere = MicrosoftCraziness_find_visual_studio_and_windows_sdk();
#endif
	
	// Detect host hardware
	TB_Arch host_arch = get_host_architecture();
	TB_System host_sys = get_host_system();
	
	// TODO(NeGate): Make some command line options for these
	TB_Arch target_arch = TB_ARCH_X86_64;
	TB_System target_sys = TB_SYSTEM_WINDOWS;
	
	char* cmd = argv[1];
	if (strcmp(cmd, "run") == 0) {
		panic("Not ready yet sorry!\n");
	} else if (strcmp(cmd, "preproc") == 0) {
		if (argc < 2) panic("Expected filename\n");
		
		const char* source_file = argv[2];
		dump_tokens(source_file);
	} else if (strcmp(cmd, "build") == 0) {
		if (argc < 2) panic("Expected filename\n");
		
		// Get filename without extension
		const char* source_file = argv[2];
		const char* ext = strrchr(source_file, '.');
		if (!ext) ext = source_file + strlen(source_file);
		
		memcpy_s(filename, 260, source_file, ext - source_file);
		filename[ext - source_file] = '\0';
		
		sprintf_s(obj_output_path, 260, "%s.obj", filename);
		
		// Parse any other options
		CuikMetaEntrypoint* meta_entry = NULL;
		
		int i = 3;
		while (i < argc) {
			if (strcmp(argv[i], "-M") == 0) {
				i++;
				if (i >= argc) panic("Missing option");
				if (meta_entry) panic("You can only specify one metaprogram.");
				
				// JIT compile the metaprogram
				timed_block("meta compilation") {
					compile_project(host_arch, host_sys, argv[i], false);
					
					tb_module_export_jit(mod);
				}
				
				meta_entry = tb_module_get_jit_func_by_name(mod, "cuik_metaprogram");
				if (meta_entry == NULL) {
					panic("Metaprogram missing entrypoint, expected: cuik_metaprogram()");
				}
				
				// TODO(NeGate): Link up the metaprogramming runtime support
				tb_jit_import(mod, "putchar", putchar);
			}
			
			i++;
		}
		
		// TODO(NeGate): Run metaprogram entrypoint which sets up the hooks
		// for the parser
		if (meta_entry) meta_entry(NULL);
		
		// Build project
		timed_block("compilation") {
			compile_project(target_arch, target_sys, source_file, true);
			
			FILE* f = fopen(obj_output_path, "wb");
			if (!tb_module_export(mod, f)) abort();
			fclose(f);
			
			tb_module_destroy(mod);
		}
		
		link_object_file(filename);
	} else if (strcmp(cmd, "live") == 0) {
		if (argc < 2) panic("Expected filename\n");
		
		// Get filename without extension
		const char* source_file = argv[2];
		const char* ext = strrchr(source_file, '.');
		if (!ext) ext = source_file + strlen(source_file);
		
		memcpy_s(filename, 260, source_file, ext - source_file);
		filename[ext - source_file] = '\0';
		
		sprintf_s(obj_output_path, 260, "%s.obj", filename);
		
		// Build project
#if _WIN32
		if (signal(SIGABRT, live_compiler_abort) == SIG_ERR) {
			panic("Failed to set signal handler.\n");
			return -1;
		}
		
		uint64_t original_last_write = get_last_write_time(source_file);
		while (true) {
			system("cls");
			
			// bootlegy exception handler...
			if (setjmp(live_compiler_savepoint) == 0) {
				timed_block("compilation") {
					compile_project(target_arch, target_sys, source_file, true);
					
					FILE* f = fopen(obj_output_path, "wb");
					if (!tb_module_export(mod, f)) abort();
					fclose(f);
					
					tb_module_destroy(mod);
				}
				disassemble_object_file(filename);
			} else {
				// set exception handling again because it's unset once it's signalled... i think?
				if (signal(SIGABRT, live_compiler_abort) == SIG_ERR) {
					printf("Failed to set signal handler.\n");
					return -1;
				}
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
							abort();
						}
					}
					break;
				}
				
				SleepEx(100, FALSE);
			}
		}
#else
		printf("Live compiler not supported");
		return -1;
#endif
	} else if (strcmp(cmd, "bindgen") == 0) {
		printf("Not ready yet sorry!\n");
	} else if (strcmp(cmd, "help") == 0) {
		print_help(argv[0]);
	} else {
		printf("Unknown command: %s\n", cmd);
		print_help(argv[0]);
		return -1;
	}
	
#if _WIN32
	MicrosoftCraziness_free_resources(&vswhere);
#endif
	return 0;
}
