// main_driver.c is the entrypoint to the compiler and should also
// act as a tutorial to writing a custom driver. Currently it doesn't 
// support multiple input files but that'll be next.
#include "driver_utils.h"
#include <ext/threadpool.h>

typedef enum {
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
	COMPILER_MODE_RUN
} CompilerMode;

static const char* cuik_source_file;
static char cuik_file_no_ext[MAX_PATH];

static CompilationUnit compilation_unit;

////////////////////////////////
// Standard compilation
////////////////////////////////
typedef struct {
	TranslationUnit* tu;
	size_t start, end;
} TaskInfo;

static atomic_bool is_optimizer_working = false;

static void irgen_task(void* arg) {
	TaskInfo task = *((TaskInfo*)arg);
	
	timed_block("irgen %zu-%zu", task.start, task.end) {
		for (size_t i = task.start; i < task.end; i++) {
			irgen_top_level_stmt(task.tu, task.tu->top_level_stmts[i]);
		}
	}
}

static void optimize_task(void* arg) {
	TaskInfo task = *((TaskInfo*)arg);
	
	timed_block("optimize %zu-%zu", task.start, task.end) {
		for (size_t i = task.start; i < task.end; i++) {
			TB_Function* func = tb_function_from_id(mod, i);
			
			if (tb_function_optimize(func)) is_optimizer_working = true;
		}
	}
}

static void codegen_task(void* arg) {
	TaskInfo task = *((TaskInfo*)arg);
	
	timed_block("codegen %zu-%zu", task.start, task.end) {
		for (size_t i = task.start; i < task.end; i++) {
			TB_Function* func = tb_function_from_id(mod, i);
			tb_module_compile_func(mod, func, TB_ISEL_FAST);
			tb_function_free(func);
		}
	}
}

static void dispatch_for_all_top_level_stmts(threadpool_t* thread_pool, void (*task)(void*)) {
	tls_init();
	
	FOR_EACH_TU(tu, &compilation_unit) {
		// split up the top level statement tasks into
		// chunks to avoid spawning too many tiny tasks
		size_t count = arrlen(tu->top_level_stmts);
		size_t padded = (count + 4095) & ~4095;
		
		for (size_t i = 0; i < padded; i += 4096) {
			size_t limit = i+4096;
			if (limit > count) limit = count;
			
			TaskInfo* t = tls_push(sizeof(TaskInfo));
			*t = (TaskInfo){ tu, i, limit };
			
			threadpool_submit(thread_pool, task, t);
		}
	}
	
	threadpool_wait(thread_pool);
}

static void dispatch_for_all_ir_functions(threadpool_t* thread_pool, void (*task)(void*)) {
	tls_init();
	
	// split up the top level statement tasks into
	// chunks to avoid spawning too many tiny tasks
	size_t count = function_count;
	size_t padded = (count + 4095) & ~4095;
	
	for (size_t i = 0; i < padded; i += 4096) {
		size_t limit = i+4096;
		if (limit > count) limit = count;
		
		TaskInfo* t = tls_push(sizeof(TaskInfo));
		*t = (TaskInfo){ NULL, i, limit };
		
		threadpool_submit(thread_pool, task, t);
	}
	
	threadpool_wait(thread_pool);
}

static void compile_project(const char* obj_output_path, bool is_multithreaded, bool is_check) {
	atoms_init();
	compilation_unit_init(&compilation_unit);
	
	irgen_init();
	
	TranslationUnit* tu = cuik_compile_file(&compilation_unit, cuik_source_file);
	if (settings.print_ast) {
		ast_dump(tu, stdout);
	}
	
	if (!is_check) {
		timed_block("ir gen & compile") {
			threadpool_t* thread_pool = threadpool_create(settings.num_of_worker_threads, 4096);
			
			dispatch_for_all_top_level_stmts(thread_pool, irgen_task);
			irgen_finalize();
			
			FOR_EACH_TU(tu, &compilation_unit) {
				translation_unit_deinit(tu);
			}
			
			// on optimizations things get fancy and we run passes over all of the IR
			// until exhaustion with WPO passes in between
			if (settings.optimize) {
				do {
					is_optimizer_working = false;
					
					dispatch_for_all_ir_functions(thread_pool, optimize_task);
				} while (is_optimizer_working);
				
				dispatch_for_all_ir_functions(thread_pool, codegen_task);
			}
			
			threadpool_free(thread_pool);
			irgen_deinit();
		}
	}
	
	compilation_unit_deinit(&compilation_unit);
	arena_free();
	atoms_deinit();
	
	// Compile
	if (!is_check) {
		if (!tb_module_compile(mod)) abort();
		
		timed_block("export") {
			if (!settings.print_tb_ir) {
				if (!tb_module_export(mod, obj_output_path, settings.debug_info)) abort();
			}
			
			tb_module_destroy(mod);
		}
	}
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
			compile_project(obj_output_path, true, false);
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
	// Preprocess file
	uint64_t t1 = timer_now();
	TokenStream s;
	{
		CPP_Context cpp_ctx;
		cpp_init(&cpp_ctx);
		cuik_set_cpp_defines(&cpp_ctx);
		
		s = cpp_process(&cpp_ctx, cuik_source_file);
		
		cpp_finalize(&cpp_ctx);
	}
	uint64_t t2 = timer_now();
	double elapsed = (t2 - t1) * timer_freq;
	printf("preprocessor took %.03f seconds\n", elapsed);
	
	char output_path[MAX_PATH];
	snprintf(output_path, 260, "%s.i", cuik_file_no_ext);
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
	
	cuik_detect_crt_include();
	
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
			settings.optimize = true;
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
			
			timer_init();
			
			// Open profiler file stream
			if (settings.is_time_report) {
				char report_filename[MAX_PATH];
				snprintf(report_filename, 260, "%s.json", cuik_file_no_ext);
				
				timer_open(report_filename);
			}
			
			// Build project
			timed_block("total") {
				compile_project(obj_output_path, true, mode == COMPILER_MODE_CHECK);
				
				if (!settings.is_object_only && mode != COMPILER_MODE_CHECK && !settings.print_tb_ir) {
					timed_block("linker") {
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
								
								return exit_code;
							}
						}
					}
				}
			}
			
			// Close out profiler output (it doesn't include the linking)
			timer_close();
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
