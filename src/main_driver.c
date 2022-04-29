// main_driver.c is the entrypoint to the compiler and should also
// act as a tutorial to writing a custom driver. Currently it doesn't 
// support multiple input files but that'll be next.
#include "driver_utils.h"
#include <ext/threadpool.h>

#if _WIN32
#define strdup(x) _strdup(x)
#endif

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
	COMPILER_MODE_ANAL,
	
	// standard compilation to executable (unless -obj is used)
	COMPILER_MODE_BUILD,
	
	// let's you access different details of the codebase
	COMPILER_MODE_QUERY,
	
	// compile & run
	COMPILER_MODE_RUN
} CompilerMode;

typedef struct {
	const char* name;
	
	TB_Arch arch;
	TB_System system;
} TargetOption;

static TargetOption target_options[] = {
	{ "x64_windows",  TB_ARCH_X86_64,   TB_SYSTEM_WINDOWS },
	{ "x64_macos",    TB_ARCH_X86_64,   TB_SYSTEM_MACOS   },
	{ "x64_linux",    TB_ARCH_X86_64,   TB_SYSTEM_LINUX   }
};
enum { TARGET_OPTION_COUNT = sizeof(target_options) / sizeof(target_options[0]) };

static BigArray(const char*) cuik_include_dirs;
static BigArray(const char*) cuik_source_files;
static char cuik_file_no_ext[255];
static bool is_frontend_only;

static CompilationUnit compilation_unit;
static threadpool_t* thread_pool;

static int parse_args(size_t arg_start, size_t arg_count, char** args);

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
		bool did_da_works = false;
		
		for (size_t i = task.start; i < task.end; i++) {
			TB_Function* func = tb_function_from_id(mod, i);
			
			if (tb_function_optimize(func)) did_da_works = true;
		}
		
		if (did_da_works) is_optimizer_working = true;
	}
}

static void codegen_task(void* arg) {
	TaskInfo task = *((TaskInfo*)arg);
	
	timed_block("codegen %zu-%zu", task.start, task.end) {
		for (size_t i = task.start; i < task.end; i++) {
			TB_Function* func = tb_function_from_id(mod, i);
			tb_module_compile_func(mod, func, TB_ISEL_COMPLEX);
			tb_function_free(func);
		}
	}
}

static void frontend_task(void* arg) {
	const char* path = (const char*)arg;
	
	cuik_compile_file(&compilation_unit, path,
					  big_array_length(cuik_include_dirs),
					  &cuik_include_dirs[0], is_frontend_only, thread_pool);
}

static void dispatch_for_all_top_level_stmts(void (*task)(void*)) {
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

static void dispatch_for_all_ir_functions(void (*task)(void*)) {
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
	is_frontend_only = is_check;
	
	init_report_system();
	compilation_unit_init(&compilation_unit);
	
	if (!is_check) irgen_init();
	thread_pool = threadpool_create(settings.num_of_worker_threads, 4096);
	
	// dispatch multithreaded
	if (settings.emit_ast == EMIT_AST_NONE) {
		for (size_t i = 0, count = big_array_length(cuik_source_files); i < count; i++) {
			threadpool_submit(thread_pool, frontend_task, (void*)cuik_source_files[i]);
		}
	} else {
		// emit-ast is single threaded just to make it nicer to read
		for (size_t i = 0, count = big_array_length(cuik_source_files); i < count; i++) {
			TranslationUnit* tu = cuik_compile_file(&compilation_unit, cuik_source_files[i],
													big_array_length(cuik_include_dirs),
													&cuik_include_dirs[0],
													is_check, NULL);
			
			ast_dump(tu, stdout);
		}
	}
	threadpool_wait(thread_pool);
	
	compilation_unit_internal_link(&compilation_unit);
	
	if (!is_check) {
		timed_block("ir gen & compile") {
			dispatch_for_all_top_level_stmts(irgen_task);
			
			FOR_EACH_TU(tu, &compilation_unit) {
				translation_unit_deinit(tu);
			}
			
			// on optimizations things get fancy and we run passes over all of the IR
			// until exhaustion with WPO passes in between
			if (settings.optimize) {
				do {
					is_optimizer_working = false;
					
					dispatch_for_all_ir_functions(optimize_task);
				} while (is_optimizer_working);
				
				dispatch_for_all_ir_functions(codegen_task);
			}
			
			irgen_deinit();
		}
	}
	
	threadpool_free(thread_pool);
	compilation_unit_deinit(&compilation_unit);
	arena_free(&thread_arena);
	
	// Compile
	if (!is_check) {
		if (!tb_module_compile(mod)) abort();
		
		timed_block("export") {
			if (!settings.print_tb_ir) {
				if (!tb_module_export(mod, obj_output_path, settings.is_debug_info)) abort();
			}
			
			tb_module_destroy(mod);
		}
	}
}

////////////////////////////////
// Cuik query
////////////////////////////////
static int execute_query_operation(const char* option, size_t arg_start, size_t arg_count, char** args) {
	if (option) {
		if (strcmp(option, "find_include") == 0) {
			int result = parse_args(arg_start, arg_count, args);
			if (result) return result;
			
			char output[MAX_PATH];
			
			CPP_Context cpp_ctx;
			cpp_init(&cpp_ctx);
			cuik_set_cpp_defines(&cpp_ctx);
			
			for (size_t i = 0, count = big_array_length(cuik_source_files); i < count; i++) {
				if (cpp_find_include_include(&cpp_ctx, output, cuik_source_files[i])) {
					printf("%s\n", output);
				} else {
					printf("NOTFOUND\n");
				}
			}
			
			cpp_finalize(&cpp_ctx);
			cpp_deinit(&cpp_ctx);
			
			return 0;
		} else if (strcmp(option, "print_type") == 0) {
			if (arg_count < 2) return -1;
			
			int result = parse_args(arg_start, arg_count - 1, args);
			if (result) return result;
			
			if (big_array_length(cuik_source_files) != 1) {
				printf("print_type expects after C source file then a typename\n");
				abort();
			}
			
			settings.hack_type_printer_name = args[arg_count - 1]; 
			
			atoms_init();
			init_report_system();
			compilation_unit_init(&compilation_unit);
			
			TranslationUnit* tu = cuik_compile_file(&compilation_unit, cuik_source_files[0],
													big_array_length(cuik_include_dirs),
													&cuik_include_dirs[0], true, thread_pool);
			
			if (tu->hack.type) {
				ast_dump_type(tu, tu->hack.type, 0, 0);
			} else {
				printf("Could not find type '%s'\n", tu->hack.name);
			}
			return 0;
		}
	}
	
	printf("Unknown cuik query option (Supported options):\n");
	printf("find_include - Resolve an include file path from name\n");
	printf("print_type   - Find a type within the translation unit\n");
	printf("\n");
	return 1;
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
	sprintf_s(obj_output_path, 260, "%s.obj", cuik_file_no_ext);
	
	uint64_t original_last_write = get_last_write_time(cuik_source_files[0]);
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
			uint64_t current_last_write = get_last_write_time(cuik_source_files[0]);
			
			if (original_last_write != current_last_write) {
				original_last_write = current_last_write;
				
				// wait for it to finish writing before trying to compile
				int ticks = 0;
				while (GetFileAttributesA(cuik_source_files[0]) == INVALID_FILE_ATTRIBUTES) {
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
	if (big_array_length(cuik_source_files) != 1) {
		printf("Standalone preprocessor cannot operate on more than one source file\n");
		abort();
	}
	
	// Preprocess file
	uint64_t t1 = timer_now();
	TokenStream s;
	{
		CPP_Context cpp_ctx;
		cpp_init(&cpp_ctx);
		cuik_set_cpp_defines(&cpp_ctx);
		
		for (size_t i = 0, cc = big_array_length(cuik_include_dirs); i < cc; i++) {
			cpp_add_include_directory(&cpp_ctx, cuik_include_dirs[i]);
		}
		
		s = cpp_process(&cpp_ctx, cuik_source_files[0]);
		
		cpp_finalize(&cpp_ctx);
	}
	uint64_t t2 = timer_now();
	
#ifdef _WIN32
	double elapsed = (t2 - t1) * timer_freq;
	printf("preprocessor took %.03f seconds\n", elapsed);
#else
	// TODO(NeGate): Windows does undefined frequency thingy,
	// the linux interface is microseconds
	double elapsed = (t2 - t1) / 1000000.0;
	printf("preprocessor took %.03f seconds\n", elapsed);
#endif
	
	char output_path[MAX_PATH];
	sprintf_s(output_path, MAX_PATH, "%s.i", cuik_file_no_ext);
	FILE* f = fopen(output_path, "w");
	if (!f) {
		printf("Could not open file a.txt\n");
		return false;
	}
	
	const unsigned char* last_file = NULL;
	int last_line = 0;
	int current_column = 0;
	
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
			current_column = 0;
		}
		
		if (last_line != loc->line) {
			fprintf(f, "\n/* line %3d */\t", loc->line);
			last_line = loc->line;
			current_column = 0;
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
	// TODO(NeGate): redocument this, it's wrong now...
	printf("Usage:\n"
		   "  %s [command] [filepath] [flags]\n"
		   "  \n"
		   "Commands:\n"
		   "  version    Prints the installed compiler's version\n"
		   "  live       Live recompilation of C into the terminal as you edit\n"
		   "  preproc    Runs the preprocessor on a file\n"
		   "  anal       Analyzes & Type checks the files.\n"
		   "  build      Compiles the input files\n"
		   "  query      Query C files or other properties\n"
		   "  run        Compiles & runs the input files\n"
		   "  help       More detailed help for the input command\n"
		   "  \n",
		   executable_path);
}

// we can do a bit of filter such as '*.c' where it'll take all 
// paths in the folder that end with .c
static void append_input_path(const char* path) {
	// avoid using the filters if we dont need to :p
	bool needs_filter = false;
	for (const char* p = path; *p; p++) if (*p == '*') {
		needs_filter = true;
		break;
	}
	
	if (needs_filter) {
#     ifdef _WIN32
		const char* slash = path;
		for (const char* p = path; *p; p++) if (*p == '/' || *p == '\\') {
			slash = p;
		}
		
		WIN32_FIND_DATA find_data;
		HANDLE find_handle = FindFirstFile(path, &find_data);
		if (find_handle == INVALID_HANDLE_VALUE) {
			printf("could not filter path: %s\n", path);
			abort();
		}
		
		do {
			char* new_path = malloc(MAX_PATH);
			if (slash == path) {
				sprintf_s(new_path, MAX_PATH, "%s", find_data.cFileName);
			} else {
				sprintf_s(new_path, MAX_PATH, "%.*s%s", (int)(slash - path) + 1, path, find_data.cFileName);
			}
			big_array_put(cuik_source_files, new_path);
		} while (FindNextFile(find_handle, &find_data));
		
		if (!FindClose(find_handle)) {
			printf("internal error: failed to close filter\n");
			abort();
		}
#     else
		printf("filepath filters not supported on your platform yet :(\n");
		printf("umm... i mean you can probably remind me if you want :)\n");
		abort();
#     endif
	} else {
		big_array_put(cuik_source_files, path);
	}
}

static int parse_args(size_t arg_start, size_t arg_count, char** args) {
	const char* output_name = NULL;
	
	for (size_t i = arg_start; i < arg_count; i++) {
		if (args[i][0] != '-') {
			append_input_path(args[i]);
			continue;
		}
		
		char* key = &args[i][1];
		char* value = key;
		
		// splits on :
		for (; *value; value++) {
			if (*value == ':') break;
			if (*value == '=') break;
		}
		
		// split the key and value
		if (*value) {
			if (*value == '=') {
				printf("compiler flags are applied as -key:value\n");
				return 1;
			}
			
			value[0] = '\0';
			value++;
		} else {
			value = NULL;
		}
		
		if (strcmp(key, "include") == 0) {
			if (!value) {
				printf("expected file path for include directory\n");
				return 1;
			}
			
			big_array_put(cuik_include_dirs, value);
		} else if (strcmp(key, "out") == 0 ||
				   strcmp(key, "output") == 0) {
			if (!value) {
				printf("expected file path for output\n");
				return 1;
			}
			
			output_name = value;
		} else if (strcmp(key, "target") == 0) {
			bool matches = false;
			for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
				if (strcmp(target_options[i].name, value) == 0) {
					target_arch = target_options[i].arch;
					target_system = target_options[i].system;
					matches = true;
					break;
				}
			}
			
			if (!matches) {
				printf("error: unsupported target: %s\n", value);
				printf("Supported targets:\n");
				for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
					printf("\t%s\n", target_options[i].name);
				}
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
			settings.emit_ast = EMIT_AST_NORMAL;
			
			if (value) {
				if (strcmp(value, "minimal") == 0) {
					settings.emit_ast = EMIT_AST_MINIMAL;
				} else if (strcmp(value, "normal") == 0) {
					settings.emit_ast = EMIT_AST_NORMAL;
				} else {
					printf("-emit-ast only supports options: 'normal', 'minimal' or nothing");
					return 1;
				}
			}
		} else if (strcmp(key, "emit-ir") == 0) {
			settings.print_tb_ir = true;
		} else if (strcmp(key, "find-include") == 0) {
			settings.find_include = true;
		} else if (strcmp(key, "opt") == 0) {
			settings.optimize = true;
		} else if (strcmp(key, "obj") == 0) {
			settings.is_object_only = true;
		} else if (strcmp(key, "time") == 0) {
			settings.is_time_report = true;
		} else if (strcmp(key, "freestanding") == 0) {
			settings.freestanding = true;
		} else if (strcmp(key, "debug") == 0) {
			settings.is_debug_build = true;
		} else if (strcmp(key, "debug-info") == 0) {
			settings.is_debug_info = true;
		} else if (strcmp(key, "thin-errors") == 0) {
			report_using_thin_errors = true;
		} else if (strcmp(key, "pedantic") == 0) {
			settings.pedantic = true;
		} else {
			printf("unsupported option: %s\n", key);
			print_help(args[0]);
			return 1;
		}
	}
	
	if (big_array_length(cuik_source_files) == 0) {
		printf("Expected input files\n");
		return 1;
	}
	
	// Get first filename without extension
	{
		const char* filename = output_name ? output_name : cuik_source_files[0];
		const char* ext = strrchr(filename, '.');
		size_t len = ext ? (ext - filename) : strlen(filename);
		
		memcpy(cuik_file_no_ext, filename, len);
		cuik_file_no_ext[len] = '\0';
	}
	
	switch (target_arch) {
		case TB_ARCH_X86_64: 
		target_desc = get_x64_target_descriptor();
		break;
		
		default:
		printf("Cannot compile to your target machine");
		return 1;
	}
	
	return 0;
}

static void print_version(const char* install_dir) {
	printf("cuik version %d.%d\n",         CUIK_COMPILER_MAJOR, CUIK_COMPILER_MINOR);
	printf("install directory: %s\n",      install_dir);
	printf("cuik include directory: %s\n", cuik_include_directory);
	
#ifdef _WIN32
	printf("windows sdk include: %S\n",    s_vswhere.windows_sdk_include);
	printf("visual studio include: %S\n",  s_vswhere.vs_include_path);
#endif
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
	else if (strcmp(cmd, "-v") == 0) mode = COMPILER_MODE_VERSION;
	else if (strcmp(cmd, "version") == 0) mode = COMPILER_MODE_VERSION;
	else if (strcmp(cmd, "preproc") == 0) mode = COMPILER_MODE_PREPROC;
	else if (strcmp(cmd, "anal") == 0) mode = COMPILER_MODE_ANAL;
	else if (strcmp(cmd, "build") == 0) mode = COMPILER_MODE_BUILD;
	else if (strcmp(cmd, "query") == 0) mode = COMPILER_MODE_QUERY;
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
	
	// I seriously dare you to tell me that im leaking these
	cuik_source_files = big_array_create(const char*, false);
	cuik_include_dirs = big_array_create(const char*, false);
	
	// Query is like a magic Swiss army knife inside of Cuik
	// so it acts differently from everyone else
	if (mode == COMPILER_MODE_QUERY) {
		if (argc < 3) {
			// should give us the help screen for query
			return execute_query_operation(NULL, 0, 0, NULL);
		} else {
			return execute_query_operation(argv[2], 3, argc, argv);
		}
	}
	
	int result = parse_args(2, argc, argv);
	if (result) return result;
	
	switch (mode) {
		case COMPILER_MODE_PREPROC: {
			timer_init();
			init_report_system();
			dump_tokens();
			break;
		}
		case COMPILER_MODE_ANAL:
		case COMPILER_MODE_BUILD:
		case COMPILER_MODE_RUN: {
			char obj_output_path[MAX_PATH];
			if (target_system == TB_SYSTEM_WINDOWS) {
				sprintf_s(obj_output_path, 260, "%s.obj", cuik_file_no_ext);
			} else if (target_system == TB_SYSTEM_LINUX) {
				sprintf_s(obj_output_path, 260, "%s.o", cuik_file_no_ext);
			} else if (mode != COMPILER_MODE_ANAL) {
				printf("Unsupported object file\n");
				abort();
			}
			
			timer_init();
			
			// Open profiler file stream
			if (settings.is_time_report) {
				char report_filename[MAX_PATH];
				sprintf_s(report_filename, 260, "%s.json", cuik_file_no_ext);
				
				timer_open(report_filename);
			}
			
			// Build project
			timed_block("total") {
				compile_project(obj_output_path, true, mode == COMPILER_MODE_ANAL);
				
				if (!settings.is_object_only && mode != COMPILER_MODE_ANAL && !settings.print_tb_ir) {
					if (settings.freestanding) {
						printf("error: cannot link and be freestanding... yet");
						return 1;
					}
					
					timed_block("linker") {
						Linker l;
						if (linker_init(&l)) {
							// Add system libpaths
							linker_add_default_libpaths(&l);
							linker_add_libpath(&l, "W:/Workspace/Cuik/crt/lib/");
							
							// Add Cuik output
							linker_add_input_file(&l, obj_output_path);
							
							// Add input libraries
#ifdef _WIN32
							linker_add_input_file(&l, "kernel32.lib");
							linker_add_input_file(&l, "user32.lib");
							linker_add_input_file(&l, "shell32.lib");
							linker_add_input_file(&l, "Gdi32.lib");
							//linker_add_input_file(&l, "Onecore.lib");
							//linker_add_input_file(&l, "Onecoreuap.lib");
							linker_add_input_file(&l, "opengl32.lib");
							linker_add_input_file(&l, "msvcrt.lib");
							linker_add_input_file(&l, "win32_rt.lib");
#endif
							
							linker_invoke_system(&l, cuik_file_no_ext);
							linker_deinit(&l);
							
							if (mode == COMPILER_MODE_RUN) {
								char exe_path[MAX_PATH];
								sprintf_s(exe_path, 260, "%s.exe", cuik_file_no_ext);
								
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
