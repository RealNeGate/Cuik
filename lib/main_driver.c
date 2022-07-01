// main_driver.c is the entrypoint to the compiler and should also
// act as a tutorial to writing a custom driver. Currently it doesn't
// support multiple input files but that'll be next.
#include "driver_utils.h"
#include <ext/threadpool.h>

#if _WIN32
// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#define strtok_r(a, b, c) strtok_s(a, b, c)
#define strdup(x) _strdup(x)
#endif

#define IRGEN_TASK_MUNCH 16384

typedef struct {
    const char* name;

    TB_Arch arch;
    TB_System system;
} TargetOption;

typedef struct {
    const char* name;

    int version;
    bool static_link;

    int lib_count;
    const char** libs;
} LibcOption;

static LibcOption libc_options[] = {
    {"none",    00, true,  0},
    {"ucrt",    99, false, 3, (const char*[]){"ucrt.lib", "msvcrt.lib", "vcruntime.lib"}},
    {"libucrt", 99, true,  3, (const char*[]){"libucrt.lib", "libcmt.lib", "libvcruntime.lib"}},
};
enum { LIBC_OPTION_COUNT = sizeof(libc_options) / sizeof(libc_options[0]) };

static TargetOption target_options[] = {
    {"x64_windows", TB_ARCH_X86_64, TB_SYSTEM_WINDOWS},
    {"x64_macos",   TB_ARCH_X86_64, TB_SYSTEM_MACOS},
    {"x64_linux",   TB_ARCH_X86_64, TB_SYSTEM_LINUX},
};
enum { TARGET_OPTION_COUNT = sizeof(target_options) / sizeof(target_options[0]) };

Warnings warnings = {
    //.data_loss = true,
    .unused_funcs = true,
};

static BigArray(const char*) cuik_include_dirs;
static BigArray(const char*) cuik_source_files;
static BigArray(const char*) cuik_libraries;
static char cuik_file_no_ext[255];
static bool is_frontend_only;
static LibcOption* chosen_libc = NULL;

static CompilationUnit compilation_unit;

// this can be NULL which just means we operate singlethreaded
static threadpool_t* thread_pool;

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

static void dispatch_for_all_top_level_stmts(void task(void*)) {
    // split up the top level statement tasks into
    // chunks to avoid spawning too many tiny tasks
    if (thread_pool != NULL) {
        tls_init();

        FOR_EACH_TU(tu, &compilation_unit) {
            size_t count = arrlen(tu->top_level_stmts);
            size_t padded = (count + (IRGEN_TASK_MUNCH - 1)) & ~(IRGEN_TASK_MUNCH - 1);

            for (size_t i = 0; i < padded; i += IRGEN_TASK_MUNCH) {
                size_t limit = i + IRGEN_TASK_MUNCH;
                if (limit > count) limit = count;

                TaskInfo* t = tls_push(sizeof(TaskInfo));
                *t = (TaskInfo){tu, i, limit};

                threadpool_submit(thread_pool, task, t);
            }
        }

        timed_block("wait") {
            threadpool_work_while_wait(thread_pool);
        }
    } else {
        FOR_EACH_TU(tu, &compilation_unit) {
            size_t count = arrlen(tu->top_level_stmts);
            size_t padded = (count + (IRGEN_TASK_MUNCH - 1)) & ~(IRGEN_TASK_MUNCH - 1);

            for (size_t i = 0; i < padded; i += IRGEN_TASK_MUNCH) {
                size_t limit = i + IRGEN_TASK_MUNCH;
                if (limit > count) limit = count;

                task(&(TaskInfo){tu, i, limit});
            }
        }
    }
}

static void dispatch_for_all_ir_functions(void (*task)(void*)) {
    if (thread_pool != NULL) {
        tls_init();

        // split up the top level statement tasks into
        // chunks to avoid spawning too many tiny tasks
        size_t count = function_count;
        size_t padded = (count + (IRGEN_TASK_MUNCH - 1)) & ~(IRGEN_TASK_MUNCH - 1);

        for (size_t i = 0; i < padded; i += IRGEN_TASK_MUNCH) {
            size_t limit = i + IRGEN_TASK_MUNCH;
            if (limit > count) limit = count;

            TaskInfo* t = tls_push(sizeof(TaskInfo));
            *t = (TaskInfo){NULL, i, limit};

            threadpool_submit(thread_pool, task, t);
        }

        timed_block("wait") {
            threadpool_work_while_wait(thread_pool);
        }
    } else {
        // split up the top level statement tasks into
        // chunks to avoid spawning too many tiny tasks
        size_t count = function_count;
        size_t padded = (count + (IRGEN_TASK_MUNCH - 1)) & ~(IRGEN_TASK_MUNCH - 1);

        for (size_t i = 0; i < padded; i += IRGEN_TASK_MUNCH) {
            size_t limit = i + IRGEN_TASK_MUNCH;
            if (limit > count) limit = count;

            task(&(TaskInfo){NULL, i, limit});
        }
    }
}

static void compile_project(const char* obj_output_path, bool is_multithreaded) {
    bool runs_backend = (settings.stage_to_stop_at >= STAGE_IR);
    is_frontend_only = !runs_backend;

    init_report_system();
    compilation_unit_init(&compilation_unit);

    if (runs_backend) {
        irgen_init();
    }

    timed_block("initialize thread pool") {
        if (settings.num_of_worker_threads <= 1) {
            thread_pool = NULL;
        } else {
            thread_pool = threadpool_create(settings.num_of_worker_threads, 4096);
        }
    }

    if (thread_pool != NULL) {
        // dispatch multithreaded
        for (size_t i = 0, count = big_array_length(cuik_source_files); i < count; i++) {
            threadpool_submit(thread_pool, frontend_task, (void*)cuik_source_files[i]);
        }

        timed_block("wait") {
            threadpool_work_while_wait(thread_pool);
        }
    } else {
        // emit-ast is single threaded just to make it nicer to read
        for (size_t i = 0, count = big_array_length(cuik_source_files); i < count; i++) {
            cuik_compile_file(&compilation_unit, cuik_source_files[i],
                big_array_length(cuik_include_dirs),
                &cuik_include_dirs[0],
                !runs_backend, NULL);
        }
    }

    if (settings.stage_to_stop_at == STAGE_TYPES && settings.emit_partial_results) {
        FOR_EACH_TU(tu, &compilation_unit) {
            ast_dump(tu, stdout);
        }
        return;
    }

    CUIK_TIMED_BLOCK("internal link") {
        compilation_unit_internal_link(&compilation_unit);
    }

    if (runs_backend) {
        CUIK_TIMED_BLOCK("ir gen & compile") {
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

                if (settings.stage_to_stop_at >= STAGE_OBJ) {
                    dispatch_for_all_ir_functions(codegen_task);
                } else if (settings.emit_partial_results) {
                    // print optimized IR
                    size_t count = function_count;
                    for (size_t i = 0; i < count; i++) {
                        tb_function_print(tb_function_from_id(mod, i), tb_default_print_callback, stdout);
                        fprintf(stdout, "\n\n");
                        fflush(stdout);
                    }
                }
            }

            irgen_deinit();
        }
    }

    CUIK_TIMED_BLOCK("freeing compilation unit") {
        if (thread_pool != NULL) {
            threadpool_free(thread_pool);
        }

        compilation_unit_deinit(&compilation_unit);
        arena_free(&thread_arena);
    }

    // Compile
    if (settings.stage_to_stop_at >= STAGE_OBJ) {
        CUIK_TIMED_BLOCK("export") {
            if (!tb_module_export(mod, obj_output_path)) {
                fprintf(stderr, "error: tb_module_export failed!\n");
                abort();
            }

            tb_module_destroy(mod);
        }
    } else if (settings.stage_to_stop_at >= STAGE_IR) {
        // or don't lmao
        tb_module_destroy(mod);
    }

    tb_free_thread_resources();
}

////////////////////////////////
// Cuik query
////////////////////////////////
static int execute_query_operation(const char* option, size_t arg_start, size_t arg_count, char** args) {
    if (option) {
        if (strcmp(option, "find_include") == 0) {
            char output[MAX_PATH];

            CPP_Context cpp_ctx;
            cpp_init(&cpp_ctx);
            cuik_set_cpp_defines(&cpp_ctx);

            for (size_t i = arg_start; i < arg_count; i++) {
                if (cpp_find_include_include(&cpp_ctx, output, args[i])) {
                    printf("%s\n", output);
                } else {
                    printf("NOTFOUND\n");
                }
            }

            cpp_finalize(&cpp_ctx);
            cpp_deinit(&cpp_ctx);

            return 0;
        } else if (strcmp(option, "print_types") == 0) {
            /*atoms_init();
            init_report_system();
            compilation_unit_init(&compilation_unit);

            TranslationUnit* tu = cuik_compile_file(&compilation_unit, cuik_source_files[0],
                                                    big_array_length(cuik_include_dirs),
                                                    &cuik_include_dirs[0], true, thread_pool);
*/
            fprintf(stderr, "TODO\n");
            return 0;
        }

        fprintf(stderr, "Unknown cuik query option '%s' (Supported options):\n", option);
    } else {
        fprintf(stderr, "Unknown cuik query option (Supported options):\n");
    }

    fprintf(stderr, "find_include - Resolve an include file path from name\n");
    fprintf(stderr, "print_types  - Prints all types in the TU\n");
    fprintf(stderr, "\n");
    return 1;
}

////////////////////////////////
// Preprocessor dump
////////////////////////////////
static bool dump_tokens() {
    if (big_array_length(cuik_source_files) != 1) {
        fprintf(stderr, "Standalone preprocessor cannot operate on more than one source file\n");
        abort();
    }

    #if 0
    unsigned char* text = (unsigned char*)read_entire_file(cuik_source_files[0]);
    if (text == NULL) {
        fprintf(stderr, "could not open: %s\n", cuik_source_files[0]);
        return false;
    }

    Lexer l = {cuik_source_files[0], text, text, 1};

    uint64_t t1 = timer_now();
    do {
        lexer_read(&l);
    } while (l.token_type);
    uint64_t t2 = timer_now();

    double elapsed = (t2 - t1) * timer_freq;
    size_t text_len = strlen((char*)text);
    fprintf(stderr, "preprocessor took %.03f seconds (over %zu bytes)\n", elapsed, text_len);
    return true;
    #else
    // Preprocess file
    TokenStream s;
    uint64_t t1, t2;
    {
        CPP_Context cpp_ctx;
        cpp_init(&cpp_ctx);
        cuik_set_cpp_defines(&cpp_ctx);

        t1 = timer_now();
        for (size_t i = 0, cc = big_array_length(cuik_include_dirs); i < cc; i++) {
            cpp_add_include_directory(&cpp_ctx, cuik_include_dirs[i]);
        }

        s = cpp_process(&cpp_ctx, cuik_source_files[0]);
        t2 = timer_now();

        if (settings.dump_defines) {
            cpp_dump(&cpp_ctx);
        }

        cpp_finalize(&cpp_ctx);
    }

    double elapsed = (t2 - t1) * timer_freq;
    fprintf(stderr, "preprocessor took %.03f seconds\n", elapsed);

    const char* last_file = NULL;
    int last_line = 0;

    for (size_t i = 0, cc = arrlen(s.tokens); i < cc; i++) {
        Token* t = &s.tokens[i];
        SourceLoc* loc = &s.locations[SOURCE_LOC_GET_DATA(t->location)];

        if (last_file != loc->line->filepath && strcmp(loc->line->filepath, "<temp>") != 0) {
            char str[MAX_PATH];

            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* in = (const char*)loc->line->filepath;
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

            printf("\n#line %d \"%s\"\t", loc->line->line, str);
            last_file = loc->line->filepath;
        }

        if (last_line != loc->line->line) {
            printf("\n/* line %3d */\t", loc->line->line);
            last_line = loc->line->line;
        }

        printf("%.*s ", (int)(t->end - t->start), t->start);
    }
    #endif
    return true;
}

////////////////////////////////
// Entry & CLI
////////////////////////////////
static void print_help(const char* executable_path) {
    O("Usage: %s [<options>] <command> [<args>]", executable_path ? executable_path : "<unknown>");
    O("");
    O("Commands:");
    O("  query                - query for compiler or source code info");
    O("");
    O("Options:");
    O("  -h, --help           - print this help screen");
    O("  -v, -V, --version    - print version");
    O("");
    O("  --thin-errors        - displays errors without the line preview");
    O("  --pedantic           - disables CuikC extensions");
    O("  -O                   - run optimizations");
    O("  -P                   - emit preprocessor output");
    O("  -c                   - emit object file output");
    O("  -t                   - type check");
    O("  -r                   - run the compiled result");
    O("  -I        <path>     - add include directory");
    O("  -o        <path>     - define output path for binary and intermediates");
    O("  --ir                 - compile until IR generation");
    O("  --ir:emit            - compile until IR generation and print IR output");
    O("  --freestanding       - compile in freestanding mode (doesn't allow for the OS or C runtime usage, only freestanding headers)");
    O("  --crt     <name>     - choose the libc you wanna compile with");
    O("  --lib     <name>     - link against a static library");
    O("  --threads <count>    - chooses how many threads to spawn");
    O("  --target  <name>     - choose a target platform to compile to");
    O("  --nopp               - No preprocessor lmao");
    O("  --verbose            - prints out the linker command used");
    O("");
}

// we can do a bit of filter such as '*.c' where it'll take all
// paths in the folder that end with .c
static void append_input_path(const char* path) {
    // avoid using the filters if we dont need to :p
    bool needs_filter = false;
    for (const char* p = path; *p; p++) {
        if (*p == '*') {
            needs_filter = true;
            break;
        }
    }

    if (needs_filter) {
        #ifdef _WIN32
        const char* slash = path;
        for (const char* p = path; *p; p++) {
            if (*p == '/' || *p == '\\') {
                slash = p;
            }
        }

        WIN32_FIND_DATA find_data;
        HANDLE find_handle = FindFirstFile(path, &find_data);
        if (find_handle == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "could not filter path: %s\n", path);
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
            fprintf(stderr, "internal error: failed to close filter\n");
            abort();
        }
        #else
        fprintf(stderr, "filepath filters not supported on your platform yet :(\n");
        fprintf(stderr, "umm... i mean you can probably remind me if you want :)\n");
        abort();
        #endif
    } else {
        big_array_put(cuik_source_files, path);
    }
}

static void print_version(const char* install_dir) {
    printf("cuik version %d.%d\n", CUIK_COMPILER_MAJOR, CUIK_COMPILER_MINOR);
    printf("install directory: %s\n", install_dir);
    printf("cuik include directory: %s\n", cuik_include_directory);

    #ifdef _WIN32
    printf("windows sdk include: %S\n", s_vswhere.windows_sdk_include);
    printf("visual studio include: %S\n", s_vswhere.vs_include_path);
    #endif
}

int main(int argc, char* argv[]) {
    #ifdef _WIN32
    {
        // Enable ANSI/VT sequences on windows
        HANDLE output_handle = GetStdHandle(STD_OUTPUT_HANDLE);
        if (output_handle != INVALID_HANDLE_VALUE) {
            DWORD old_mode;
            if (GetConsoleMode(output_handle, &old_mode)) {
                SetConsoleMode(output_handle, old_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
            }
        }
    }
    #endif

    // We hook the crash handler to create crash dumps
    hook_crash_handler();

    if (argc < 2) {
        print_help(argv[0]);
        return 1;
    }

    cuik_detect_crt_include();
    settings.stage_to_stop_at = STAGE_FINAL;

    #ifdef _WIN32
    // This is used to detect includes for the preprocessor
    // and library paths for the linker
    s_vswhere = MicrosoftCraziness_find_visual_studio_and_windows_sdk();

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
    cuik_libraries = big_array_create(const char*, false);
    cuik_include_dirs = big_array_create(const char*, false);

    const char* output_name = NULL;

    // parse command
    if (strcmp(argv[1], "query") == 0) {
        // Query is like a magic Swiss army knife inside of Cuik
        // so it acts differently from everyone else
        return execute_query_operation(argv[2], 3, argc, argv);
    } else if (strcmp(argv[1], "help") == 0) {
        print_help(argv[0]);
        return 0;
    }

    // parse options
    for (size_t i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            append_input_path(argv[i]);
            continue;
        }

        // --option
        if (argv[i][1] == '-') {
            const char* option = &argv[i][2];
            if (strcmp(option, "lib") == 0) {
                i += 1;
                if (i >= argc) {
                    fprintf(stderr, "error: expected filepath\n");
                    return 1;
                }

                char* ctx;
                char* a = strtok_r(argv[i], ",", &ctx);
                while (a != NULL) {
                    big_array_put(cuik_libraries, a);
                    a = strtok_r(NULL, ",", &ctx);
                }
            } else if (strcmp(option, "freestanding") == 0) {
                settings.freestanding = true;
            } else if (strcmp(option, "target") == 0) {
                i += 1;
                if (i >= argc) {
                    fprintf(stderr, "error: expected target\n");
                    fprintf(stderr, "supported targets:\n");
                    for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
                        fprintf(stderr, "\t%s\n", target_options[i].name);
                    }
                    fprintf(stderr, "\n");
                    return 1;
                }

                const char* value = argv[i];

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
                    fprintf(stderr, "error: unsupported target: %s\n", value);
                    fprintf(stderr, "supported targets:\n");
                    for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
                        fprintf(stderr, "\t%s\n", target_options[i].name);
                    }
                    fprintf(stderr, "\n");
                    return 1;
                }
            } else if (strcmp(option, "threads") == 0) {
                i += 1;
                if (i >= argc) {
                    fprintf(stderr, "error: expected number\n");
                    return 1;
                }

                int num;
                int matches = sscanf(argv[i], "%d", &num);
                if (matches != 1) {
                    fprintf(stderr, "error: expected integer for thread count\n");
                    return 1;
                }

                if (num < 1 || num > TB_MAX_THREADS) {
                    fprintf(stderr, "error: expected thread count between 1-%d\n", TB_MAX_THREADS);
                    return 1;
                }

                settings.num_of_worker_threads = num;
            } else if (strcmp(option, "help") == 0) {
                print_help(argv[0]);
            } else if (strcmp(option, "nopp") == 0) {
                settings.nopp = true;
            } else if (strcmp(option, "verbose") == 0) {
                settings.verbose = true;
            } else if (strcmp(option, "thin-errors") == 0) {
                report_using_thin_errors = true;
            } else if (strcmp(option, "exercise") == 0) {
                settings.exercise = true;
            } else if (strstr(option, "ir") == option) {
                settings.stage_to_stop_at = STAGE_IR;

                char* p = strchr(option, ':');
                if (p != NULL && strcmp(p + 1, "emit") == 0) {
                    settings.emit_partial_results = true;
                }
            } else if (strcmp(option, "pedantic") == 0) {
                settings.pedantic = true;
            } else if (strcmp(option, "crt") == 0) {
                i += 1;
                if (i >= argc) {
                    fprintf(stderr, "error: expected valid crt name\n");
                    fprintf(stderr, "supported crts:\n");
                    for (int i = 0; i < LIBC_OPTION_COUNT; i++) {
                        fprintf(stderr, "  %-10s C%-5d %s\n", libc_options[i].name, libc_options[i].version, libc_options[i].static_link ? "Static" : "Shared");
                    }
                    return 1;
                }

                const char* name = argv[i];
                for (int i = 0; i < LIBC_OPTION_COUNT; i++) {
                    if (strcmp(name, libc_options[i].name) == 0) {
                        chosen_libc = &libc_options[i];
                        break;
                    }
                }

                if (chosen_libc == NULL) {
                    fprintf(stderr, "error: expected name\n");
                    fprintf(stderr, "supported LibCs:\n");
                    for (int i = 0; i < LIBC_OPTION_COUNT; i++) {
                        fprintf(stderr, "  %-10s C%-5d %s\n", libc_options[i].name, libc_options[i].version, libc_options[i].static_link ? "Static" : "Shared");
                    }
                    return 1;
                }
            } else {
                fprintf(stderr, "error: unknown argument: %s\n", argv[i]);
                return 1;
            }
        } else {
            switch (argv[i][1]) {
                case 'h':
                print_help(argv[0]);
                return 0;

                case 'v':
                case 'V':
                print_version(argv[0]);
                return 0;

                case 'P':
                if (argv[i][2] == 'd') {
                    settings.dump_defines = true;
                }
                settings.stage_to_stop_at = STAGE_PREPROC;
                break;
                case 't':
                settings.stage_to_stop_at = STAGE_TYPES;
                break;
                case 'c':
                settings.stage_to_stop_at = STAGE_OBJ;
                break;

                case 'g':
                settings.is_debug_info = true;
                break;
                case 'r':
                settings.run_output = true;
                break;
                case 'o': {
                    i += 1;
                    if (i >= argc) {
                        fprintf(stderr, "error: expected filepath\n");
                        return 1;
                    }

                    output_name = argv[i];
                    break;
                }
                case 'T':
                settings.is_time_report = true;
                break;
                case 'O':
                settings.optimize = true;
                break;
                case 'I': {
                    i += 1;
                    if (i >= argc) {
                        fprintf(stderr, "error: expected filepath\n");
                        return 1;
                    }

                    size_t len = strlen(argv[i]);
                    if (len > 0 && argv[i][len - 1] != '\\' && argv[i][len - 1] != '/') {
                        char* newstr = malloc(len + 2);
                        snprintf(newstr, len + 2, "%s/", argv[i]);

                        big_array_put(cuik_include_dirs, newstr);
                    }
                    break;
                }

                default:
                fprintf(stderr, "error: unknown argument: %s\n", argv[i]);
                return 1;
            }
        }
    }

    // choose a CRT if we haven't gotten one already
    #if _WIN32
    if (chosen_libc == NULL) {
        chosen_libc = &libc_options[settings.freestanding ? 0 /* None */ : 1 /* UCRT */];
    }
    #else
    {
        fprintf(stderr, "error: TODO pick a crt manually... idk man\n");
        return 1;
    }
    #endif

    settings.nostdlib = ((chosen_libc - libc_options) == 0);
    settings.static_crt = chosen_libc->static_link;

    if (big_array_length(cuik_source_files) == 0) {
        fprintf(stderr, "error: expected input files\n");
        return 1;
    }

    // Get target descriptor from explicit (or default) target option
    switch (target_arch) {
        case TB_ARCH_X86_64:
        target_desc = get_x64_target_descriptor();
        break;

        default:
        fprintf(stderr, "Cannot compile to your target machine");
        return 1;
    }

    // Get first filename without extension
    {
        const char* filename = output_name ? output_name : cuik_source_files[0];
        const char* ext = strrchr(filename, '.');
        size_t len = ext ? (ext - filename) : strlen(filename);

        if (filename[len - 1] == '/' &&
            filename[len - 1] == '\\') {
            // we have an output directory instead of a file
            sprintf_s(cuik_file_no_ext, MAX_PATH, "%.*s%s", (int)len, filename, cuik_source_files[0]);
        } else {
            memcpy(cuik_file_no_ext, filename, len);
            cuik_file_no_ext[len] = '\0';
        }
    }

    if (settings.verbose) {
        printf("Includes:\n");
        for (size_t i = 0; i < big_array_length(cuik_include_dirs); i++) {
            printf("  %s\n", cuik_include_dirs[i]);
        }
        printf("Output:\n");
        printf("  %s\n", cuik_file_no_ext);
    }

    // Initialize some subsystems that everyone uses
    timer_init();
    init_report_system();

    if (settings.stage_to_stop_at <= STAGE_PREPROC) {
        // only preprocessor work
        dump_tokens();
        return 0;
    }

    char obj_output_path[MAX_PATH];
    if (target_system == TB_SYSTEM_WINDOWS) {
        // if we spit out an exectuable, we'll keep the object file as a temporary and delete it to avoid cluttering crap
        if (settings.stage_to_stop_at >= STAGE_FINAL) {
            assert(L_tmpnam <= 260);
            if (tmpnam(obj_output_path) == NULL) {
                fprintf(stderr, "cannot get a temporary file for the .obj... resorting to violence\n");
                return 1;
            }
        } else {
            sprintf_s(obj_output_path, 260, "%s.obj", cuik_file_no_ext);
        }
    } else if (target_system == TB_SYSTEM_LINUX) {
        sprintf_s(obj_output_path, 260, "%s.o", cuik_file_no_ext);
    }

    // Open profiler file stream
    if (settings.is_time_report) {
        char report_filename[MAX_PATH];
        sprintf_s(report_filename, 260, "%s.json", cuik_file_no_ext);

        timer_open(report_filename);
    }

    // Build project
    timed_block("total") {
        timed_block("cuik") {
            compile_project(obj_output_path, true);

            if (settings.exercise) {
                // just delays the compilation because... you're fat
                uint64_t t1 = timer_now();
                double elapsed = 0.0;

                // 60 seconds of gamer time
                printf("Waiting around...\n");

                int old_chars = -1;
                while (elapsed = (timer_now() - t1) * timer_freq, elapsed < 60.5) {
                    int num_chars = (int)((elapsed / 60.0) * 30.0);
                    if (num_chars != old_chars) {
                        old_chars = num_chars;

                        printf("\r[");
                        for (int i = 0; i < num_chars; i++) printf("#");
                        for (int i = 0; i < 30 - num_chars; i++) printf(" ");
                        printf("]");
                    }

                    thrd_yield();
                }
                printf("\n");
                printf("Cool!\n");
            }
        }

        if (settings.stage_to_stop_at >= STAGE_FINAL) {
            timed_block("linker") {
                Linker l;
                if (linker_init(&l)) {
                    // Add system libpaths
                    linker_add_default_libpaths(&l);
                    linker_add_libpath(&l, "W:/Workspace/Cuik/crt/lib/");

                    // Add Cuik output
                    linker_add_input_file(&l, obj_output_path);

                    // Add input libraries
                    size_t count = big_array_length(cuik_libraries);
                    for (size_t i = 0; i < count; i++) {
                        linker_add_input_file(&l, cuik_libraries[i]);
                    }

                    if (settings.freestanding) {
                        #ifdef _WIN32
                        linker_add_input_file(&l, "win32_rt.lib");
                        #endif
                    }

                    for (int i = 0; i < chosen_libc->lib_count; i++) {
                        linker_add_input_file(&l, chosen_libc->libs[i]);
                    }

                    linker_invoke_system(&l, cuik_file_no_ext, settings.verbose, chosen_libc->name);
                    //linker_invoke_tb(&l, cuik_file_no_ext, settings.verbose);
                    linker_deinit(&l);

                    remove(obj_output_path);
                }
            }
        }
    }

    // Close out profiler output (it doesn't include the linking)
    timer_close();

    if (settings.stage_to_stop_at >= STAGE_FINAL && settings.run_output) {
        char exe_path[MAX_PATH];
        sprintf_s(exe_path, 260, "%s.exe", cuik_file_no_ext);

        #ifdef _WIN32
        for (char* i = exe_path; *i; i++) {
            if (*i == '/') *i = '\\';
        }
        #endif

        printf("\n\nRunning: %s...\n", exe_path);
        int exit_code = system(exe_path);
        printf("Exit code: %d\n", exit_code);

        return exit_code;
    }

    return 0;
}
