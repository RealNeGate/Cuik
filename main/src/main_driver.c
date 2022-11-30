#include <cuik.h>
#include "helper.h"
#include "cli_parser.h"
#include "json_perf.h"
#include "flint_perf.h"
#include "bindgen.h"
#include <dyn_array.h>

// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#ifdef _WIN32
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

#ifndef __CUIK__
#define CUIK_ALLOW_THREADS 1
#else
#define CUIK_ALLOW_THREADS 0
#endif

#if CUIK_ALLOW_THREADS
#include <threads.h>
#include <stdatomic.h>
#include "threadpool.h"
#endif

enum {
    IRGEN_TASK_BATCH_SIZE = 8192,
    TB_TASK_BATCH_SIZE = 8192,
};
#define TIMESTAMP(x) if (args_verbose) mark_timestamp(x)

#ifdef _WIN32
#define NULL_FILEPATH "NUL"
#else
#define NULL_FILEPATH "/dev/null"
#endif

static _Atomic int files_with_errors;

// compiler arguments
static DynArray(const char*) include_directories;
static DynArray(const char*) input_libraries;
static DynArray(const char*) input_objects;
static DynArray(const char*) input_files;
static DynArray(const char*) input_defines;
static DynArray(TB_Pass) da_passes;
static const char* output_name;
static char output_path_no_ext[FILENAME_MAX];
static bool output_path_null;

static TB_OutputFlavor flavor = TB_FLAVOR_EXECUTABLE;

static bool args_ir;
static bool args_ast;
static bool args_types;
static bool args_run;
static bool args_nocrt;
static bool args_pprepl;
static bool args_live;
static bool args_time;
static bool args_verbose;
static bool args_syntax_only;
static bool args_debug_info;
static bool args_preprocess;
static bool args_think;
static bool args_use_syslinker = true;
static int args_threads = -1;

static Bindgen* args_bindgen;
static int args_opt_level;

static TB_Module* mod;

static Cuik_IThreadpool* ithread_pool;
static CompilationUnit compilation_unit;
static Cuik_Target* target_desc;

typedef struct TargetOption {
    const char* key;

    Cuik_Target* (*target)(Cuik_System, Cuik_Environment);
    Cuik_System system;
    Cuik_Environment env;
} TargetOption;
static DynArray(TargetOption) target_options;

#include "pp_repl.h"
#include "live.h"

static void exit_or_hook(int code) {
    if (IsDebuggerPresent()) {
        __debugbreak();
    }
    exit(code);
}

static void initialize_targets(void) {
    target_options = dyn_array_create(TargetOption);

    #define M(a, b, c, d) dyn_array_put(target_options, (TargetOption){ a, b, c, d })
    M("x64_windows_msvc", cuik_target_x64,  CUIK_SYSTEM_WINDOWS, CUIK_ENV_MSVC);
    M("x64_macos_gnu",    cuik_target_x64,  CUIK_SYSTEM_MACOS,   CUIK_ENV_GNU);
    M("x64_linux_gnu",    cuik_target_x64,  CUIK_SYSTEM_LINUX,   CUIK_ENV_GNU);
    M("wasm32",           cuik_target_wasm, CUIK_SYSTEM_WEB,     CUIK_ENV_GNU);
}

static void initialize_opt_passes(void) {
    da_passes = dyn_array_create(TB_Pass);

    if (args_opt_level) {
        dyn_array_put(da_passes, tb_opt_hoist_locals());
        dyn_array_put(da_passes, tb_opt_merge_rets());

        dyn_array_put(da_passes, tb_opt_instcombine());
        dyn_array_put(da_passes, tb_opt_dead_expr_elim());
        dyn_array_put(da_passes, tb_opt_dead_block_elim());
        dyn_array_put(da_passes, tb_opt_subexpr_elim());

        dyn_array_put(da_passes, tb_opt_mem2reg());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        dyn_array_put(da_passes, tb_opt_instcombine());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        dyn_array_put(da_passes, tb_opt_dead_expr_elim());
        dyn_array_put(da_passes, tb_opt_dead_block_elim());
        dyn_array_put(da_passes, tb_opt_subexpr_elim());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        dyn_array_put(da_passes, tb_opt_compact_dead_regs());

        // dyn_array_put(da_passes, tb_opt_inline());

        // aggresive optimizations
        // TODO(NeGate): loop optimizations, data structure reordering
        // switch optimizations

        // dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        // dyn_array_put(da_passes, tb_opt_compact_dead_regs());
    }
}

static void mark_timestamp(const char* label) {
    static uint64_t last_mark = 0;
    static const char* last_label = NULL;

    uint64_t time = cuik_time_in_nanos();

    if (last_mark != 0 && last_label != NULL) {
        printf("%s: %f ms\n", last_label, (time - last_mark) / 1000000.0);
    }

    last_mark = time;
    last_label = label;
}

#if CUIK_ALLOW_THREADS
static int calculate_worker_thread_count(void) {
    #ifdef _WIN32
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors - 4;
    #else
    return 1;
    #endif
}

static void tp_submit(void* user_data, void fn(void*), void* arg) {
    threadpool_submit((threadpool_t*) user_data, fn, arg);
}

static void tp_work_one_job(void* user_data) {
    threadpool_work_one_job((threadpool_t*) user_data);
}
#endif

static void dump_tokens(FILE* out_file, TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuikpp_get_tokens(s);
    size_t count = cuikpp_get_token_count(s);

    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];

        /*int depth = 0;
        SourceLoc loc = t->location;
        MacroInvoke* m;
        while ((m = cuikpp_find_macro(s, loc)) != NULL) {
            loc = m->call_site;
            depth++;
        }

        if (depth > 0 && out_file == stdout) {
            fprintf(out_file, "\x1b[0;35m");
        }*/

        ResolvedSourceLoc r = cuikpp_find_location(s, t->location);
        if (last_file != r.file->filename) {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* in = (const char*) r.file->filename;
            char str[FILENAME_MAX], *out = str;

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

            fprintf(out_file, "\n#line %d \"%s\"\t", r.line, str);
            last_file = r.file->filename;
        }

        if (last_line != r.line) {
            fprintf(out_file, "\n/* line %3d */\t", r.line);
            last_line = r.line;
        }

        fprintf(out_file, "%.*s ", (int) t->content.length, t->content.data);

        /*if (depth > 0 && out_file == stdout) {
            fprintf(out_file, "\x1b[0m");
        }*/
    }
}

typedef struct {
    TranslationUnit* tu;

    Stmt** stmts;
    size_t count;

    #if CUIK_ALLOW_THREADS
    atomic_size_t* remaining;
    #endif
} IRGenTask;

static void irgen_job(void* arg) {
    IRGenTask task = *((IRGenTask*) arg);

    // simple function level passes
    TB_Pass passes[] = {
        tb_opt_instcombine(),
        tb_opt_remove_pass_nodes(),
        tb_opt_dead_expr_elim(),
        tb_opt_compact_dead_regs()
    };
    enum { PASS_COUNT = sizeof(passes) / sizeof(passes[0]) };

    CUIK_TIMED_BLOCK("IrGen: %zu", task.count) {
        size_t i = 0;
        while (i < task.count) {
            // skip all the typedefs
            if (task.stmts[i]->decl.attrs.is_typedef || !task.stmts[i]->decl.attrs.is_used) {
                i += 1;
                continue;
            }

            TB_Symbol* sym = NULL;
            const char* name = task.stmts[i]->decl.name;
            if (name == NULL) {
                // these are untracked in the gen ir because they don't map to named IR stuff
                sym = cuikcg_top_level(task.tu, mod, task.stmts[i]);
            } else {
                CUIK_TIMED_BLOCK("IrGen: %s", name) {
                    sym = cuikcg_top_level(task.tu, mod, task.stmts[i]);
                }
            }

            TB_Function* func = tb_symbol_as_function(sym);
            if (func != NULL && args_opt_level == 0) {
                CUIK_TIMED_BLOCK("Canonicalize: %s", name) {
                    for (size_t j = 0; j < PASS_COUNT; j++) {
                        CUIK_TIMED_BLOCK("Opt%s", passes[j].name, name) {
                            passes[j].func_run(func);
                        }
                    }

                    /*#ifndef NDEBUG
                    int error_count = tb_function_validate(func);
                    if (error_count > 0) {
                        fprintf(stderr, "TB validator failed with %d error%s!\n", error_count, error_count ? "s" : "");
                        abort();
                    }
                    #endif*/
                }
            }
            i += 1;
        }
    }

    #if CUIK_ALLOW_THREADS
    if (task.remaining != NULL) *task.remaining -= 1;
    #endif
}

typedef struct {
    TB_Function* start;

    #if CUIK_ALLOW_THREADS
    atomic_size_t* remaining;
    #endif
} CodegenTask;

static void codegen_job(void* arg) {
    CodegenTask task = *((CodegenTask*) arg);

    CUIK_TIMED_BLOCK("Codegen") {
        TB_Function* f = task.start;

        for (size_t i = 0; i < TB_TASK_BATCH_SIZE && f != NULL; i++) {
            tb_module_compile_function(mod, f, TB_ISEL_FAST);
            f = tb_next_function(f);
        }
    }

    #if CUIK_ALLOW_THREADS
    if (task.remaining != NULL) *task.remaining -= 1;
    #endif
}

// it'll use the normal CLI crap to do so
static Cuik_CPP* make_preprocessor(const char* filepath, bool should_finalize) {
    Cuik_CPP* cpp = malloc(sizeof(Cuik_CPP));
    CUIK_TIMED_BLOCK("cuikpp_init") {
        cuikpp_init(cpp, filepath);
    }

    cuikpp_set_common_defines(cpp, target_desc, !args_nocrt);
    dyn_array_for(i, include_directories) {
        cuikpp_add_include_directory(cpp, include_directories[i]);
    }

    dyn_array_for(i, input_defines) {
        const char* equal = strchr(input_defines[i], '=');

        if (equal == NULL) {
            cuikpp_define_empty_cstr(cpp, input_defines[i]);
        } else {
            cuikpp_define(
                cpp,
                // before equals
                equal - input_defines[i], input_defines[i],
                // after equals
                strlen(equal + 1), equal + 1
            );
        }
    }

    // run the preprocessor
    if (cuikpp_default_run(cpp) == CUIKPP_ERROR) {
        // dump_tokens(stdout, cuikpp_get_token_stream(cpp));
        files_with_errors++;
        return NULL;
    }

    if (args_bindgen != NULL) {
        cuik_lock_compilation_unit(&compilation_unit);

        // include any of the files that are directly included by the original file
        /*CUIKPP_FOR_FILES(it, cpp) {
            if (it.file->depth == 2) {
                args_bindgen->include(it.file->filepath);
            }
        }

        TokenStream* tokens = cuikpp_get_token_stream(cpp);
        CUIKPP_FOR_DEFINES(it, cpp) {
            if (cuikpp_is_in_main_file(tokens, it.loc.start)) {
                args_bindgen->define(it);
            }
        }*/

        cuik_unlock_compilation_unit(&compilation_unit);
    }

    if (should_finalize) {
        cuikpp_finalize(cpp);
    }
    return cpp;
}

static void free_preprocessor(Cuik_CPP* cpp) {
    cuikpp_deinit(cpp);
    free(cpp);
}

static void compile_file(void* arg);
static void preproc_file(void* arg) {
    const char* input = (const char*)arg;

    // preproc
    Cuik_CPP* cpp = make_preprocessor(input, true);
    if (cpp != NULL) {
        if (ithread_pool != NULL) {
            CUIK_CALL(ithread_pool, submit, compile_file, cpp);
        } else {
            compile_file(cpp);
        }
    } else {
        files_with_errors++;
    }
}

static void compile_file(void* arg) {
    Cuik_ParseResult result;
    TokenStream* tokens = cuikpp_get_token_stream(arg);
    CUIK_TIMED_BLOCK("parse: %s\n", cuikpp_get_main_file(tokens)) {
        result = cuikparse_run(CUIK_VERSION_C23, tokens, target_desc);
        if (result.error_count > 0) {
            printf("Failed to parse with %d errors...\n", result.error_count);
            files_with_errors++;
            return;
        }
    }

    // #pragma comment(lib, "foo.lib")
    Cuik_ImportRequest* imports = result.imports;
    if (imports != NULL) {
        cuik_lock_compilation_unit(&compilation_unit);
        for (; imports != NULL; imports = imports->next) {
            dyn_array_put(input_libraries, imports->lib_name);
        }
        cuik_unlock_compilation_unit(&compilation_unit);
    }

    TranslationUnit* tu = result.tu;
    int r = cuiksema_run(tu, NULL);
    if (r > 0) {
        printf("Failed to type check with %d errors...\n", r);
        files_with_errors++;
        return;
    }

    cuik_set_translation_unit_user_data(tu, arg /* the preprocessor */);
    cuik_add_to_compilation_unit(&compilation_unit, tu);

    // parse
    /*
        Cuik_TranslationUnitDesc desc = {
            .tokens         = cuikpp_get_token_stream(cpp),
            .ir_module      = mod,
            .has_debug_info = args_debug_info,
            .target         = &target_desc,
            #if CUIK_ALLOW_THREADS
            .thread_pool    = ithread_pool ? ithread_pool : NULL,
            #endif
        };

        TranslationUnit* tu = cuik_parse_translation_unit(&desc);
        if (tu == NULL) {
            printf("Failed to parse with errors...");
            exit(1);
        }

        Cuik_ImportRequest* imports = cuik_translation_unit_import_requests(tu);
        if (imports != NULL) {
            cuik_lock_compilation_unit(&compilation_unit);
            for (; imports != NULL; imports = imports->next) {
                dyn_array_put(input_libraries, imports->lib_name);
            }
            cuik_unlock_compilation_unit(&compilation_unit);
        }*/

    /*
    if (args_bindgen != NULL) {
        cuik_lock_compilation_unit(&compilation_unit);
        TokenStream* tokens = cuikpp_get_token_stream(cpp);

        args_bindgen->file(cuikpp_get_main_file(tokens));

        DefinedTypeEntry* defined_types = NULL;
        CUIK_FOR_TOP_LEVEL_STMT(it, tu, 1) {
            Stmt* s = *it.start;
            if (cuikpp_is_in_main_file(tokens, s->loc.start)) {
                args_bindgen->decl(tu, &defined_types, s);
            }
        }

        cuik_unlock_compilation_unit(&compilation_unit);
    }

    cuik_set_translation_unit_user_data(tu, cpp);
    cuik_add_to_compilation_unit(&compilation_unit, tu);*/
}

static bool str_ends_with(const char* cstr, const char* postfix) {
    const size_t cstr_len = strlen(cstr);
    const size_t postfix_len = strlen(postfix);

    return postfix_len <= cstr_len && strcmp(cstr + cstr_len - postfix_len, postfix) == 0;
}

// handles the **.c *.c type stuff
static void filtered_append(const char* path, bool recursive) {
    const char* slash = path;
    for (const char* p = path; *p; p++) {
        if (*p == '/' || *p == '\\') {
            slash = p;
        }
    }

    #ifdef _WIN32
    WIN32_FIND_DATA find_data;
    HANDLE find_handle = FindFirstFile(path, &find_data);

    // loops through normal files
    if (find_handle != INVALID_HANDLE_VALUE) {
        do {
            char tmp[FILENAME_MAX];
            if (slash == path) {
                sprintf_s(tmp, MAX_PATH, "%s", find_data.cFileName);
            } else {
                sprintf_s(tmp, MAX_PATH, "%.*s%s", (int)(slash - path) + 1, path, find_data.cFileName);
            }

            char* new_path = malloc(MAX_PATH);
            if (!cuik_canonicalize_path(new_path, tmp)) {
                fprintf(stderr, "Invalid filepath! %s\n", tmp);
            }

            if (str_ends_with(new_path, ".o") || str_ends_with(new_path, ".obj")) {
                dyn_array_put(input_objects, new_path);
            } else {
                dyn_array_put(input_files, new_path);
            }
        } while (FindNextFile(find_handle, &find_data));
    }
    FindClose(find_handle);

    if (recursive) {
        char dir_path[MAX_PATH];
        sprintf_s(dir_path, sizeof(dir_path), "%.*s*", (int)(slash - path) + 1, path);
        HANDLE dir = FindFirstFile(dir_path, &find_data);

        if (dir != INVALID_HANDLE_VALUE) {
            do {
                if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && find_data.cFileName[0] != '.') {
                    char new_pattern[FILENAME_MAX];
                    sprintf_s(new_pattern, sizeof(new_pattern), "%.*s%s%s", (int)(slash - path) + 1, path, find_data.cFileName, slash);

                    filtered_append(new_pattern, true);
                }
            } while (FindNextFile(dir, &find_data));
        }
        FindClose(dir);
    }

    #else
    fprintf(stderr, "filepath filters not supported on your platform yet :(\n");
    fprintf(stderr, "umm... i mean you can probably remind me if you want :)\n");
    abort();
    #endif
}

// we can do a bit of filter such as '*.c' where it'll take all
// paths in the folder that end with .c
static void append_input_path(const char* path) {
    // we don't check this very well because we're based
    const char* star = NULL;
    for (const char* p = path; *p; p++) {
        if (*p == '*') {
            star = p;
            break;
        }
    }

    if (star != NULL) {
        // this is a really hacky way to handle recursive directories
        // but im a baller so i dont care
        filtered_append(path, star[1] == '*');
    } else {
        char* newstr = malloc(FILENAME_MAX);
        if (!cuik_canonicalize_path(newstr, path)) {
            fprintf(stderr, "Invalid filepath! %s\n", path);
        }

        if (str_ends_with(newstr, ".a") || str_ends_with(newstr, ".lib")) {
            dyn_array_put(input_libraries, newstr);
        } else if (str_ends_with(newstr, ".o") || str_ends_with(newstr, ".obj")) {
            dyn_array_put(input_objects, newstr);
        } else {
            dyn_array_put(input_files, newstr);
        }
    }
}

// we'll use subsystem windows if they defined WinMain in any of the TUs
static bool subsystem_windows = false;

static void irgen(void) {
    TIMESTAMP("IR generation");

    CUIK_TIMED_BLOCK("IR generation") {
        if (ithread_pool != NULL) {
            #if CUIK_ALLOW_THREADS
            size_t task_capacity = 0;
            FOR_EACH_TU(tu, &compilation_unit) {
                if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN) {
                    subsystem_windows = true;
                }

                size_t c = cuik_num_of_top_level_stmts(tu);
                task_capacity += (c + (IRGEN_TASK_BATCH_SIZE - 1)) / IRGEN_TASK_BATCH_SIZE;
            }

            IRGenTask* tasks = malloc(task_capacity * sizeof(IRGenTask));
            atomic_size_t tasks_remaining = task_capacity;

            size_t task_count = 0;
            FOR_EACH_TU(tu, &compilation_unit) {
                // dispose the preprocessor crap now
                free_preprocessor((Cuik_CPP*) cuik_set_translation_unit_user_data(tu, NULL));

                size_t top_level_count = cuik_num_of_top_level_stmts(tu);
                Stmt** top_level = cuik_get_top_level_stmts(tu);
                for (size_t i = 0; i < top_level_count; i += 8192) {
                    size_t end = i + 8192;
                    if (end >= top_level_count) end = top_level_count;

                    assert(task_count < task_capacity);
                    IRGenTask* task = &tasks[task_count++];
                    *task = (IRGenTask){
                        .tu = tu,
                        .stmts = &top_level[i],
                        .count = end - i,
                        .remaining = &tasks_remaining
                    };

                    CUIK_CALL(ithread_pool, submit, irgen_job, task);
                }
            }

            // "highway robbery on steve jobs" job stealing amirite...
            while (atomic_load(&tasks_remaining) != 0) {
                CUIK_CALL(ithread_pool, work_one_job);
            }
            #else
            fprintf(stderr, "Please compile with -DCUIK_ALLOW_THREADS if you wanna spin up threads");
            abort();
            #endif
            // free(tasks);
        } else {
            FOR_EACH_TU(tu, &compilation_unit) {
                if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN) {
                    subsystem_windows = true;
                }

                Cuik_CPP* cpp = cuik_get_translation_unit_user_data(tu);
                free_preprocessor(cpp);

                size_t c = cuik_num_of_top_level_stmts(tu);
                IRGenTask task = {
                    .tu = tu,
                    .stmts = cuik_get_top_level_stmts(tu),
                    .count = c
                };

                irgen_job(&task);
            }
        }
    }
}

static void codegen(void) {
    if (ithread_pool != NULL) {
        #if CUIK_ALLOW_THREADS
        size_t count = 0, capacity = (tb_module_get_function_count(mod) + TB_TASK_BATCH_SIZE - 1) / TB_TASK_BATCH_SIZE;
        atomic_size_t tasks_remaining = capacity;

        CodegenTask* tasks = malloc(capacity * sizeof(CodegenTask));
        size_t i = 0;
        TB_FOR_FUNCTIONS(f, mod) {
            if ((i % TB_TASK_BATCH_SIZE) == 0) {
                assert(count < capacity);

                tasks[count] = (CodegenTask){ .start = f, .remaining = &tasks_remaining };
                CUIK_CALL(ithread_pool, submit, codegen_job, &tasks[count]);
                count += 1;
            }

            i += 1;
        }

        // "highway robbery on steve jobs" job stealing amirite...
        while (atomic_load(&tasks_remaining) != 0) {
            CUIK_CALL(ithread_pool, work_one_job);
        }

        free(tasks);
        #else
        fprintf(stderr, "Please compile with -DCUIK_ALLOW_THREADS if you wanna spin up threads");
        abort();
        #endif /* CUIK_ALLOW_THREADS */
    } else {
        TB_FOR_FUNCTIONS(f, mod) {
            tb_module_compile_function(mod, f, TB_ISEL_FAST);
        }
    }
}

// TODO(NeGate): finish implementing this stuff
/* static void run_as_jit(void) {
    #ifdef _WIN32
    dyn_array_put(input_libraries, "kernel32.lib");

    HMODULE* modules = malloc(sizeof(HMODULE) * dyn_array_length(input_libraries));
    dyn_array_for(i, input_libraries) {
        char tmp[FILENAME_MAX];
        strncpy(tmp, input_libraries[i], FILENAME_MAX);

        // HACK: replace extension
        const size_t cstr_len = strlen(tmp);
        const size_t postfix_len = strlen(".lib");
        if (postfix_len <= cstr_len && strcmp(tmp + cstr_len - postfix_len, ".lib") == 0) {
            strcpy(tmp + cstr_len - postfix_len, ".dll");
        }

        modules[i] = LoadLibraryA(tmp);
        if (modules[i] == NULL) {
            fprintf(stderr, "error: Could not load: %s\n", input_libraries[i]);
            abort();
        }
    }

    TB_FOR_EXTERNALS(e, mod) {
        const char* name = tb_symbol_get_name((TB_Symbol*) e);
        printf("  %s\n", name);

        void* p = NULL;
        dyn_array_for(i, input_libraries) {
            p = GetProcAddress(modules[i], name);
            if (p != NULL) {
                printf("    Loaded from %s (%p)\n", input_libraries[i], p);
                break;
            }
        }

        if (p == NULL) {
            fprintf(stderr, "error: Could not load symbol: %s\n", name);
            abort();
        }
        tb_symbol_bind_ptr((TB_Symbol*) e, p);
    }

    tb_module_export_jit(mod);

    // run the main function
    TB_Function* func = NULL;
    TB_FOR_FUNCTIONS(f, mod) {
        if (strcmp(tb_symbol_get_name((TB_Symbol*) f), "main") == 0) {
            func = f;
            break;
        }
    }

    if (func == NULL) {
        fprintf(stderr, "error: Could not find entrypoint 'main'\n");
        abort();
    }

    void(*jit_entry)(void) = tb_function_get_jit_pos(func);
    jit_entry();
    #else
    fprintf(stderr, "error: JIT not supported yet!\n");
    abort();
    #endif
} */

static bool export_output(void) {
    // TODO(NeGate): do a smarter system (just default to whatever the different platforms like)
    TB_DebugFormat debug_fmt = (args_debug_info ? TB_DEBUGFMT_CODEVIEW : TB_DEBUGFMT_NONE);

    if (!args_use_syslinker) {
        bool is_windows = (cuik_get_target_system(target_desc) == CUIK_SYSTEM_WINDOWS);
        const char* extension = NULL;
        switch (flavor) {
            case TB_FLAVOR_OBJECT:     extension = (is_windows?".obj":".o");  break;
            case TB_FLAVOR_STATIC:     extension = (is_windows?".lib":".a");  break;
            case TB_FLAVOR_SHARED:     extension = (is_windows?".dll":".so"); break;
            case TB_FLAVOR_EXECUTABLE: extension = (is_windows?".exe":"");    break;
            default: assert(0);
        }

        char path[FILENAME_MAX];
        if (output_path_null) {
            strcpy(path, NULL_FILEPATH);
        } else {
            sprintf_s(path, FILENAME_MAX, "%s%s", output_path_no_ext, extension);
        }

        TIMESTAMP("Export");
        CUIK_TIMED_BLOCK("Export") {
            if (!tb_exporter_write_files(mod, flavor, debug_fmt, 1, &(const char*){ path })) {
                fprintf(stderr, "error: could not write output. %s\n", path);
                return false;
            }
        }

        return true;
    } else {
        char obj_output_path[FILENAME_MAX];
        if (output_path_null) {
            strcpy(obj_output_path, NULL_FILEPATH);
        } else {
            sprintf_s(
                obj_output_path, FILENAME_MAX, "%s%s", output_path_no_ext,
                cuik_get_target_system(target_desc) == CUIK_SYSTEM_WINDOWS ? ".obj" : ".o"
            );
        }

        TIMESTAMP("Export object");
        CUIK_TIMED_BLOCK("Export object") {
            if (!tb_exporter_write_files(mod, TB_FLAVOR_OBJECT, debug_fmt, 1, (const char*[]) { obj_output_path })) {
                remove(obj_output_path);
                fprintf(stderr, "error: could not write object file output. %s\n", obj_output_path);
                return false;
            }
        }

        if (flavor == TB_FLAVOR_ASSEMBLY) {
            char cmd[2048];
            snprintf(cmd, 2048, "dumpbin %s /disasm", obj_output_path);
            return system(cmd) == 0;
        }

        if (output_path_null || flavor == TB_FLAVOR_OBJECT) {
            return true;
        }

        TIMESTAMP("Linker");
        CUIK_TIMED_BLOCK("linker") {
            Cuik_Linker l;
            if (cuiklink_init(&l)) {
                if (subsystem_windows) {
                    cuiklink_subsystem_windows(&l);
                }

                // Add system libpaths
                cuiklink_add_default_libpaths(&l);

                char lib_dir[FILENAME_MAX];
                sprintf_s(lib_dir, FILENAME_MAX, "%s/crt/lib/", crt_dirpath);
                cuiklink_add_libpath(&l, lib_dir);

                // Add Cuik output
                cuiklink_add_input_file(&l, obj_output_path);

                // Add input libraries
                dyn_array_for(i, input_libraries) {
                    cuiklink_add_input_file(&l, input_libraries[i]);
                }

                dyn_array_for(i, input_objects) {
                    cuiklink_add_input_file(&l, input_objects[i]);
                }

                if (!args_nocrt) {
                    #ifdef _WIN32
                    cuiklink_add_input_file(&l, "ucrt.lib");
                    cuiklink_add_input_file(&l, "msvcrt.lib");
                    cuiklink_add_input_file(&l, "vcruntime.lib");
                    cuiklink_add_input_file(&l, "win32_rt.lib");
                    #endif
                }

                cuiklink_invoke(&l, output_path_no_ext, "ucrt");
                cuiklink_deinit(&l);
            }
        }

        return true;
    }
}

// returns status code
static int run_compiler(threadpool_t* thread_pool, bool destroy_cu_after_ir) {
    files_with_errors = 0;

    ////////////////////////////////
    // frontend work
    ////////////////////////////////
    TIMESTAMP("Frontend");
    CUIK_TIMED_BLOCK("Frontend") {
        if (ithread_pool != NULL) {
            #if CUIK_ALLOW_THREADS
            dyn_array_for(i, input_files) {
                tp_submit(thread_pool, preproc_file, (void*) input_files[i]);
            }

            threadpool_wait(thread_pool);
            #endif
        } else {
            dyn_array_for(i, input_files) {
                preproc_file((void*) input_files[i]);
            }
        }
    }

    TB_FeatureSet features = { 0 };
    mod = tb_module_create(
        TB_ARCH_X86_64, (TB_System) cuik_get_target_system(target_desc), &features, false
    );

    TIMESTAMP("Internal link");
    CUIK_TIMED_BLOCK("internal link") {
        cuik_internal_link_compilation_unit(&compilation_unit, mod, args_debug_info);
    }

    if (files_with_errors > 0 && dyn_array_length(input_files) > 1) {
        fprintf(stderr, "%d files with errors!\n", files_with_errors);
        return 1;
    }

    if (args_syntax_only) return 0;
    if (args_types) return 0;

    if (args_ast) {
        FOR_EACH_TU(tu, &compilation_unit) {
            cuik_dump_translation_unit(stdout, tu, true);
        }
        return 0;
    }

    ////////////////////////////////
    // backend work
    ////////////////////////////////
    CUIK_TIMED_BLOCK("Backend") {
        irgen();
        if (destroy_cu_after_ir) {
            cuik_destroy_compilation_unit(&compilation_unit);
        }

        if (dyn_array_length(da_passes) != 0) {
            // TODO: we probably want to do the fancy threading soon
            TIMESTAMP("Optimizer");
            CUIK_TIMED_BLOCK("Optimizer") {
                tb_module_optimize(mod, dyn_array_length(da_passes), da_passes);
            }
        }

        if (args_ir) {
            TIMESTAMP("IR Printer");
            TB_FOR_FUNCTIONS(f, mod) {
                tb_function_print(f, tb_default_print_callback, stdout, false);
                printf("\n\n");
            }

            goto cleanup_tb;
        }

        CUIK_TIMED_BLOCK("CodeGen") {
            codegen();
        }
    }

    if (!export_output()) {
        return 1;
    }

    ////////////////////////////////
    // Running executable
    ////////////////////////////////
    if (args_run) {
        char* exe = strdup(output_name);

        #ifdef _WIN32
        for (char* s = exe; *s; s++) {
            if (*s == '/') *s = '\\';
        }
        #endif

        printf("\n\nRunning: %s...\n", exe);
        int exit_code = system(exe);

        printf("Exit code: %d\n", exit_code);
        if (exit_code) return exit_code;
    }

    cleanup_tb:
    tb_free_thread_resources();
    tb_module_destroy(mod);
    return 0;
}

int main(int argc, char** argv) {
    cuik_init();
    find_system_deps();

    mark_timestamp(NULL);
    initialize_targets();

    program_name = argv[0];
    include_directories = dyn_array_create(const char*);
    input_libraries = dyn_array_create(const char*);
    input_objects = dyn_array_create(const char*);
    input_files = dyn_array_create(const char*);
    input_defines = dyn_array_create(const char*);

    // get default system
    #if defined(_WIN32)
    target_desc = cuik_target_x64(CUIK_SYSTEM_WINDOWS, CUIK_ENV_MSVC);
    #elif defined(__linux) || defined(linux)
    target_desc = cuik_target_x64(CUIK_SYSTEM_LINUX, CUIK_ENV_MSVC);
    #elif defined(__APPLE__) || defined(__MACH__) || defined(macintosh)
    target_desc = cuik_target_x64(CUIK_SYSTEM_MACOS, CUIK_ENV_MSVC);
    #endif

    // parse arguments
    int i = 1;
    for (;;) {
        // optimizer levels
        if (i < argc && argv[i][0] == '-' && argv[i][1] == 'O') {
            args_opt_level = atoi(&argv[i][2]);
            i += 1;
            continue;
        }

        Arg arg = get_cli_arg(&i, argc, argv);
        switch (arg.key) {
            case ARG_NONE: {
                if (arg.value == NULL) goto done_wit_args;

                append_input_path(arg.value);
                break;
            }
            case ARG_DEFINE: {
                dyn_array_put(input_defines, arg.value);
                break;
            }
            case ARG_INCLUDE: {
                // resolve a fullpath
                char* newstr = malloc(FILENAME_MAX);
                if (cuik_canonicalize_path(newstr, arg.value)) {
                    size_t end = strlen(newstr);

                    if (newstr[end - 1] != '\\' && newstr[end - 1] != '/') {
                        assert(end+2 < FILENAME_MAX);
                        #ifdef _WIN32
                        newstr[end] = '\\';
                        #else
                        newstr[end] = '/';
                        #endif
                        newstr[end+1] = '\0';
                    }

                    dyn_array_put(include_directories, newstr);
                } else {
                    fprintf(stderr, "error: could not resolve include: %s\n", arg.value);
                    return EXIT_FAILURE;
                }
                break;
            }
            case ARG_LIB: {
                char* newstr = strdup(arg.value);

                char* ctx;
                char* a = strtok_r(newstr, ",", &ctx);
                while (a != NULL) {
                    dyn_array_put(input_libraries, a);
                    a = strtok_r(NULL, ",", &ctx);
                }
                break;
            }
            case ARG_TARGET: {
                bool success = false;
                for (size_t i = 0; i < dyn_array_length(target_options); i++) {
                    if (strcmp(arg.value, target_options[i].key) == 0) {
                        TargetOption* o = &target_options[i];
                        target_desc = o->target(o->system, o->env);
                        success = true;
                        break;
                    }
                }

                if (!success) {
                    fprintf(stderr, "unknown target: %s\n", arg.value);
                    fprintf(stderr, "Supported targets:\n");
                    for (size_t i = 0; i < dyn_array_length(target_options); i++) {
                        fprintf(stderr, "    %s\n", target_options[i].key);
                    }
                    fprintf(stderr, "\n");
                }
                break;
            }
            case ARG_O0: args_opt_level = 0; break;
            case ARG_O1: args_opt_level = 1; break;
            case ARG_OUTPUT: output_name = arg.value; break;
            case ARG_OBJECT: flavor = TB_FLAVOR_OBJECT; break;
            case ARG_PREPROC: args_preprocess = true; break;
            case ARG_RUN: args_run = true; break;
            case ARG_LIVE: args_live = true; break;
            case ARG_PPREPL: args_pprepl = true; break;
            case ARG_AST: args_ast = true; break;
            case ARG_ASSEMBLY: flavor = TB_FLAVOR_ASSEMBLY; break;
            case ARG_SYNTAX: args_syntax_only = true; break;
            case ARG_VERBOSE: args_verbose = true; break;
            case ARG_THINK: args_think = true; break;
            case ARG_BASED: args_use_syslinker = false; break;
            case ARG_TIME: args_time = true; break;
            case ARG_THREADS: args_threads = calculate_worker_thread_count(); break;
            case ARG_DEBUG: args_debug_info = true; break;
            case ARG_EMITIR: args_ir = true; break;
            case ARG_NOLIBC: args_nocrt = true; break;
            case ARG_HELP: {
                print_help(argv[0]);
                return EXIT_SUCCESS;
            }
            default: break;
        }
    }

    done_wit_args:
    if (dyn_array_length(input_files) == 0) {
        fprintf(stderr, "error: no input files!\n");
        return EXIT_FAILURE;
    }

    if (output_name != NULL && strcmp(output_name, "$nul") == 0) {
        output_path_null = true;
    } else {
        const char* filename = output_name ? output_name : input_files[0];
        const char* ext = strrchr(filename, '.');
        size_t len = ext ? (ext - filename) : strlen(filename);

        if (filename[len - 1] == '/' || filename[len - 1] == '\\') {
            const char* slash = strrchr(input_files[0], '/');
            if (!slash) slash = strrchr(input_files[0], '\\');

            if (!slash) slash = input_files[0];
            else slash += 1; // skip the slash

            // we have an output directory instead of a file
            sprintf_s(output_path_no_ext, FILENAME_MAX, "%.*s%s", (int)len, filename, slash);
        } else {
            memcpy(output_path_no_ext, filename, len);
            output_path_no_ext[len] = '\0';
        }

        if (output_name == NULL) {
            #if _WIN32
            char* str = malloc(FILENAME_MAX);
            sprintf_s(str, FILENAME_MAX, "%s.exe", output_path_no_ext);
            output_name = str;
            #else
            output_name = output_path_no_ext;
            #endif
        }
    }

    if (args_time) {
        #if 0
        char* perf_output_path = cuikperf_init(FILENAME_MAX, &json_profiler, false);
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.json", output_path_no_ext);
        #else
        char* perf_output_path = cuikperf_init(FILENAME_MAX, &spall_profiler, false);
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.spall", output_path_no_ext);
        #endif

        cuikperf_start();
    }

    // spin up worker threads
    #if CUIK_ALLOW_THREADS
    threadpool_t* thread_pool = NULL;
    if (args_threads > 1) {
        if (args_verbose) printf("Starting with %d threads...\n", args_threads);

        thread_pool = threadpool_create(args_threads - 1, 4096);
        ithread_pool = malloc(sizeof(Cuik_IThreadpool));
        *ithread_pool = (Cuik_IThreadpool){
            .user_data = thread_pool,
            .submit = tp_submit,
            .work_one_job = tp_work_one_job
        };
    }
    #endif

    initialize_opt_passes();

    if (args_pprepl) {
        return pp_repl();
    } else if (args_preprocess) {
        // preproc only
        Cuik_CPP* cpp = make_preprocessor(input_files[0], true);
        if (cpp) {
            dump_tokens(stdout, cuikpp_get_token_stream(cpp));
            free_preprocessor(cpp);
        } else {
            fprintf(stderr, "Could not preprocess file: %s", input_files[0]);
            return EXIT_FAILURE;
        }

        return EXIT_SUCCESS;
    }

    if (args_live) {
        LiveCompiler l;
        do {
            printf("\x1b[2J");
            printf("OUTPUT OF %s:\n", input_files[0]);

            cuik_create_compilation_unit(&compilation_unit);
            run_compiler(thread_pool, true);
        } while (live_compile_watch(&l));
    } else {
        cuik_create_compilation_unit(&compilation_unit);

        int status = run_compiler(thread_pool, true);
        if (status != 0) exit_or_hook(status);
    }

    TIMESTAMP("Done");
    #if CUIK_ALLOW_THREADS
    if (thread_pool != NULL) {
        threadpool_free(thread_pool);
        thread_pool = NULL;
    }
    #endif

    if (args_think) {
        uint64_t t1 = cuik_time_in_nanos();
        double elapsed = 0.0;

        // 120 seconds of gamer time
        printf("Waiting around...\n\n");
        printf(
            "So people have told me that 2 minute compiles aren't really that bad\n"
            "so i figured that we should give them the freedom to waste their time\n"
        );

        int old_chars = -1;
        while (elapsed = (cuik_time_in_nanos() - t1) / 1000000000.0, elapsed < 120.0) {
            int num_chars = (int)((elapsed / 120.0) * 30.0);
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
    }

    if (args_time) cuikperf_stop();

    return 0;
}