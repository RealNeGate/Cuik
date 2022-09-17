#include <cuik.h>
#include <cuik_ast.h>
#include "helper.h"
#include "big_array.h"
#include "cli_parser.h"
#include "json_perf.h"
#include "flint_perf.h"
#include "bindgen.h"

#ifndef __CUIK__
#define CUIK_ALLOW_THREADS 1
#else
#define CUIK_ALLOW_THREADS 0
#endif

#if CUIK_ALLOW_THREADS
#include <stdatomic.h>
#include "threadpool.h"
#endif

enum { IRGEN_TASK_BATCH_SIZE = 8192 };
#define TIMESTAMP(x) if (args_verbose) mark_timestamp(x)

// compiler arguments
static DynArray(const char*) include_directories;
static DynArray(const char*) input_libraries;
static DynArray(const char*) input_objects;
static DynArray(const char*) input_files;
static DynArray(const char*) input_defines;
static DynArray(TB_Pass) da_passes;
static const char* output_name;
static char output_path_no_ext[FILENAME_MAX];

static TB_OutputFlavor flavor = TB_FLAVOR_EXECUTABLE;

static bool args_ir;
static bool args_ast;
static bool args_types;
static bool args_run;
static bool args_nocrt;
static bool args_pploc;
static bool args_assembly;
static bool args_time;
static bool args_verbose;
static bool args_syntax_only;
static bool args_debug_info;
static bool args_preprocess;
static bool args_exercise;
static bool args_experiment;
static bool args_use_syslinker = true;
static int args_threads = -1;

static Bindgen* args_bindgen;
static int args_opt_level;

static TB_Module* mod;
static Cuik_FileCache* fscache;

static Cuik_IThreadpool* ithread_pool;
static CompilationUnit compilation_unit;
static Cuik_Target target_desc;

static struct {
    const char* key;

    const Cuik_ArchDesc* (*arch_fn)(void);
    Cuik_System system;
} target_options[] = {
    { "x64_windows", cuik_get_x64_target_desc, CUIK_SYSTEM_WINDOWS },
    { "x64_macos",   cuik_get_x64_target_desc, CUIK_SYSTEM_MACOS   },
    { "x64_linux",   cuik_get_x64_target_desc, CUIK_SYSTEM_LINUX   },
};
enum { TARGET_OPTION_COUNT = sizeof(target_options) / sizeof(target_options[0]) };

static void initialize_opt_passes(void) {
    char custom_luapath[FILENAME_MAX];
    da_passes = dyn_array_create(TB_Pass);

    if (args_opt_level) {
        dyn_array_put(da_passes, tb_opt_hoist_locals());
        dyn_array_put(da_passes, tb_opt_merge_rets());

        dyn_array_put(da_passes, tb_opt_instcombine());
        dyn_array_put(da_passes, tb_opt_dead_expr_elim());
        dyn_array_put(da_passes, tb_opt_dead_block_elim());
        dyn_array_put(da_passes, tb_opt_subexpr_elim());

        dyn_array_put(da_passes, tb_opt_mem2reg());

        dyn_array_put(da_passes, tb_opt_instcombine());
        dyn_array_put(da_passes, tb_opt_dead_expr_elim());
        dyn_array_put(da_passes, tb_opt_dead_block_elim());
        dyn_array_put(da_passes, tb_opt_subexpr_elim());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());

        if (args_experiment) {
            sprintf_s(custom_luapath, FILENAME_MAX, "%s/custom.lua", crt_dirpath);
            dyn_array_put(da_passes, tb_opt_load_lua_pass(custom_luapath, TB_FUNCTION_PASS));
        }

        // dyn_array_put(da_passes, tb_opt_inline());

        // aggresive optimizations
        // TODO(NeGate): loop optimizations, data structure reordering
        // switch optimizations

        dyn_array_put(da_passes, tb_opt_compact_dead_regs());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
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
    return sysinfo.dwNumberOfProcessors;
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

static int count_pp_lines(TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuik_get_tokens(s);
    size_t count = cuik_get_token_count(s);

    int line_count = 0;
    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];
        SourceLoc* loc = &s->locations[t->location];

        if (last_file != loc->line->filepath && strcmp(loc->line->filepath, "<temp>") != 0) {
            line_count += 1;
            last_file = loc->line->filepath;
        }

        if (last_line != loc->line->line) {
            line_count += 1;
            last_line = loc->line->line;
        }
    }

    return line_count;
}

static SourceLoc* try_for_nicer_loc(TokenStream* s, SourceLoc* loc) {
    while (loc->line->filepath[0] == '<' && loc->line->parent != 0) {
        loc = &s->locations[loc->line->parent];
    }

    return loc;
}

static void dump_tokens(FILE* out_file, TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuik_get_tokens(s);
    size_t count = cuik_get_token_count(s);

    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];
        SourceLoc* loc = try_for_nicer_loc(s, &s->locations[t->location]);

        if (last_file != loc->line->filepath && strcmp(loc->line->filepath, "<temp>") != 0) {
            char str[FILENAME_MAX];

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

            fprintf(out_file, "\n#line %d \"%s\"\t", loc->line->line, str);
            last_file = loc->line->filepath;
        }

        if (last_line != loc->line->line) {
            fprintf(out_file, "\n/* line %3d */\t", loc->line->line);
            last_line = loc->line->line;
        }

        fprintf(out_file, "%.*s ", (int)(t->end - t->start), t->start);
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

            TB_Function* func = NULL;
            const char* name = task.stmts[i]->decl.name;
            if (name == NULL) {
                // these are untracked in the gen ir because they don't map to named IR stuff
                func = cuik_stmt_gen_ir(task.tu, task.stmts[i]);
            } else {
                CUIK_TIMED_BLOCK("IrGen: %s", name) {
                    func = cuik_stmt_gen_ir(task.tu, task.stmts[i]);
                }
            }

            if (func != NULL) {
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
    size_t count;

    #if CUIK_ALLOW_THREADS
    atomic_size_t* remaining;
    #endif
} CodegenTask;

static void codegen_job(void* arg) {
    CodegenTask task = *((CodegenTask*) arg);

    CUIK_TIMED_BLOCK("Codegen: %zu", task.count) {
        tb_module_compile_functions(mod, task.count, task.start, /* args_opt_level ? TB_ISEL_COMPLEX : */ TB_ISEL_FAST);
    }

    #if CUIK_ALLOW_THREADS
    if (task.remaining != NULL) *task.remaining -= 1;
    #endif
}

// it'll use the normal CLI crap to do so
static Cuik_CPP* make_preprocessor(const char* filepath) {
    Cuik_CPP* cpp = malloc(sizeof(Cuik_CPP));
    cuikpp_init(cpp, filepath);
    cuikpp_set_common_defines(cpp, &target_desc, !args_nocrt);

    dyn_array_for(i, include_directories) {
        cuikpp_add_include_directory(cpp, include_directories[i]);
    }

    dyn_array_for(i, input_defines) {
        const char* equal = strchr(input_defines[i], '=');

        if (equal == NULL) {
            cuikpp_define_empty(cpp, input_defines[i]);
        } else {
            cuikpp_define_slice(
                cpp,
                // before equals
                equal - input_defines[i], input_defines[i],
                // after equals
                strlen(equal + 1), equal + 1
            );
        }
    }

    // run the preprocessor
    if (cuikpp_default_run(cpp, fscache) == CUIKPP_ERROR) {
        abort();
    }

    if (args_bindgen != NULL) {
        cuik_lock_compilation_unit(&compilation_unit);

        // include any of the files that are directly included by the original file
        CUIKPP_FOR_FILES(it, cpp) {
            if (it.file->depth == 2) {
                args_bindgen->include(it.file->filepath);
            }
        }

        TokenStream* tokens = cuikpp_get_token_stream(cpp);
        CUIKPP_FOR_DEFINES(it, cpp) {
            if (cuikpp_is_in_main_file(tokens, it.loc)) {
                args_bindgen->define(it);
            }
        }

        cuik_unlock_compilation_unit(&compilation_unit);
    }

    cuikpp_finalize(cpp);
    return cpp;
}

static void free_preprocessor(Cuik_CPP* cpp) {
    cuikpp_deinit(cpp);
    free(cpp);
}

static void compile_file(void* arg) {
    Cuik_CPP* cpp = arg;

    // parse
    Cuik_ErrorStatus errors;
    Cuik_TranslationUnitDesc desc = {
        .tokens         = cuikpp_get_token_stream(cpp),
        .errors         = &errors,
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

    if (args_bindgen != NULL) {
        cuik_lock_compilation_unit(&compilation_unit);
        TokenStream* tokens = cuikpp_get_token_stream(cpp);

        args_bindgen->file(cuikpp_get_main_file(tokens));

        DefinedTypeEntry* defined_types = NULL;
        CUIK_FOR_TOP_LEVEL_STMT(it, tu, 1) {
            Stmt* s = *it.start;
            if (cuikpp_is_in_main_file(tokens, s->loc)) {
                args_bindgen->decl(tu, &defined_types, s);
            }
        }

        cuik_unlock_compilation_unit(&compilation_unit);
    }

    cuik_set_translation_unit_user_data(tu, cpp);
    cuik_add_to_compilation_unit(&compilation_unit, tu);
}

static void preproc_file(void* arg) {
    const char* input = (const char*)arg;

    // preproc
    Cuik_CPP* cpp = make_preprocessor(input);

    if (ithread_pool != NULL) {
        CUIK_CALL(ithread_pool, submit, compile_file, cpp);
    } else {
        compile_file(cpp);
    }
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

        if (str_ends_with(newstr, ".o") || str_ends_with(newstr, ".obj")) {
            dyn_array_put(input_objects, newstr);
        } else {
            dyn_array_put(input_files, newstr);
        }
    }
}

static void irgen(void) {
    TIMESTAMP("IR generation");

    CUIK_TIMED_BLOCK("IR generation") {
        if (ithread_pool != NULL) {
            #if CUIK_ALLOW_THREADS
            size_t task_capacity = 0;
            FOR_EACH_TU(tu, &compilation_unit) {
                size_t c = cuik_num_of_top_level_stmts(tu);
                task_capacity += (c + (IRGEN_TASK_BATCH_SIZE - 1)) / IRGEN_TASK_BATCH_SIZE;
            }

            IRGenTask* tasks = malloc(task_capacity * sizeof(IRGenTask));
            atomic_size_t tasks_remaining = task_capacity;

            size_t task_count = 0;
            FOR_EACH_TU(tu, &compilation_unit) {
                // dispose the preprocessor crap now
                free_preprocessor((Cuik_CPP*) cuik_get_translation_unit_user_data(tu));

                CUIK_FOR_TOP_LEVEL_STMT(it, tu, 8192) {
                    assert(task_count < task_capacity);
                    IRGenTask* task = &tasks[task_count++];
                    *task = (IRGenTask){
                        .tu = tu,
                        .stmts = it.start,
                        .count = it.count,
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
                Cuik_CPP* cpp = cuik_get_translation_unit_user_data(tu);
                cuikpp_deinit(cpp);
                free(cpp);

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
        size_t count = 0, capacity = tb_estimate_function_batch_count(mod);
        atomic_size_t tasks_remaining = capacity;

        CodegenTask* tasks = malloc(capacity * sizeof(CodegenTask));
        TB_FOR_FUNCTION_BATCH(it, mod) {
            tasks[count] = (CodegenTask){
                .start = it.start,
                .count = it.count,
                .remaining = &tasks_remaining
            };

            CUIK_CALL(ithread_pool, submit, codegen_job, &tasks[count]);
            count += 1;
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
        TB_FOR_FUNCTIONS(it, mod) {
            tb_module_compile_function(mod, it.f, /* args_opt_level ? TB_ISEL_COMPLEX : */ TB_ISEL_FAST);
        }
    }
}

static bool export_output(void) {
    // TODO(NeGate): do a smarter system (just default to whatever the different platforms like)
    TB_DebugFormat debug_fmt = args_debug_info ? TB_DEBUGFMT_CODEVIEW : TB_DEBUGFMT_NONE;

    if (!args_use_syslinker) {
        bool is_windows = (target_desc.sys == CUIK_SYSTEM_WINDOWS);
        const char* extension = NULL;
        switch (flavor) {
            case TB_FLAVOR_OBJECT:     extension = (is_windows?".obj":".o");  break;
            case TB_FLAVOR_STATIC:     extension = (is_windows?".lib":".a");  break;
            case TB_FLAVOR_SHARED:     extension = (is_windows?".dll":".so"); break;
            case TB_FLAVOR_EXECUTABLE: extension = (is_windows?".exe":"");    break;
            default: assert(0);
        }

        char path[FILENAME_MAX];
        sprintf_s(path, FILENAME_MAX, "%s%s", output_path_no_ext, extension);

        TIMESTAMP("Export");
        CUIK_TIMED_BLOCK("Export") {
            if (!tb_exporter_write_files(mod, flavor, debug_fmt, 1, (const char*[]) { path })) {
                fprintf(stderr, "error: could not write output. %s\n", path);
                return false;
            }
        }

        return true;
    } else {
        char obj_output_path[FILENAME_MAX];
        sprintf_s(
            obj_output_path, FILENAME_MAX, "%s%s", output_path_no_ext,
            target_desc.sys == CUIK_SYSTEM_WINDOWS ? ".obj" : ".o"
        );

        TIMESTAMP("Export object");
        CUIK_TIMED_BLOCK("Export object") {
            if (!tb_exporter_write_files(mod, TB_FLAVOR_OBJECT, debug_fmt, 1, (const char*[]) { obj_output_path })) {
                remove(obj_output_path);
                fprintf(stderr, "error: could not write object file output. %s\n", obj_output_path);
                return false;
            }
        }

        if (flavor == TB_FLAVOR_OBJECT) {
            return true;
        }

        TIMESTAMP("Linker");
        CUIK_TIMED_BLOCK("linker") {
            Cuik_Linker l;
            if (cuiklink_init(&l)) {
                // we'll use subsystem windows if they defined WinMain in any of the TUs
                bool subsystem_windows = false;
                FOR_EACH_TU(tu, &compilation_unit) {
                    if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN) {
                        subsystem_windows = true;
                    }
                }

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

#if 0
static uint64_t table[256];

static void emit_range(uint64_t min, uint64_t max, uint64_t old, uint64_t next) {
    old *= 4;

    for (size_t i = min; i < max; i++) {
        table[i] |= (next & 15) << old;
    }
}

static void emit_chars(const char* str, uint64_t old, uint64_t next) {
    old *= 4;

    for (; *str; str++) {
        table[(ptrdiff_t) *str] |= (next & 15) << old;
    }
}

static void emit_all_chars(const char* str, uint64_t old, uint64_t next) {
    old *= 4;

    for (size_t i = 0; i < 256; i++) {
        table[i] |= (next & 15) << old;
    }
}

static void test(const char* str) {
    const char* end = str + strlen(str);

    for (;;) {
        // Skip any whitespace and comments
        // branchless space skip
        str += (*str == ' ');
        if (*str == '\0') {
            // quit, we're done
            return;
        }

        const char* token_start = str;
        uint64_t state = 0;
        for (;;) {
            // table[state][*str]
            uint64_t row = table[(ptrdiff_t) *str];
            uint64_t next_state = (row >> (state * 4)) & 63;
            if (next_state == 0) {
                break;
            }

            // move along
            state = next_state;
            str += 1;
        }

        if (state == 0) {
            break;
        } else if (state == 7) {
            char end = *token_start;
            for (; *str != end; str++) {}
            str++;
        }

        state &= 15;
        printf("'%.*s' %llu\n", (int)(str - token_start), token_start, state);
    }
}

int main(int argc, char** argv) {
    // [A-Za-z]
    emit_range('a', 'z', 0, 1);
    emit_range('A', 'Z', 0, 1);

    // [0-9]+
    emit_range('0', '9', 0, 2);
    emit_range('0', '9', 2, 2);
    emit_chars("bB",     2, 2);
    emit_chars("xX",     2, 13);
    emit_chars(".",      2, 14);
    emit_chars("uilfd",  2, 15);

    // hex
    emit_chars("0123456789ABCDEFabcdef", 13, 13);

    // floats
    emit_chars("0123456789e+-", 14, 14);
    emit_chars("uilfd", 14, 15);

    // suffix
    emit_chars("0123456789", 15, 0);

    // identifier body
    {
        emit_range('a', 'z', 1, 1);
        emit_range('A', 'Z', 1, 1);
        emit_range('0', '9', 1, 1);
    }

    // separators
    emit_chars("@?;:,(){}", 0, 3);

    // char or char-assign
    emit_chars("+*/%!=&^|~", 0, 4);
    emit_chars("=", 4, 5);

    // >= <<=
    emit_chars("><", 0, 6);
    emit_chars("><", 6, 4);

    emit_chars("\'\"", 0, 7);

    emit_chars("-", 0, 8);
    emit_chars("=>", 8, 9);

    emit_chars("#", 0, 10);
    emit_chars("#", 10, 11);

    emit_chars(".", 0, 12);
    emit_chars(".", 12, 12);

    test("hello wOrld b01; 12a(16, 6, 0x1535); 1.f; 16.83573f; 3.4028234e38f; 1.18e-4932l; a += b, a <<= 16, a &= b; a->foo(bar, \"baz\", ...);");
    return 0;
}
#else
int main(int argc, char** argv) {
    cuik_init();
    find_system_deps();

    mark_timestamp(NULL);

    program_name = argv[0];
    include_directories = dyn_array_create(const char*);
    input_libraries = dyn_array_create(const char*);
    input_objects = dyn_array_create(const char*);
    input_files = dyn_array_create(const char*);
    input_defines = dyn_array_create(const char*);

    // get default system
    #if defined(_WIN32)
    target_desc.sys = CUIK_SYSTEM_WINDOWS;
    #elif defined(__linux) || defined(linux)
    target_desc.sys = CUIK_SYSTEM_LINUX;
    #elif defined(__APPLE__) || defined(__MACH__) || defined(macintosh)
    target_desc.sys = CUIK_SYSTEM_MACOS;
    #endif

    // get target
    target_desc.arch = cuik_get_x64_target_desc();

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
                for (int i = 0; i < TARGET_OPTION_COUNT; i++) {
                    if (strcmp(arg.value, target_options[i].key) == 0) {
                        target_desc.arch = target_options[i].arch_fn();
                        target_desc.sys = target_options[i].system;
                        success = true;
                        break;
                    }
                }

                if (!success) {
                    fprintf(stderr, "unknown target: %s (try --list targets)\n", arg.value);
                }
                break;
            }
            case ARG_LIST: {
                if (strcmp(arg.value, "targets") == 0) {
                    printf("Supported targets:\n");
                    for (int i = 0; i < TARGET_OPTION_COUNT; i++) {
                        printf("  %s\n", target_options[i].key);
                    }
                    printf("\n");
                    return EXIT_SUCCESS;
                } else if (strcmp(arg.value, "bindgen") == 0) {
                    printf("Supported bindgen:\n");
                    printf("  c99\n");
                    printf("  odin\n");
                    return EXIT_SUCCESS;
                } else if (strcmp(arg.value, "flavors") == 0) {
                    printf("Supported flavors:\n");
                    printf("  object\n");
                    printf("  shared\n");
                    printf("  static\n");
                    printf("  exec\n");
                    return EXIT_SUCCESS;
                } else {
                    fprintf(stderr, "unknown list name, options are: targets, flavors, bindgen\n");
                }
                break;
            }
            case ARG_FLAVOR: {
                if (strcmp(arg.value, "object") == 0) {
                    flavor = TB_FLAVOR_OBJECT;
                } else if (strcmp(arg.value, "shared") == 0) {
                    flavor = TB_FLAVOR_SHARED;
                } else if (strcmp(arg.value, "static") == 0) {
                    flavor = TB_FLAVOR_STATIC;
                } else if (strcmp(arg.value, "exec") == 0) {
                    flavor = TB_FLAVOR_EXECUTABLE;
                } else {
                    fprintf(stderr, "unknown list name, options are: targets\n");
                }
                break;
            }
            case ARG_BINDGEN: {
                if (strcmp(arg.value, "c99") == 0) {
                    args_bindgen = &bindgen__c99;
                } else if (strcmp(arg.value, "odin") == 0) {
                    args_bindgen = &bindgen__odin;
                } else {
                    fprintf(stderr, "unknown list name, options are: bindgen\n");
                }
                break;
            }
            case ARG_OUT: output_name = arg.value; break;
            case ARG_OBJ: flavor = TB_FLAVOR_OBJECT; break;
            case ARG_RUN: args_run = true; break;
            case ARG_PREPROC: args_preprocess = true; break;
            case ARG_PPLOC: args_pploc = true; break;
            case ARG_TIME: args_time = true; break;
            case ARG_ASM: args_assembly = true; break;
            case ARG_SYNTAX_ONLY: args_syntax_only = true; break;
            case ARG_AST: args_ast = true; break;
            case ARG_TYPES: args_types = true; break;
            case ARG_IR: args_ir = true; break;
            case ARG_NOCRT: args_nocrt = true; break;
            case ARG_VERBOSE: args_verbose = true; break;
            case ARG_EXERCISE: args_exercise = true; break;
            case ARG_BASED: args_use_syslinker = false; break;
            case ARG_EXPERIMENT: args_experiment = true; break;
            case ARG_THREADS: args_threads = atoi(arg.value); break;
            case ARG_DEBUG: args_debug_info = true; break;
            case ARG_TBTESTS: {
                #ifdef TB_COMPILE_TESTS
                int passed = 1, total = 1;
                if (tb_x64_test_suite()) {
                    printf("Failure! tb_x64_test_suite\n");
                    passed -= 1;
                }

                printf("Passed %d of %d!\n", passed, total);
                #else
                printf("Didn't compile TB tests!\n");
                #endif
                return EXIT_SUCCESS;
            }
            case ARG_HELP: {
                print_help();
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

    if (args_assembly) {
        fprintf(stderr, "error: emitting assembly doesn't work yet\n");
        return EXIT_FAILURE;
    }

    {
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
        char* perf_output_path = malloc(FILENAME_MAX);

        #if 0
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.json", output_path_no_ext);

        jsonperf_profiler.user_data = perf_output_path;
        cuik_start_global_profiler(&jsonperf_profiler, false);
        #else
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.flint", output_path_no_ext);

        flintperf_profiler.user_data = perf_output_path;
        cuik_start_global_profiler(&flintperf_profiler, false);
        #endif
    }

    // spin up worker threads
    #if CUIK_ALLOW_THREADS
    threadpool_t* thread_pool = NULL;
    int thread_count = args_threads >= 0 ? args_threads : calculate_worker_thread_count();
    if (thread_count > 1) {
        if (args_verbose) printf("Starting with %d threads...\n", thread_count);

        thread_pool = threadpool_create(thread_count - 1, 4096);
        ithread_pool = malloc(sizeof(Cuik_IThreadpool));
        *ithread_pool = (Cuik_IThreadpool){
            .user_data = thread_pool,
            .submit = tp_submit,
            .work_one_job = tp_work_one_job
        };
    }
    #endif

    initialize_opt_passes();
    cuik_create_compilation_unit(&compilation_unit);

    if (!args_ast && !args_types) {
        TB_FeatureSet features = {0};
        mod = tb_module_create(
            TB_ARCH_X86_64, cuik_system_to_tb(target_desc.sys), &features, false
        );
    }

    fscache = cuik_fscache_create();

    if (args_pploc) {
        int total = 0;
        dyn_array_for(i, input_files) {
            Cuik_CPP* cpp = make_preprocessor(input_files[i]);

            int c = count_pp_lines(cuikpp_get_token_stream(cpp));
            printf("%s : %d (%zu tokens)\n", input_files[i], c, cuik_get_token_count(cuikpp_get_token_stream(cpp)));
            total += c;

            free_preprocessor(cpp);
        }
        printf("Total PPLoc: %d\n", total);
        return EXIT_SUCCESS;
    } else if (args_preprocess) {
        // preproc only
        Cuik_CPP* cpp = make_preprocessor(input_files[0]);

        dump_tokens(stdout, cuikpp_get_token_stream(cpp));
        free_preprocessor(cpp);

        return EXIT_SUCCESS;
    }

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

        TIMESTAMP("Internal link");
        CUIK_TIMED_BLOCK("internal link") {
            cuik_internal_link_compilation_unit(&compilation_unit);
        }

        if (args_syntax_only) goto done;
    }

    if (args_ast) {
        FOR_EACH_TU(tu, &compilation_unit) {
            cuik_dump_translation_unit(stdout, tu, true);
        }
        goto done;
    }

    ////////////////////////////////
    // backend work
    ////////////////////////////////
    cuik_fscache_destroy(fscache);
    irgen();

    if (dyn_array_length(da_passes) != 0) {
        // TODO: we probably want to do the fancy threading soon
        TIMESTAMP("Optimizer");
        CUIK_TIMED_BLOCK("Optimizer") {
            tb_module_optimize(mod, dyn_array_length(da_passes), da_passes);
        }
    }

    if (args_ir) {
        TIMESTAMP("IR Printer");
        TB_FOR_FUNCTIONS(it, mod) {
            tb_function_print(it.f, tb_default_print_callback, stdout, false);
            printf("\n\n");
        }
        goto done;
    }

    CUIK_TIMED_BLOCK("CodeGen") {
        codegen();
    }

    if (!export_output()) {
        return 1;
    }

    tb_free_thread_resources();
    tb_module_destroy(mod);

    done:
    TIMESTAMP("Done");
    #if CUIK_ALLOW_THREADS
    if (thread_pool != NULL) {
        threadpool_free(thread_pool);
        thread_pool = NULL;
    }
    #endif

    if (args_exercise) {
        // just delays the compilation because... you're fat
        uint64_t t1 = cuik_time_in_nanos();
        double elapsed = 0.0;

        // 60 seconds of gamer time
        printf("Waiting around...\n\n");
        printf(
            "So people have told me that 1 minute compiles aren't really that bad\n"
            "so i figured that we should give them the freedom to waste their time\n"
        );

        int old_chars = -1;
        while (elapsed = (cuik_time_in_nanos() - t1) / 1000000000.0, elapsed < 60.5) {
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
    }

    cuik_destroy_compilation_unit(&compilation_unit);
    if (args_time) cuik_stop_global_profiler();

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

        return exit_code;
    }

    return 0;
}
#endif
