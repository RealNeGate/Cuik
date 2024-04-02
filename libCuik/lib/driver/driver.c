#include <cuik.h>
#include <common.h>
#include <futex.h>
#include <log.h>
#include <arena.h>
#include <threads.h>
#include "driver_fs.h"
#include "driver_sched.h"
#include "driver_arg_parse.h"

#include "../targets/targets.h"
#include "../front/parser.h"

#ifdef CUIK_ALLOW_THREADS
#include <stdatomic.h>
#endif

// this is used by the worker routines
typedef struct {
    Cuik_BuildStep* step;
    mtx_t* mutex;
} BuildStepInfo;

struct Cuik_BuildStep {
    enum {
        BUILD_STEP_NONE,

        BUILD_STEP_SYS,
        BUILD_STEP_CC,
        BUILD_STEP_LD,
    } tag;
    size_t dep_count;
    Cuik_BuildStep** deps;

    // returns exit status, anything but 0 is failure.
    void(*invoke)(BuildStepInfo* s);

    // once the step is completed, it'll decrement from the anti dep's
    // remaining
    Cuik_BuildStep* anti_dep;

    bool error_root; // created an error rather than just propagating
    bool visited;

    size_t local_ordinal;

    _Atomic int errors;
    Futex remaining;

    Cuik_IThreadpool* tp;

    union {
        struct {
            Cuik_DriverArgs* args;
            const char* source;

            TB_Arena* arena;
            Cuik_CPP* cpp;
            TranslationUnit* tu;
        } cc;

        struct {
            Cuik_DriverArgs* args;
            CompilationUnit* cu;
        } ld;

        struct {
            char* data;
        } sys;
    };
};

// good names are for losers anyways *copium*
// it's just a one-off struct only used in this file
typedef struct {
    TB_Arena* ir;
    TB_Arena* tmp;
    TB_Arena* code;
} MyArenas;

static MyArenas* get_ir_arena(void) {
    static _Thread_local MyArenas arenas;
    if (arenas.ir == NULL) {
        arenas.ir   = tb_arena_create(TB_ARENA_LARGE_CHUNK_SIZE);
        arenas.tmp  = tb_arena_create(TB_ARENA_LARGE_CHUNK_SIZE);
        arenas.code = tb_arena_create(TB_ARENA_LARGE_CHUNK_SIZE);
    }

    return &arenas;
}

static void step_error(Cuik_BuildStep* s) {
    if (s->anti_dep != NULL) {
        s->anti_dep->errors += 1;
    }
    s->error_root = true;
}

static void step_done(Cuik_BuildStep* s) {
    if (s->anti_dep != NULL) {
        futex_dec(&s->anti_dep->remaining);
    }
}

static bool has_file_ext(const char* path) {
    for (; *path; path++) {
        // if (*path == '/')  return false;
        // if (*path == '\\') return false;
        if (*path == '.')  return true;
    }

    return false;
}

static Cuik_Linker gimme_linker(Cuik_DriverArgs* restrict args) {
    Cuik_Linker l = { .toolchain = args->toolchain };

    // Add system libpaths
    cuiklink_apply_toolchain_libs(&l, args->nocrt);

    dyn_array_for(i, args->libpaths) {
        cuiklink_add_libpath(&l, args->libpaths[i]->data);
    }

    // Add input libraries
    dyn_array_for(i, args->libraries) {
        cuiklink_add_input_file(&l, args->libraries[i]->data);
    }

    return l;
}

static void sys_invoke(BuildStepInfo* info) {
    Cuik_BuildStep* s = info->step;

    // TODO(NeGate): this is going to splay the diagnostics
    // without any care for the rest of the running tasks.
    if (system(s->sys.data) != 0) {
        step_error(s);
    }

    cuik_free(s->sys.data);
    step_done(s);
}

#ifdef CUIK_USE_TB
static void irgen(Cuik_IThreadpool* restrict thread_pool, Cuik_DriverArgs* restrict args, CompilationUnit* restrict cu, TB_Module* mod);

static _Thread_local TB_Worklist* ir_worklist;
static void local_opt_func(TB_Function* f, void* arg) {
    if (ir_worklist == NULL) {
        // we just leak these btw, i don't care yet
        ir_worklist = tb_worklist_alloc();
    }

    Cuik_DriverArgs* args = arg;
    assert(args->optimize);

    const char* name = ((TB_Symbol*) f)->name;
    CUIK_TIMED_BLOCK_ARGS("passes", name) {
        MyArenas* arenas = get_ir_arena();
        float start = tb_arena_current_size(arenas->ir);

        tb_opt(f, ir_worklist, arenas->ir, arenas->tmp, false);

        float end = tb_arena_current_size(arenas->ir);
        log_debug("%s: func=%.1f KiB, total=%.1f KiB", name, (end - start) / 1024.0f, end / 1024.0f);
    }
}

static void apply_func(TB_Function* f, void* arg) {
    if (ir_worklist == NULL) {
        // we just leak these btw, i don't care yet
        ir_worklist = tb_worklist_alloc();
    }

    Cuik_DriverArgs* args = arg;
    bool print_asm = args->assembly;

    const char* name = ((TB_Symbol*) f)->name;
    CUIK_TIMED_BLOCK_ARGS("passes", name) {
        MyArenas* arenas = get_ir_arena();
        float start = tb_arena_current_size(arenas->ir);

        if (args->emit_dot) {
            tb_print_dot(f, tb_default_print_callback, stdout);
        } else if (args->emit_ir) {
            tb_print(f, arenas->tmp);

            // char* str = tb_print_c(f, ir_worklist, arenas->tmp);
            // printf("%s", str);
        } else {
            CUIK_TIMED_BLOCK("codegen") {
                TB_FeatureSet features = { 0 };
                TB_FunctionOutput* out = tb_codegen(f, ir_worklist, arenas->ir, arenas->tmp, arenas->code, &features, print_asm);
                if (print_asm) {
                    tb_output_print_asm(out, stdout);
                    printf("\n\n");
                }
            }
        }

        float end = tb_arena_current_size(arenas->ir);
        log_debug("%s: func=%.1f KiB, total=%.1f KiB", name, (end - start) / 1024.0f, end / 1024.0f);
    }
}
#endif

static void cc_invoke(BuildStepInfo* restrict info) {
    Cuik_BuildStep* s = info->step;
    Cuik_DriverArgs* args = s->cc.args;
    if (args->verbose) {
        mtx_lock(info->mutex);
        printf("CC %s\n", s->cc.source);
        mtx_unlock(info->mutex);
    }

    log_debug("BuildStep %p: cc_invoke %s", s, s->cc.source);

    // dispose the preprocessor crap since we didn't need it
    Cuik_CPP* cpp = s->cc.cpp = cuik_driver_preprocess(s->cc.source, args, true);
    if (cpp == NULL) {
        step_error(s);
        goto done_no_cpp;
    }

    TokenStream* tokens = cuikpp_get_token_stream(cpp);
    if (args->write_deps) {
        FILE* file = stdout;
        if (args->dep_file) {
            file = fopen(args->dep_file, "wb");
        }

        Cuik_Path obj_path;
        if (args->output_name == NULL) {
            cuik_path_set_ext(&obj_path, args->sources[0], 2, ".o");
        } else {
            cuik_path_append2(&obj_path, strlen(args->output_name), args->output_name, 4, ".o");
        }

        // write depfile
        Cuik_FileEntry* files = cuikpp_get_files(tokens);
        size_t file_count = cuikpp_get_file_count(tokens);

        fprintf(file, "%s: ", obj_path.data);
        for (size_t i = 0; i < file_count; i++) {
            const char* filename = files[i].filename;
            if (filename[0] == '$') continue;

            fprintf(file, "\\\n  %s ", filename);
        }
        fprintf(file, "\n");

        if (args->dep_file) {
            fclose(file);
        }
    }

    if (args->preprocess) {
        cuikpp_dump_tokens(tokens);
        goto done;
    } else if (args->test_preproc) {
        goto done;
    }

    Cuik_ParseResult result;
    CUIK_TIMED_BLOCK_ARGS("parse", s->cc.source) {
        s->cc.arena = tb_arena_create(TB_ARENA_LARGE_CHUNK_SIZE);

        result = cuikparse_run(args->version, tokens, args->target, s->cc.arena, false);
        s->cc.tu = result.tu;

        if (result.error_count > 0) {
            step_error(s);
            goto done;
        }
    }

    log_debug("BuildStep %p: parsed file", s);

    CompilationUnit* cu = (s->anti_dep != NULL && s->anti_dep->tag == BUILD_STEP_LD) ? s->anti_dep->ld.cu : NULL;
    TranslationUnit* tu = result.tu;

    cuik_set_tu_ordinal(tu, s->local_ordinal);

    // #pragma comment(lib, "foo.lib")
    Cuik_ImportRequest* imports = result.imports;
    if (cu != NULL) {
        if (imports != NULL) {
            cuik_lock_compilation_unit(cu);
            for (; imports != NULL; imports = imports->next) {
                Cuik_Path* p = cuik_malloc(sizeof(Cuik_Path));
                cuik_path_set(p, imports->lib_name);
                dyn_array_put(args->libraries, p);
            }
            cuik_unlock_compilation_unit(cu);
        }

        cuik_add_to_compilation_unit(cu, tu);
    }

    if (cuiksema_run(tu, NULL) > 0) {
        step_error(s);
        goto done;
    }

    if (args->syntax_only) {
        goto done;
    } else if (args->ast) {
        CUIK_FOR_EACH_TU(tu, cu) {
            // log_error("Cannot dump translation unit... TODO");
            cuik_dump_translation_unit(stdout, tu, true);
        }
        goto done;
    }

    // we wanna display diagnostics before any of the backend stuff
    mtx_lock(info->mutex);
    cuikdg_dump_to_file(tokens, stderr);
    mtx_unlock(info->mutex);

    #ifdef CUIK_USE_TB
    TB_Module* mod = cu->ir_mod;
    CUIK_TIMED_BLOCK("Allocate IR") {
        cuikcg_allocate_ir2(tu, mod, args->debug_info);
        /*if (s->tp) {
            cuikcg_allocate_ir(tu, s->tp, mod, args->debug_info);
        } else {
            cuikcg_allocate_ir2(tu, mod, args->debug_info);
        }*/
    }

    CUIK_TIMED_BLOCK("IR Gen") {
        irgen(s->tp, args, cu, mod);

        // once we've complete debug info and diagnostics we don't need line info
        CUIK_TIMED_BLOCK("Free CPP") {
            cuiklex_free_tokens(tokens);
            cuikpp_free(cpp);
        }
    }
    #endif

    if (!args->preserve_ast) {
        CUIK_TIMED_BLOCK("Destroy TU") {
            cuik_destroy_translation_unit(tu);
        }

        CUIK_TIMED_BLOCK("Free arena") {
            tb_arena_destroy(s->cc.arena);
        }
    }

    goto done_no_cpp;

    // these are called for early exits
    done: cuikdg_dump_to_file(tokens, stderr);
    done_no_cpp: step_done(s);
}

static void ld_invoke(BuildStepInfo* info) {
    Cuik_BuildStep* s = info->step;
    Cuik_DriverArgs* args = s->ld.args;
    if (args->verbose) {
        mtx_lock(info->mutex);
        printf("LINK\n");
        mtx_unlock(info->mutex);
    }

    log_debug("BuildStep %p: ld_invoke", s);

    // Without the backend, we can't link... it's basically just stubbed out
    #ifdef CUIK_USE_TB
    TB_Module* mod = s->ld.cu->ir_mod;

    CUIK_TIMED_BLOCK("Backend") {
        if (args->optimize) {
            int t = 0;
            do {
                CUIK_TIMED_BLOCK("Local opts") {
                    log_debug("Optimizing in functions... t=%d", ++t);
                    cuiksched_per_function(s->tp, args->threads, s->ld.cu, mod, args, local_opt_func);
                    log_debug("Interprocedural opts...");
                }
            } while (tb_module_ipo(mod));
        }

        if (args->emit_ir) {
            // char* pre = tb_c_prelude(mod);
            // printf("%s", pre);
        }

        cuiksched_per_function(s->tp, args->threads, s->ld.cu, mod, args, apply_func);
    }

    // Once the frontend is complete we don't need this... unless we wanna keep it
    if (!args->preserve_ast) {
        cuik_destroy_compilation_unit(s->ld.cu);
    }

    if (!cuik_driver_does_codegen(args)) {
        goto done;
    }

    // TODO(NeGate): do a smarter system (just default to whatever the different platforms like)
    TB_DebugFormat debug_fmt = (args->debug_info ? TB_DEBUGFMT_CODEVIEW : TB_DEBUGFMT_NONE);
    Cuik_System sys = cuik_get_target_system(args->target);

    Cuik_Path output_path;
    if (args->output_name == NULL) {
        cuik_path_set(&output_path, sys == TB_SYSTEM_WINDOWS ? "a.exe" : "a.out");
    } else if (!has_file_ext(args->output_name) && cuik_get_target_system(args->target) == CUIK_SYSTEM_WINDOWS) {
        cuik_path_append2(&output_path, strlen(args->output_name), args->output_name, 4, ".exe");
    } else {
        cuik_path_set(&output_path, args->output_name);
    }

    if (args->run) {
        fprintf(stderr, "C JIT unsupported here :(\n");
        goto done;
    }

    ////////////////////////////////
    // generate object file
    ////////////////////////////////
    if (args->based && args->flavor != TB_FLAVOR_OBJECT) {
        TB_ExecutableType exe;
        switch (sys) {
            case CUIK_SYSTEM_WINDOWS: exe = TB_EXECUTABLE_PE;  break;
            case CUIK_SYSTEM_LINUX:   exe = TB_EXECUTABLE_ELF; break;

            default:
            fprintf(stderr, "unsupported platform to link with... sorry (contact NeGate)\n");
            goto done;
        }

        TB_Linker* l = tb_linker_create(exe, args->target->arch);

        TB_Arena* arena = get_ir_arena()->ir;
        TB_ArenaSavepoint sp = tb_arena_save(arena);

        // locate libraries and feed them into TB... in theory this process
        // can be somewhat multithreaded so we might wanna consider that.
        int errors = 0;
        Cuik_Linker tmp_linker = gimme_linker(args);
        char path[FILENAME_MAX];
        dyn_array_for(i, tmp_linker.inputs) {
            CUIK_TIMED_BLOCK(tmp_linker.inputs[i]) {
                if (!cuiklink_find_library(&tmp_linker, path, tmp_linker.inputs[i])) {
                    fprintf(stderr, "could not find library: %s\n", tmp_linker.inputs[i]);
                    step_error(s);
                    errors++;
                    goto skip;
                }

                FileMap fm = open_file_map(path);
                tb_linker_append_library(
                    l,
                    (TB_Slice){ (const uint8_t*) cuik_strdup(path), strlen(path) },
                    (TB_Slice){ fm.data, fm.size }
                );
            }

            skip:;
        }

        if (errors) {
            fprintf(stderr, "library search paths:\n");
            dyn_array_for(i, tmp_linker.libpaths) {
                fprintf(stderr, "  %s\n", tmp_linker.libpaths[i]);
            }
            goto error;
        }

        CUIK_TIMED_BLOCK("tb_linker_append_module") {
            tb_linker_append_module(l, mod);
        }

        if (args->entrypoint) {
            tb_linker_set_entrypoint(l, args->entrypoint);
        }

        if (args->subsystem) {
            tb_linker_set_subsystem(l, args->subsystem);
        }

        /*TB_LinkerMsg m;
        while (tb_linker_get_msg(l, &m)) {
            if (m.tag == TB_LINKER_MSG_IMPORT) {
                // TODO(NeGate): implement this
            }
        }*/

        TB_ExportBuffer buffer = tb_linker_export(l, arena);
        if (!tb_export_buffer_to_file(buffer, output_path.data)) {
            goto error;
        }

        error:
        tb_arena_restore(arena, sp);
        step_error(s);
        tb_module_destroy(mod);
        goto done;
    } else {
        Cuik_Path obj_path;
        if (args->output_name == NULL) {
            cuik_path_set_ext(&obj_path, args->sources[0], 2, ".o");
        } else {
            cuik_path_set_ext(&obj_path, &output_path, 2, ".o");
        }

        TB_Arena* arena = get_ir_arena()->ir;
        TB_ArenaSavepoint sp = tb_arena_save(arena);

        TB_ExportBuffer buffer = tb_module_object_export(mod, arena, debug_fmt);
        tb_module_destroy(mod);

        // copy into file
        if (!tb_export_buffer_to_file(buffer, obj_path.data)) {
            step_error(s);
            goto done;
        }
        tb_arena_restore(arena, sp);

        if (args->flavor == TB_FLAVOR_OBJECT) {
            goto done;
        }

        ////////////////////////////////
        // run system linker
        ////////////////////////////////
        CUIK_TIMED_BLOCK("linker") {
            Cuik_Linker l = gimme_linker(args);
            cuiklink_add_input_file(&l, obj_path.data);
            cuiklink_invoke(&l, args, output_path.data, args->output_name);
            cuiklink_deinit(&l);
        }
    }
    #endif

    done: step_done(s);
}

Cuik_BuildStep* cuik_driver_sys(Cuik_DriverArgs* args, const char* cmd) {
    Cuik_BuildStep* s = cuik_calloc(1, sizeof(Cuik_BuildStep));
    s->tag = BUILD_STEP_SYS;
    s->invoke = sys_invoke;
    s->sys.data = cuik_strdup(cmd);
    return s;
}

Cuik_BuildStep* cuik_driver_cc(Cuik_DriverArgs* args, const char* source) {
    Cuik_BuildStep* s = cuik_calloc(1, sizeof(Cuik_BuildStep));
    s->tag = BUILD_STEP_CC;
    s->invoke = cc_invoke;
    s->cc.source = source;
    s->cc.args = args;
    return s;
}

Cuik_BuildStep* cuik_driver_ld(Cuik_DriverArgs* args, int dep_count, Cuik_BuildStep** deps) {
    Cuik_BuildStep* s = cuik_calloc(1, sizeof(Cuik_BuildStep));
    s->tag = BUILD_STEP_LD;
    s->dep_count = dep_count;
    s->deps = deps;
    s->invoke = ld_invoke;
    s->remaining = dep_count;
    s->ld.cu = cuik_create_compilation_unit();
    s->ld.args = args;

    #ifdef CUIK_USE_TB
    s->ld.cu->ir_mod = tb_module_create(
        args->target->arch, (TB_System) cuik_get_target_system(args->target), args->run
    );
    #endif

    for (size_t i = 0; i < dep_count; i++) {
        deps[i]->anti_dep = s;
    }
    return s;
}

TranslationUnit* cuik_driver_cc_get_tu(Cuik_BuildStep* s) {
    assert(s->tag == BUILD_STEP_CC);
    return s->cc.tu;
}

CompilationUnit* cuik_driver_ld_get_cu(Cuik_BuildStep* s) {
    assert(s->tag == BUILD_STEP_LD);
    return s->ld.cu;
}

static void step_submit(Cuik_BuildStep* s, Cuik_IThreadpool* tp, mtx_t* mutex, bool has_siblings) {
    assert(!s->visited);
    s->visited = true;
    s->tp = tp;

    // submit dependencies
    size_t dep_count = s->dep_count;
    if (dep_count > 0) {
        for (size_t i = 0; i < s->dep_count; i++) {
            s->deps[i]->local_ordinal = i;
            step_submit(s->deps[i], tp, mutex, dep_count > 1);
        }

        if (tp) {
            // once dependencies are complete, we can invoke the step
            while (s->remaining > 0) { CUIK_CALL(tp, work_one_job); }
        }

        // we can't run the step with broken deps, forward the error and early out
        if (s->errors != 0) {
            step_error(s);
            return;
        }
    }

    BuildStepInfo info = { s, mutex };
    CUIK_TIMED_BLOCK("task invoke") {
        if (tp != NULL && has_siblings) {
            log_debug("Punting build step %p to another thread", s);
            CUIK_CALL(tp, submit, (Cuik_TaskFn) s->invoke, sizeof(info), &info);
        } else {
            // we're an only child, there's no reason to multithread
            s->invoke(&info);
        }
    }
}

bool cuik_step_run(Cuik_BuildStep* s, Cuik_IThreadpool* tp) {
    // create temporary mutex for locked operations (usually logging)
    mtx_t m;
    mtx_init(&m, mtx_plain);
    step_submit(s, tp, &m, false);
    mtx_destroy(&m);

    return s->errors == 0;
}

void cuik_step_free(Cuik_BuildStep* s) {
    for (size_t i = 0; i < s->dep_count; i++) {
        cuik_step_free(s->deps[i]);
    }

    if (s->tag == BUILD_STEP_SYS) {
        cuik_free(s->sys.data);
    }

    cuik_free(s);
}

bool cuik_driver_does_codegen(const Cuik_DriverArgs* args) {
    return !args->emit_dot && !args->emit_ir && !args->test_preproc && !args->preprocess && !args->syntax_only && !args->ast;
}

void cuikpp_dump_tokens(TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuikpp_get_tokens(s);
    size_t count = cuikpp_get_token_count(s);

    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];

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

            printf("\n#line %d \"%s\"\t", r.line, str);
            last_file = r.file->filename;
        }

        if (last_line != r.line) {
            printf("\n/* line %3d */\t", r.line);
            last_line = r.line;
        }

        if (t->type == TOKEN_STRING_WIDE_SINGLE_QUOTE || t->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
            printf("L");
        }

        printf("%.*s ", (int) t->content.length, t->content.data);
    }
    printf("\n");
}

void cuik_free_driver_args(Cuik_DriverArgs* args) {
    dyn_array_for(i, args->sources) cuik_free(args->sources[i]);
    dyn_array_for(i, args->includes) cuik_free(args->includes[i]);
    dyn_array_for(i, args->libraries) cuik_free(args->libraries[i]);
    dyn_array_for(i, args->defines) cuik_free(args->defines[i]);

    dyn_array_destroy(args->sources);
    dyn_array_destroy(args->includes);
    dyn_array_destroy(args->libraries);
    dyn_array_destroy(args->defines);
}

static bool run_cpp(Cuik_CPP* cpp, const Cuik_DriverArgs* args, bool should_finalize) {
    CUIK_TIMED_BLOCK("set CPP options") {
        cuik_set_standard_defines(cpp, args);

        dyn_array_for(i, args->includes) {
            cuikpp_add_include_directory(cpp, false, args->includes[i]->data);
        }

        dyn_array_for(i, args->defines) {
            const char* equal = strchr(args->defines[i], '=');

            if (equal == NULL) {
                cuikpp_define_empty_cstr(cpp, args->defines[i]);
            } else {
                cuikpp_define(
                    cpp,
                    // before equals
                    equal - args->defines[i], args->defines[i],
                    // after equals
                    strlen(equal + 1), equal + 1
                );
            }
        }
    }

    // run the preprocessor
    if (cuikpp_run(cpp) == CUIKPP_ERROR) {
        cuikdg_dump_to_file(cuikpp_get_token_stream(cpp), stderr);
        cuikpp_free(cpp);
        return false;
    }

    if (should_finalize) {
        cuikpp_finalize(cpp);
    }

    return true;
}

CUIK_API Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_DriverArgs* args, bool should_finalize) {
    Cuik_CPP* cpp = NULL;
    CUIK_TIMED_BLOCK("cuikpp_make") {
        cpp = cuikpp_make(&(Cuik_CPPDesc){
                .version       = args->version,
                .case_insensitive = args->toolchain.case_insensitive,
                .filepath      = filepath,
                .locate        = cuikpp_locate_file,
                .fs            = cuikpp_default_fs,
                .diag_data     = args->diag_userdata,
                .diag          = args->diag_callback,
            });
    }

    return run_cpp(cpp, args, should_finalize) ? cpp : NULL;
}

CUIK_API Cuik_CPP* cuik_driver_preprocess_str(String source, const Cuik_DriverArgs* args, bool should_finalize) {
    Cuik_CPP* cpp = NULL;
    CUIK_TIMED_BLOCK("cuikpp_make") {
        cpp = cuikpp_make(&(Cuik_CPPDesc){
                .version       = args->version,
                .case_insensitive = args->toolchain.case_insensitive,
                .fs_data       = &source,
                .locate        = cuikpp_locate_file,
                .fs            = cuikpp_default_fs,
                .diag_data     = args->diag_userdata,
                .diag          = args->diag_callback,
            });
    }

    return run_cpp(cpp, args, should_finalize) ? cpp : NULL;
}

CUIK_API Cuik_CPP* cuik_driver_preprocess_cstr(const char* source, const Cuik_DriverArgs* args, bool should_finalize) {
    Cuik_CPP* cpp = NULL;
    CUIK_TIMED_BLOCK("cuikpp_make") {
        cpp = cuikpp_make(&(Cuik_CPPDesc){
                .version       = args->version,
                .case_insensitive = args->toolchain.case_insensitive,
                .fs_data       = &(String){ strlen(source), (const unsigned char*) source },
                .locate        = cuikpp_locate_file,
                .fs            = cuikpp_default_fs,
                .diag_data     = args->diag_userdata,
                .diag          = args->diag_callback,
            });
    }

    return run_cpp(cpp, args, should_finalize) ? cpp : NULL;
}

#ifdef CUIK_USE_TB
typedef struct {
    TB_Module* mod;
    TranslationUnit* tu;
    const Cuik_DriverArgs* args;

    Stmt** stmts;
    size_t count;

    #if CUIK_ALLOW_THREADS
    Futex* remaining;
    #endif
} IRGenTask;

static void irgen_job(void* arg) {
    IRGenTask task = *((IRGenTask*) arg);
    TB_Module* mod = task.mod;

    CompilationUnit* cu = task.tu->parent;
    MyArenas* arenas = get_ir_arena();

    for (size_t i = 0; i < task.count; i++) {
        if ((task.stmts[i]->flags & STMT_FLAGS_HAS_IR_BACKING) == 0) {
            continue;
        }

        const char* name = task.stmts[i]->decl.name;
        TB_Symbol* s;
        CUIK_TIMED_BLOCK_ARGS("irgen", name) {
            float start = tb_arena_current_size(arenas->ir);
            s = cuikcg_top_level(task.tu, mod, arenas->ir, task.stmts[i]);
            float end = tb_arena_current_size(arenas->ir);

            log_debug("%s: func=%.1f KiB, total=%.1f KiB", name, (end - start) / 1024.0f, end / 1024.0f);
        }

        if (s != NULL && s->tag == TB_SYMBOL_FUNCTION) {
            cuik_lock_compilation_unit(cu);
            dyn_array_put(cu->worklist, (TB_Function*) s);
            cuik_unlock_compilation_unit(cu);
        }
    }

    if (task.remaining) {
        futex_dec(task.remaining);
    }
}

static void irgen(Cuik_IThreadpool* restrict thread_pool, Cuik_DriverArgs* restrict args, CompilationUnit* restrict cu, TB_Module* mod) {
    log_debug("IR generation...");

    if (thread_pool != NULL) {
        #if CUIK_ALLOW_THREADS
        size_t stmt_count = 0;
        CUIK_FOR_EACH_TU(tu, cu) {
            if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN && args->subsystem == TB_WIN_SUBSYSTEM_UNKNOWN) {
                args->subsystem = TB_WIN_SUBSYSTEM_WINDOWS;
            }

            stmt_count += cuik_num_of_top_level_stmts(tu);
        }

        size_t batch_size = good_batch_size(args->threads, stmt_count);
        size_t task_capacity = (stmt_count + batch_size - 1) / batch_size;

        IRGenTask* tasks = cuik_malloc(task_capacity * sizeof(IRGenTask));
        Futex remaining = task_capacity;

        size_t task_count = 0;
        CUIK_FOR_EACH_TU(tu, cu) {
            size_t top_level_count = cuik_num_of_top_level_stmts(tu);
            Stmt** top_level = cuik_get_top_level_stmts(tu);
            for (size_t i = 0; i < top_level_count; i += batch_size) {
                size_t end = i + batch_size;
                if (end >= top_level_count) end = top_level_count;

                assert(task_count < task_capacity);
                IRGenTask task = {
                    .mod = mod,
                    .tu = tu,
                    .args = args,
                    .stmts = &top_level[i],
                    .count = end - i,
                    .remaining = &remaining
                };

                CUIK_CALL(thread_pool, submit, irgen_job, sizeof(task), &task);
            }
        }

        // wait for the threads to finish
        while (remaining > 0) { CUIK_CALL(thread_pool, work_one_job); }
        #else
        fprintf(stderr, "Please compile with -DCUIK_ALLOW_THREADS if you wanna spin up threads");
        abort();
        #endif
    } else {
        CUIK_FOR_EACH_TU(tu, cu) {
            if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN && args->subsystem == TB_WIN_SUBSYSTEM_UNKNOWN) {
                args->subsystem = TB_WIN_SUBSYSTEM_WINDOWS;
            }

            size_t c = cuik_num_of_top_level_stmts(tu);
            IRGenTask task = {
                .mod = mod,
                .tu = tu,
                .args = args,
                .stmts = cuik_get_top_level_stmts(tu),
                .count = c
            };

            irgen_job(&task);
        }
    }
}
#endif

void cuik_toolchain_free(Cuik_Toolchain* toolchain) {
    cuik_free(toolchain->ctx);
}

Cuik_Toolchain cuik_toolchain_host(void) {
    #ifdef _MSC_VER
    return cuik_toolchain_msvc();
    #elif __APPLE__
    return cuik_toolchain_darwin();
    #elif __linux__
    return cuik_toolchain_gnu();
    #else
    return (Cuik_Toolchain){ 0 };
    #endif
}
