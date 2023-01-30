#include "tb_internal.h"
#include <stdarg.h>

#ifdef TB_USE_LUAJIT
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <luajit.h>

#include "opt/lua_glue.h"
#include "opt/lua_prelude.inc"
#endif

#ifdef _WIN32
#define strdup(s) _strdup(s)
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

#define DIFF_BUFFER_SIZE 131072

static void print_to_buffer(void* user_data, const char* fmt, ...) {
    char* buffer = (char*) user_data;
    size_t len = strlen(buffer);

    va_list ap;
    va_start(ap, fmt);
    int result = vsnprintf(buffer + len, DIFF_BUFFER_SIZE - len, fmt, ap);
    va_end(ap);

    if (result < 0 || result >= (DIFF_BUFFER_SIZE - len)) {
        tb_panic("Ran out of space in my internal buffer");
    }
}

#if 1
#define GREEN_TEXT "\x1b[32m"
#define RED_TEXT   "\x1b[31m"
#define RESET_TEXT "\x1b[0m"
#else
#define GREEN_TEXT ""
#define RED_TEXT   ""
#define RESET_TEXT ""
#endif

static void print_diff(const char* description, const char* oldstr, const char* newstr) {
    printf("  %s\n", description);
    /* for (;;) {
        const char* oldend = oldstr ? strchr(oldstr, '\n') : NULL;
        const char* newend = newstr ? strchr(newstr, '\n') : NULL;

        int l = 0;

        if ((oldend - oldstr) == (newend - newstr) && memcmp(oldstr, newstr, newend - newstr) == 0) {
            printf(RESET_TEXT);
            l += printf("%.*s", (int)(oldend - oldstr), oldstr);
            // pad to 80 columns
            while (l < 80) printf(" "), l += 1;
            printf(RESET_TEXT "|");
        } else {
            printf(RED_TEXT);
            if (oldstr) l += printf("%.*s", (int)(oldend - oldstr), oldstr);
            // pad to 80 columns
            while (l < 80) printf(" "), l += 1;

            printf(RESET_TEXT "|");

            printf(GREEN_TEXT);
            if (newstr) l += printf("%.*s", (int)(newend - newstr), newstr);
        }
        printf("\n");

        if (oldend == NULL || newend == NULL) {
            break;
        }

        oldstr = oldend + 1;
        newstr = newend + 1;
    }

    printf(RESET_TEXT);
    printf("\n\n\n");*/
    printf("%s\n\n\n", newstr);
    //__debugbreak();
}

#ifdef TB_USE_LUAJIT
static lua_State* begin_lua_pass(void* l_state) {
    lua_State* L = lua_newthread(l_state);
    lua_getglobal(L, "DA_FUNC");
    return L;
}

static bool end_lua_pass(lua_State* L, int arg_count) {
    int ret = lua_pcall(L, arg_count, 0, 0);
    if (ret > 1) {
        const char* str = lua_tostring(L, -1);
        tb_panic("Lua runtime exited with %d\n%s\n", ret, str);
    } else if (ret == 1) {
        return true;
    } else {
        return false;
    }
}
#endif

#define TB_DEBUG_DIFF_TOOL 0
static bool schedule_function_level_opts(TB_Module* m, size_t pass_count, const TB_Pass passes[]) {
    bool changes = false;

    #if TB_DEBUG_DIFF_TOOL
    int buffer_num = 0;
    char* buffers[2] = {
        tb_platform_heap_alloc(DIFF_BUFFER_SIZE),
        tb_platform_heap_alloc(DIFF_BUFFER_SIZE),
    };
    #endif

    TB_FOR_FUNCTIONS(f, m) {
        // printf("ORIGINAL\n");
        // tb_function_print(f, tb_default_print_callback, stdout, false);
        // printf("\n\n");

        if (tb_function_validate(f) > 0) {
            fprintf(stderr, "Validator failed on %s on original IR\n", f->super.name);
            abort();
        }

        #if TB_DEBUG_DIFF_TOOL
        tb_function_print(f, print_to_buffer, buffers[buffer_num], false);
        buffer_num = 1;
        #endif

        FOREACH_N(j, 0, pass_count) {
            switch (passes[j].mode) {
                case TB_BASIC_BLOCK_PASS: {
                    TB_FOR_BASIC_BLOCK(bb, f) {
                        if (passes[j].l_state != NULL) {
                            #ifdef TB_USE_LUAJIT
                            lua_State* L = begin_lua_pass(passes[j].l_state);
                            lua_pushlightuserdata(L, f);
                            lua_pushinteger(L, bb);
                            changes |= end_lua_pass(L, 2);
                            #else
                            tb_panic("Not compiled with luajit support");
                            #endif
                        } else {
                            changes |= passes[j].bb_run(f, bb);
                        }
                    }
                    break;
                }

                case TB_LOOP_PASS: {
                    // We probably want a function to get all this info together
                    TB_TemporaryStorage* tls = tb_tls_allocate();
                    TB_Predeccesors preds = tb_get_temp_predeccesors(f, tls);

                    TB_Label* doms = tb_tls_push(tls, f->bb_count * sizeof(TB_Label));
                    tb_get_dominators(f, preds, doms);

                    // probably don't wanna do this using heap allocations
                    TB_LoopInfo loops = tb_get_loop_info(f, preds, doms);

                    FOREACH_N(k, 0, loops.count) {
                        const TB_Loop* l = &loops.loops[k];

                        if (passes[j].l_state != NULL) {
                            #ifdef TB_USE_LUAJIT
                            lua_State* L = begin_lua_pass(passes[j].l_state);
                            lua_pushlightuserdata(L, f);
                            lua_pushlightuserdata(L, (void*) l);
                            changes |= end_lua_pass(L, 2);
                            #else
                            tb_panic("Not compiled with luajit support");
                            #endif
                        } else {
                            changes |= passes[j].loop_run(f, l);
                        }
                    }

                    tb_free_loop_info(loops);
                    break;
                }

                case TB_FUNCTION_PASS:
                if (passes[j].l_state != NULL) {
                    #ifdef TB_USE_LUAJIT
                    lua_State* L = begin_lua_pass(passes[j].l_state);
                    lua_pushlightuserdata(L, f);
                    changes |= end_lua_pass(L, 1);
                    #else
                    tb_panic("Not compiled with luajit support");
                    #endif
                } else {
                    changes |= passes[j].func_run(f);

                    // printf("%s\n", passes[j].name);
                    // tb_function_print(f, tb_default_print_callback, stdout, false);
                    // printf("\n\n");

                    if (tb_function_validate(f) > 0) {
                        fprintf(stderr, "Validator failed on %s after %s\n", f->super.name, passes[j].name);
                        abort();
                    }
                }
                break;

                default: tb_unreachable();
            }

            // tb_function_print(f, tb_default_print_callback, stdout, false);

            #if TB_DEBUG_DIFF_TOOL
            tb_function_print(f, print_to_buffer, buffers[buffer_num], false);
            int next = (buffer_num + 1) % 2;
            print_diff(passes[j].name, buffers[next], buffers[buffer_num]);
            buffer_num = next;
            #endif

            // pause
            // printf("Was just %s\n", passes[j].name);
            // getchar();
            // printf("==================================================\n\n\n");
        }
    }

    #if TB_DEBUG_DIFF_TOOL
    tb_platform_heap_free(buffers[1]);
    tb_platform_heap_free(buffers[0]);
    #endif

    return changes;
}

static bool schedule_module_level_opt(TB_Module* m, const TB_Pass* pass) {
    // this is the only module level mode we have rn
    if (pass->mode != TB_MODULE_PASS) {
        tb_unreachable();
    }

    if (pass->l_state != NULL) {
        #ifdef TB_USE_LUAJIT
        lua_State* L = begin_lua_pass(pass->l_state);
        lua_pushlightuserdata(L, m);
        return end_lua_pass(L, 1);
        #else
        fprintf(stderr, "Not compiled with luajit support");
        return false;
        #endif
    } else {
        return pass->mod_run(m);
    }
}

TB_API bool tb_module_optimize(TB_Module* m, size_t pass_count, const TB_Pass passes[]) {
    bool changes = false;

    size_t i = 0;
    while (i < pass_count) {
        // anything below or equal to function-level passes can be trivially
        // parallel and thus we handle this as a
        size_t sync = i;
        for (; sync < pass_count; sync++) {
            if (passes[sync].mode > TB_FUNCTION_PASS) break;
        }

        // TODO(NeGate): convert this into a trivial parallel dispatch
        if (sync != i) {
            changes |= schedule_function_level_opts(m, sync - i, &passes[i]);
            i = sync;
        }

        // synchronize and handle the module level stuff
        // TODO(NeGate): this requires special scheduling to be threaded
        // but it's completely possible
        for (; i < pass_count; i++) {
            if (passes[i].mode <= TB_FUNCTION_PASS) break;

            // run module level
            changes |= schedule_module_level_opt(m, &passes[i]);
        }
    }

    return changes;
}

#ifdef TB_USE_LUAJIT
TB_API TB_Pass tb_opt_load_lua_pass(const char* path, enum TB_PassMode mode) {
    lua_State *L = luaL_newstate();
    if (L == NULL) abort();

    // Load prelude into block
    luaL_openlibs(L);
    int status = luaL_loadstring(L, PRELUDE);
    if (status != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("could not load lua script: %d\n%s\n", status, str);
    }

    // pcall will call that block as a function
    int ret = lua_pcall(L, 0, 0, 0);
    if (ret != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("Lua runtime exited with %d\n%s\n", ret, str);
    }

    // Load passes
    status = luaL_loadfile(L, path);
    if (status != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("could not load lua script: %d\n%s\n", status, str);
    }

    // Returns the function for the pass
    ret = lua_pcall(L, 0, 1, 0);
    if (ret != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("Lua runtime exited with %d\n%s\n", ret, str);
    }

    lua_setglobal(L, "DA_FUNC");
    return (TB_Pass){ .mode = mode, .name = NULL, .l_state = L };
}

TB_API void tb_opt_unload_lua_pass(TB_Pass* p) {
    lua_close((lua_State*) p->l_state);
}
#endif
