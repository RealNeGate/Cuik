#include <stdio.h>
#include <dyn_array.h>
#include <hash_map.h>
#include <perf.h>
#include "../main/spall_perf.h"

#define CUIK_FS_IMPL
#include <cuik_fs.h>

#include <tb.h>
#include <arena.h>

// compile threads are separate from interpreter threads
#include <threads.h>
#include <stdatomic.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <GL/gl.h>

#include "../main/live.h"

#pragma comment(lib, "opengl32.lib")

// no need for do while crap if it's an expression :p
// it's basically an assert but it doesn't get vaporized
#define CHECK(x) ((x) ? (void)0 : abort())

typedef struct Word Word;

enum {
    // arithmatic
    OP_ADD = -1,
    OP_SUB = -2,
    OP_MUL = -3,
    OP_DIV = -4,

    // stack
    OP_DUP  = -5,
    OP_DROP = -6,

    // memory
    OP_READ  = -7,
    OP_WRITE = -8,

    // control flow
    OP_IF    = -9,  // if{
    OP_ELSE  = -10, // }else{
    OP_CLOSE = -11, // }

    OP_TAIL  = -12, // no name, generated automatically

    // debug
    OP_DUMP  = -13,

    // console
    OP_EMIT  = -14,
    OP_PRINT = -15,
    OP_KEY   = -16,

    // graphics
    OP_RECT  = -17, // ( x y w h -- )
};

struct Word {
    // we don't want to free the word while the
    // JIT is using that memory
    _Atomic int refs;

    NL_Slice name;
    DynArray(int64_t) ops;

    // type
    int arity, outputs;
    int tails;

    // we might wanna move this out of here to avoid false-sharing
    //
    // for now there's no tiers or recompilation.
    //
    // so once 'trip_count' == JIT_THRESHOLD, we submit
    // to the JIT thread. once it's ready later we can
    // check 'jitted'
    _Atomic int trip_count;
    _Atomic(void*) jitted;
};

static void infer(Word* w);

static _Thread_local bool is_main_thread;

#include "forth_dict.h"

static Dictionary root_dict;

////////////////////////////////
// Parser
////////////////////////////////
typedef struct {
    const char* source;
} Parser;

static NL_Slice dup_slice(NL_Slice s) {
    uint8_t* new_mem = cuik_malloc(s.length + 1);
    memcpy(new_mem, s.data, s.length);
    new_mem[s.length] = 0;
    return (NL_Slice){ s.length, new_mem };
}

static NL_Slice slice_fmt(TB_Arena* arena, const char* fmt, ...) {
    char* buf = tb_arena_alloc(arena, 64);

    va_list ap;
    va_start(ap, fmt);
    int len = vsnprintf(buf, 64, fmt, ap);
    va_end(ap);

    tb_arena_pop(arena, buf + len + 1, 64 - (len + 1));
    tb_arena_realign(arena);

    return (NL_Slice){ len, (const uint8_t*) buf };
}

static Word* add_new_to_dictionary(Dictionary* dict, NL_Slice name, int ins, int outs) {
    WordIndex id = alloc_word();
    Word* w = &words.entries[id];
    *w = (Word){ .refs = 1, .name = name, .arity = ins, .outputs = outs };
    dict_put(dict, name.length, (const char*) name.data, -1000 - id);
    return w;
}

static NL_Slice lex(Parser* p) {
    // skip spaces & comments
    const char* line = p->source;
    for (;;) {
        while (*line > 0 && *line <= 32) line++;

        if (*line == 0) {
            return (NL_Slice){ 0 };
        } else if (*line == '\\') {
            while (*line && *line != '\n') line++;
            continue;
        }

        break;
    }

    // read word
    const char* end = line;
    while (*end > 32) end++;

    // advance
    p->source = *end ? end + 1 : end;

    return (NL_Slice){ end - line, (const uint8_t*) line };
}

// parse builds up a word
static void parse(Dictionary* dict, Parser* p, Word* w) {
    for (;;) {
        NL_Slice token = lex(p);

        if (token.length == 0) {
            break;
        } else if (token.length == 1 && token.data[0] == ':') {
            // define word
            NL_Slice name = dup_slice(lex(p));

            WordIndex id = alloc_word();
            Word* w = &words.entries[id];
            *w = (Word){ .refs = 1, .name = name };

            dict_put(dict, name.length, (const char*) name.data, -1000 - id);

            // build up word inside root word
            Word kid = { 0 };
            parse(dict, p, &kid);

            // split from the parse function to avoid resize problems
            kid.name = name;
            *w = kid;

            infer(w);
        } else if (token.length == 4 && memcmp(token.data, "var:", 4) == 0) {
            // define variable
            NL_Slice name = dup_slice(lex(p));

            // allocate some memory for it
            if (tb_arena_is_empty(&dict->data)) {
                tb_arena_create(&dict->data, TB_ARENA_MEDIUM_CHUNK_SIZE);
            }
            void* ptr = tb_arena_alloc(&dict->data, sizeof(int64_t));

            // generate words
            Word* st_word = add_new_to_dictionary(dict, slice_fmt(&dict->data, "%s!", name.data), 1, 0);
            dyn_array_put(st_word->ops, (uintptr_t) ptr);
            dyn_array_put(st_word->ops, OP_WRITE);

            Word* ld_word = add_new_to_dictionary(dict, slice_fmt(&dict->data, "%s@", name.data), 0, 1);
            dyn_array_put(ld_word->ops, (uintptr_t) ptr);
            dyn_array_put(ld_word->ops, OP_READ);

            token = lex(p);
            if (token.length != 1 || token.data[0] != ';') {
                printf("error: missing semicolon after var: %s decl\n", name.data);
                abort();
            }
        } else if (token.length == 1 && token.data[0] == ';') {
            return;
        } else if (token.data[0] == '\'') {
            // char literals
            dyn_array_put(w->ops, token.data[1]);
            assert(token.data[2] == '\'');
        } else if (token.data[0] >= '0' && token.data[0] <= '9') {
            uint64_t num = 0;
            for (size_t i = 0; i < token.length; i++) {
                if (token.data[i] < '0' || token.data[i] > '9') {
                    break;
                }

                num *= 10;
                num += token.data[i] - '0';
            }

            dyn_array_put(w->ops, num);
        } else {
            WordIndex found = dict_get(dict, token.length, (const char*) token.data);
            if (found >= 0) {
                printf("error: undefined word %.*s\n", (int) token.length, token.data);
                abort();
            }

            dyn_array_put(w->ops, found);
        }
    }
}

////////////////////////////////
// Type checker
////////////////////////////////
// represents control flow structures like if{ }else{ }
typedef struct {
    // the outs of a Control must match
    int arity, pos, out_count;

    // TB specific
    TB_Node *entry, *exit;

    // didn't want to write true and false
    TB_Node *on, *off;

    // "phis"
    TB_Node** outs;
} Control;

typedef struct {
    int head;
    Control stack[64];
} ControlStack;

static Control* control_push(TB_Function* f, ControlStack* restrict cs, int pos, int arity) {
    Control* c = &cs->stack[cs->head++];
    c->arity = arity;
    c->pos = pos;

    if (f != NULL) {
        c->entry = tb_inst_get_control(f);
        c->on = tb_inst_region(f);
        c->off = tb_inst_region(f);
        c->exit = tb_inst_region(f);
    }

    return c;
}

static Control* control_peek(ControlStack* restrict cs) {
    return cs->head ? &cs->stack[cs->head - 1] : NULL;
}

static Control* control_pop(ControlStack* restrict cs) {
    return &cs->stack[--cs->head];
}

static int prim_arity(int64_t x) {
    switch (x) {
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_WRITE:
        return 2;

        case OP_DUP:
        case OP_DROP:
        case OP_IF:
        case OP_PRINT:
        case OP_EMIT:
        case OP_READ:
        return 1;

        case OP_DUMP:
        case OP_CLOSE:
        case OP_ELSE:
        case OP_KEY:
        return 0;

        case OP_RECT:
        return 4;

        default:
        assert(0 && "TODO");
        return -1;
    }
}

static void infer(Word* w) {
    // we'll be building this up as we go
    w->arity = 0;
    w->outputs = 0;

    ControlStack cs;
    cs.head = 0;

    bool has_tail = false;
    size_t head = 0, len = dyn_array_length(w->ops);
    for (size_t i = 0; i < len; i++) {
        int64_t x = w->ops[i];

        if (x >= 0) { // push literal
            head++;
        } else if (x <= -1000) {
            Word* new_w = &words.entries[-x - 1000];

            // pluck parameter from outside the word
            if (new_w->arity > head) {
                w->arity += new_w->arity - head;
                head = 0;
            } else {
                head -= new_w->arity;
            }

            // check for tail recursion
            if (w == new_w) {
                // we're literally at the end
                size_t j = i + 1;
                if (j < len && w->ops[j] == OP_ELSE) {
                    // if it's an }else{ we might be able to reach the end
                    int depth = 1;
                    while (j < len && depth != 0) {
                        j += 1;

                        if (w->ops[j] == OP_IF) depth++;
                        if (w->ops[j] == OP_CLOSE) depth--;
                    }
                    assert(j != len && "missing }");
                }

                if (j == len - 1) {
                    w->ops[i] = OP_TAIL;
                    w->tails++;
                }
            }

            head += new_w->outputs;
        } else if (x == OP_TAIL) {
            // tails can't pluck parameter from outside the word
            assert(w->arity <= head);
            head -= w->arity;

            w->tails++;
        } else {
            int arity = prim_arity(x);

            // pluck parameter from outside the word
            if (arity > head) {
                w->arity += arity - head;
                head = 0;
            } else {
                head -= arity;
            }

            switch (x) {
                // arithmatic
                case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: head += 1; break;

                // stack
                case OP_DUP: head += 2; break;
                case OP_DROP: break;

                // memory
                case OP_READ: head += 1; break;

                // console
                case OP_KEY: head += 1; break;
                case OP_EMIT: break;
                case OP_PRINT: break;

                // graphics
                case OP_RECT: break;

                // control flow
                case OP_IF: control_push(NULL, &cs, i, head); break;
                case OP_ELSE: {
                    Control* c = control_peek(&cs);
                    assert(c && "missing if{");

                    c->out_count = head;
                    head = c->arity;
                    break;
                }
                case OP_CLOSE: {
                    Control* c = control_pop(&cs);
                    assert(head == c->out_count && "stinky dynamic effects");
                    break;
                }

                default: assert(0 && "TODO");
            }
        }
    }

    assert(cs.head == 0 && "you're missing closing braces?");
    if (w->tails > 0) {
        assert(w->outputs == 0 && "tail recursive functions can't return things... yet?");
    } else {
        w->outputs = head;
    }

    // dump resulting type
    if (1) {
        printf(": %.*s ( ", (int) w->name.length, w->name.data);
        for (int i = 0; i < w->arity; i++) {
            printf("%c ", 'a'+i);
        }
        printf("-- ");
        for (int i = 0; i < head; i++) {
            printf("%c ", 'A'+i);
        }
        printf(") ... ;\n");
    }
}

////////////////////////////////
// Interpreter
////////////////////////////////
typedef struct Env {
    // data stack
    size_t head;
    uint64_t stack[64];

    // control stack
    size_t control_head;
    uint64_t control[32];

    // debugging
    bool single_step;
} Env;

#define PEEK()  (env->stack[env->head - 1])
#define POP()   (env->stack[--env->head])
#define PUSH(x) (env->stack[env->head++] = (x))

static void compile_word(Word* w);

static void push(Env* env, uint64_t x) {
    assert(env->head < 64);
    env->stack[env->head++] = x;
}

static uint64_t pop(Env* env) {
    assert(env->head > 0);
    return env->stack[--env->head];
}

static uint64_t peek(Env* env) {
    assert(env->head > 0);
    return env->stack[env->head - 1];
}

static void dump(Env* env) {
    printf("[ ");
    for (size_t i = env->head; i--;) {
        printf("%llu ", env->stack[i]);
    }
    printf("]\n");
}

// this is the interp() result
enum {
    // no errors
    INTERP_OK = 0,

    // not enough elements on the stack
    INTERP_UNDERFLOW,

    // the function
    INTERP_NO_JIT,

    // if env->single_step is on this is what we return
    INTERP_STEP,
};

#include "forth_jit.h"

// written as a non-recursive style to accomodate pausing and resuming
// because the JIT will do those sorts of things.
static int interp(Env* env, Word* w) {
    // this is used to initialize the control flow easily
    if (w != NULL) {
        // if the JIT is ready, we'll just use this, ideally
        // we switch to directly calling the JIT soon
        int r = jit_try_call(env, w);
        if (r != INTERP_NO_JIT) {
            return r;
        } else {
            env->control_head = 1;
            env->control[0] = (w - words.entries) << 32ull;
        }
    }

    while (env->control_head > 0) recover: {
        // peek the top control
        uint64_t ip = env->control[env->control_head - 1];

        // process next instruction
        w = &words.entries[ip >> 32ull];
        size_t i = ip & 0xFFFFFFFF, len = dyn_array_length(w->ops);
        while (i < len) {
            int64_t x = w->ops[i];

            if (x >= 0) { // trivial literals
                push(env, w->ops[i]), i += 1;
            } else if (x == OP_TAIL) {
                int r = env->single_step ? INTERP_NO_JIT : jit_try_call(env, w);
                if (r != INTERP_NO_JIT) {
                    // once the JIT finishes with a tail call, we just return
                    break;
                } else {
                    i = 0;
                }
            } else if (x <= -1000) {
                Word* new_w = &words.entries[-x - 1000];

                int r = env->single_step ? INTERP_NO_JIT : jit_try_call(env, w);
                if (r != INTERP_NO_JIT) {
                    i += 1;
                } else {
                    // save return continuation
                    env->control[env->control_head - 1] = ((w - words.entries) << 32ull) | (i + 1);

                    // push new continuation
                    env->control[env->control_head++] = (new_w - words.entries) << 32ull;
                    goto recover;
                }
            } else {
                uint64_t tmp;
                switch (x) {
                    case OP_ADD:  tmp = pop(env), push(env, pop(env) + tmp); break;
                    case OP_SUB:  tmp = pop(env), push(env, pop(env) - tmp); break;
                    case OP_MUL:  tmp = pop(env), push(env, pop(env) * tmp); break;
                    case OP_DIV:  tmp = pop(env), push(env, pop(env) / tmp); break;
                    case OP_DUP:  tmp = peek(env), push(env, tmp); break;
                    case OP_DROP: env->head -= 1; break;
                    case OP_READ: {
                        uint64_t* addr = (uint64_t*) pop(env);
                        push(env, *addr);
                        break;
                    }
                    case OP_WRITE: {
                        uint64_t* addr = (uint64_t*) pop(env);
                        uint64_t val = pop(env);
                        *addr = val;
                        break;
                    }
                    case OP_KEY:  push(env, getchar()); break;
                    case OP_EMIT: printf("%c", (char) pop(env)); break;
                    case OP_PRINT: printf("%lld", pop(env)); break;
                    case OP_RECT: {
                        int h = pop(env);
                        int w = pop(env);
                        int y = pop(env);
                        int x = pop(env);
                        draw_rect(0xFFE6E1E5, x, y, w, h);
                        break;
                    }
                    case OP_DUMP: dump(env); break;
                    case OP_IF: {
                        // find matching }else{
                        int depth = 0, brace = i;
                        while (brace < len) {
                            brace += 1;

                            if (w->ops[brace] == OP_IF) depth++;
                            if (w->ops[brace] == OP_CLOSE) depth--;
                            if (w->ops[brace] == OP_ELSE && depth == 0) break;
                        }
                        assert(brace != len && "missing }else{");

                        int x = pop(env);
                        if (x == 0) {
                            i = brace; // jump to else
                        }
                        break;
                    }
                    case OP_ELSE: {
                        // skip to matching closing brace
                        int depth = 1, brace = i;
                        while (brace < len && depth != 0) {
                            brace += 1;

                            if (w->ops[brace] == OP_IF) depth++;
                            if (w->ops[brace] == OP_CLOSE) depth--;
                        }
                        assert(brace != len && "missing }");

                        i = brace;
                        break;
                    }
                    case OP_CLOSE: break; // doesn't really do anything, it's more of a marker
                    default: assert(0 && "TODO"); break;
                }
                i += 1;
            }

            if (env->single_step) {
                // interpreter savepoint
                env->control[env->control_head - 1] = ((w - words.entries) << 32ull) | i;
                return INTERP_STEP;
            }
        }

        // pop
        env->control_head -= 1;
        if (env->single_step) {
            return INTERP_STEP;
        }
    }

    return INTERP_OK;
}

static bool is_opaque = false;

static LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam) {
    switch (message) {
        // force shutdown
        case WM_CLOSE:
        jit.running = false;
        return 1;

        case WM_ACTIVATEAPP: {
            SetLayeredWindowAttributes(hwnd, RGB(0,0,0), is_opaque || wparam ? 255 : 100, LWA_ALPHA);
            break;
        }
    }

    return DefWindowProcA(hwnd, message, wparam, lparam);
}

static HWND gimme_window(int w, int h) {
    WNDCLASSA wc = {
        .style = (CS_HREDRAW | CS_VREDRAW | CS_OWNDC),
        .lpfnWndProc = WndProc,
        .hInstance = GetModuleHandle(NULL),
        .hCursor = LoadCursor(NULL, IDC_ARROW),
        .lpszClassName = "WindowClass",
    };

    if (!RegisterClassA(&wc)) {
        printf("RegisterClassA failed!");
        abort();
    }

    const DWORD style = WS_OVERLAPPEDWINDOW;
    const DWORD ex_style = WS_EX_APPWINDOW | (is_opaque ? 0 : WS_EX_TOPMOST | WS_EX_LAYERED);

    // Get the size of the border.
    RECT border_rect = { 0 };
    AdjustWindowRectEx(&border_rect, style, false, ex_style);

    RECT monitor_rect;
    GetClientRect(GetDesktopWindow(), &monitor_rect);

    int window_x = (monitor_rect.right / 2) - (w / 2);
    int window_y = (monitor_rect.bottom / 2) - (h / 2);
    int window_w = w;
    int window_h = h;

    // Border rectangle in this case is negative.
    window_x += border_rect.left;
    window_y += border_rect.top;

    // Grow the window size by the OS border. This makes the client width/height correct.
    window_w += border_rect.right - border_rect.left;
    window_h += border_rect.bottom - border_rect.top;

    HWND wnd = CreateWindowExA(
        ex_style, wc.lpszClassName, "NeGate's forth", style,
        window_x, window_y, window_w, window_h,
        0, 0, wc.hInstance, 0
    );

    if (wnd == NULL) {
        printf("CreateWindowExA failed!");
        return NULL;
    }

    return wnd;
}

static void set_color(void fn(float, float, float, float), uint32_t color) {
    fn(
        ((color >> 16) & 0xFF) / 255.0f,  ((color >> 8)  & 0xFF) / 255.0f,
        ((color >> 0)  & 0xFF) / 255.0f, ((color >> 24) & 0xFF) / 255.0f
    );
}

static void draw_rect(uint32_t color, float x, float y, float w, float h) {
    float x1 = x + w;
    float y1 = y + h;

    glBegin(GL_QUADS);
    set_color(glColor4f, color);
    glVertex2f(x, y),   glVertex2f(x1, y);
    glVertex2f(x1, y1), glVertex2f(x, y1);
    glEnd();
}

static WordIndex load_forth(Dictionary* dict, const char* path) {
    static char* buffer;
    if (buffer != NULL) {
        cuik_free(buffer);
    }

    Cuik_File* f = cuikfs_open(path, false);

    size_t len;
    if (!cuikfs_get_length(f, &len)) {
        printf("error: bad file!\n");
        return -1;
    }

    buffer = cuik_malloc(len + 1);
    cuikfs_read(f, buffer, len);
    buffer[len] = 0;

    cuikfs_close(f);

    // parse
    WordIndex i = alloc_word();
    Word* root = &words.entries[i];
    *root = (Word){
        .refs = 1,
        .name = { 6, (const uint8_t*) "<root>" }
    };

    parse(dict, &(Parser){ .source = buffer }, root);
    infer(root);
    return -1000 - i;
}

int main(int argc, const char** argv) {
    is_main_thread = true;

    cuik_init_terminal();
    cuik_init_timer_system();
    cuikperf_start("forth.spall", &spall_profiler, false);

    thrd_t jit_thread;
    if (thrd_create(&jit_thread, jit_thread_routine, NULL) != thrd_success) {
        fprintf(stderr, "error: could not create JIT thread");
        return EXIT_FAILURE;
    }

    HWND wnd = gimme_window(1600, 900);
    ShowWindow(wnd, SW_SHOW);
    SetFocus(wnd);

    // create GL context
    HDC dc = GetDC(wnd);

    PIXELFORMATDESCRIPTOR pfd = {
        .nSize = sizeof(pfd),
        .nVersion = 1,
        .iPixelType = PFD_TYPE_RGBA,
        .dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,
        .cColorBits = 32,
        .cAlphaBits = 8,
        .iLayerType = PFD_MAIN_PLANE
    };

    int pixel_format;
    CHECK(pixel_format = ChoosePixelFormat(dc, &pfd));
    CHECK(SetPixelFormat(dc, pixel_format, &pfd));

    HGLRC glctx;
    CHECK(glctx = wglCreateContext(dc));
    CHECK(wglMakeCurrent(dc, glctx));

    set_color(glClearColor, 0xFF1C1B1F);

    Dictionary sandbox = { };
    LiveCompiler live = { 0 };

    // initialize dictionary
    init_word_pool();
    dict_put(&root_dict, 1, "+",      OP_ADD);
    dict_put(&root_dict, 1, "-",      OP_SUB);
    dict_put(&root_dict, 1, "*",      OP_MUL);
    dict_put(&root_dict, 1, "/",      OP_DIV);
    dict_put(&root_dict, 1, ".",      OP_PRINT);
    dict_put(&root_dict, 3, "dup",    OP_DUP);
    dict_put(&root_dict, 4, "drop",   OP_DROP);
    dict_put(&root_dict, 4, "emit",   OP_EMIT);
    dict_put(&root_dict, 4, "rect",   OP_RECT);
    dict_put(&root_dict, 4, "dump",   OP_DUMP);
    dict_put(&root_dict, 3, "key",    OP_KEY);
    dict_put(&root_dict, 3, "if{",    OP_IF);
    dict_put(&root_dict, 6, "}else{", OP_ELSE);
    dict_put(&root_dict, 1, "}",      OP_CLOSE);
    dict_put(&root_dict, 4, "tail",   OP_TAIL);

    Env env;
    while (jit.running) CUIK_TIMED_BLOCK("main loop") {
        // Win32 message pump
        MSG message;
        while (PeekMessage(&message, 0, 0, 0, PM_REMOVE)) {
            if (message.message == WM_QUIT) goto exit_program;

            TranslateMessage(&message);
            DispatchMessage(&message);
        }

        // check hot reload
        if (live_compile_watch(&live, "rect.forth")) {
            dict_free(&sandbox);
            sandbox.parent = &root_dict;

            CUIK_TIMED_BLOCK("load code") {
                WordIndex root_word = load_forth(&sandbox, "rect.forth");
                assert(root_word < 0);

                env.head = 0;
                env.single_step = false;
                interp(&env, &words.entries[-1000 - root_word]);
            }
        }

        RECT rect;
        GetWindowRect(wnd, &rect);
        int w = rect.right - rect.left, h = rect.bottom - rect.top;

        glViewport(0, 0, w, h);
        glClear(GL_COLOR_BUFFER_BIT);

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(0, w, 0, h, -1, 1);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();

        // per frame logic
        WordIndex update_word = dict_get(&sandbox, -1, "update");
        if (update_word < 0) {
            push(&env, w);
            push(&env, h);
            interp(&env, &words.entries[-1000 - update_word]);
            assert(env.head == 0 && "stack \"leak\"?");
        }

        SwapBuffers(dc);
    }

    exit_program:
    // wait for JIT thread to stop
    jit.running = false;
    thrd_join(&jit_thread, NULL);
    cuikperf_stop();

    return 0;
}
