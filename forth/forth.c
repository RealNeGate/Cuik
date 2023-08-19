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

#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

#pragma comment(lib, "opengl32.lib")

// no need for do while crap if it's an expression :p
// it's basically an assert but it doesn't get vaporized
#define CHECK(x) ((x) ? (void)0 : abort())

enum {
    // arithmatic
    OP_ADD = -1,
    OP_SUB = -2,
    OP_MUL = -3,
    OP_DIV = -4,

    // stack
    OP_DUP  = -5,
    OP_DROP = -6,
    OP_SWAP = -7,

    // memory
    OP_READ  = -8,
    OP_WRITE = -9,

    // control flow
    OP_IF    = -10, // if{
    OP_ELSE  = -11, // }else{
    OP_CLOSE = -12, // }

    OP_TAIL  = -13, // no name, generated automatically

    // debug
    OP_DUMP  = -14,

    // console
    OP_EMIT  = -15,
    OP_PRINT = -16,
    OP_KEY   = -17,

    // graphics
    OP_RECT  = -18, // ( x y w h -- )
};

enum {
    WORD_NORMAL,
    WORD_VAR,
    WORD_TYPE,
};

typedef struct Dictionary Dictionary;
typedef struct {
    // we don't want to free the word while the
    // JIT is using that memory
    _Atomic int refs;

    NL_Slice name;
    DynArray(int64_t) ops;

    Dictionary* as_type;

    // type
    int arity, outputs;
    int tails;

    uint32_t kind;

    // we might wanna move this out of here to avoid false-sharing
    //
    // for now there's no tiers or recompilation.
    //
    // so once 'trip_count' == JIT_THRESHOLD, we submit
    // to the JIT thread. once it's ready later we can
    // check 'jitted'
    _Atomic int trip_count;
    _Atomic(void*) jitted;
} Word;

static _Thread_local bool is_main_thread;

#include "forth_dict.h"

static Dictionary root_dict;

static void dump_type(Word* w) {
    printf(": %.*s ( ", (int) w->name.length, w->name.data);
    for (int i = 0; i < w->arity; i++) {
        printf("%c ", 'a'+i);
    }
    printf("-- ");
    for (int i = 0; i < w->outputs; i++) {
        printf("%c ", 'A'+i);
    }
    printf(") ... ;\n");
}

////////////////////////////////
// Parser
////////////////////////////////
static void infer(Word* w);

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

    dump_type(w);
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
static void parse(Dictionary* dict, Parser* p, Word* w, bool in_record) {
    for (;;) {
        NL_Slice token = lex(p);

        if (token.length == 0) {
            break;
        } else if (token.length == 1 && token.data[0] == ':') {
            // define word
            NL_Slice name = dup_slice(lex(p));

            WordIndex id = alloc_word();
            Word* w = &words.entries[id];
            *w = (Word){ 0 };

            dict_put(dict, name.length, (const char*) name.data, -1000 - id);

            // build up word inside root word
            Word kid = { .refs = 1, .name = name };
            parse(dict, p, &kid, false);

            // split from the parse function to avoid resize problems
            kid.name = name;
            *w = kid;

            infer(w);
        } else if (token.length == 7 && memcmp(token.data, "record:", 7) == 0) {
            NL_Slice name = dup_slice(lex(p));

            WordIndex id = alloc_word();
            Word* w = &words.entries[id];
            *w = (Word){ .refs = 1, .name = name, .kind = WORD_TYPE };

            dict_put(dict, name.length, (const char*) name.data, -1000 - id);

            // build up word within separate dictionary
            Dictionary* record_dict = cuik_calloc(1, sizeof(Dictionary));

            Word kid = { 0 };
            parse(record_dict, p, &kid, true);

            // split from the parse function to avoid resize problems
            kid.name = name;
            *w = kid;

            infer(w);
        } else if (token.length == 4 && memcmp(token.data, "var:", 4) == 0) {
            // define variable
            token = lex(p);
            while (token.length != 1 || token.data[0] != ';') {
                NL_Slice name = dup_slice(token);
                token = lex(p);

                // allocate some memory for it
                if (tb_arena_is_empty(&dict->data)) {
                    tb_arena_create(&dict->data, TB_ARENA_MEDIUM_CHUNK_SIZE);
                }

                void* ptr = tb_arena_alloc(&dict->data, sizeof(int64_t));
                memset(ptr, 0, sizeof(int64_t));

                // generate words
                if (in_record) {
                    // the accessor words in a record are relative to a pointer
                    dict->field_count += 1;
                    size_t offset = dict->data_size;
                    dict->data_size += sizeof(int64_t);

                    Word* st_word = add_new_to_dictionary(dict, slice_fmt(&dict->data, ".%s!", name.data), 2, 0);
                    dyn_array_put(st_word->ops, offset);
                    dyn_array_put(st_word->ops, OP_ADD);
                    dyn_array_put(st_word->ops, OP_WRITE);

                    Word* ld_word = add_new_to_dictionary(dict, slice_fmt(&dict->data, ".%s@", name.data), 1, 1);
                    dyn_array_put(ld_word->ops, offset);
                    dyn_array_put(st_word->ops, OP_ADD);
                    dyn_array_put(ld_word->ops, OP_READ);
                    ld_word->kind = WORD_VAR;
                } else {
                    Word* st_word = add_new_to_dictionary(dict, slice_fmt(&dict->data, "%s!", name.data), 1, 0);
                    dyn_array_put(st_word->ops, (uintptr_t) ptr);
                    dyn_array_put(st_word->ops, OP_WRITE);

                    Word* ld_word = add_new_to_dictionary(dict, slice_fmt(&dict->data, "%s@", name.data), 0, 1);
                    dyn_array_put(ld_word->ops, (uintptr_t) ptr);
                    dyn_array_put(ld_word->ops, OP_READ);
                    ld_word->kind = WORD_VAR;
                }
            }

            if (token.length != 1 || token.data[0] != ';') {
                printf("error: missing semicolon after var: decl\n");
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
        case OP_SWAP:
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
                case OP_SWAP: head += 2; break;

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

    dump_type(w);
}

static void draw_rect2(uint32_t color, float x, float y, float w, float h);

#include "forth_interp.h"
#include "forth_jit.h"

static bool is_opaque = true;

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

////////////////////////////////
// UI
////////////////////////////////
// https://halt.software/dead-simple-layouts/
typedef struct {
    float minx, miny, maxx, maxy;
} Rect;

Rect cut_inset(Rect rect, float a) {
    return (Rect){ rect.minx + a, rect.miny + a, rect.maxx - a, rect.maxy - a };
}

Rect cut_left(Rect* rect, float a) {
    float minx = rect->minx;
    rect->minx = min(rect->maxx, rect->minx + a);
    return (Rect){ minx, rect->miny, rect->minx, rect->maxy };
}

Rect cut_right(Rect* rect, float a) {
    float maxx = rect->maxx;
    rect->maxx = max(rect->minx, rect->maxx - a);
    return (Rect){ rect->maxx, rect->miny, maxx, rect->maxy };
}

Rect cut_top(Rect* rect, float a) {
    float miny = rect->miny;
    rect->miny = min(rect->maxy, rect->miny + a);
    return (Rect){ rect->minx, miny, rect->maxx, rect->miny };
}

Rect cut_bottom(Rect* rect, float a) {
    float maxy = rect->maxy;
    rect->maxy = max(rect->miny, rect->maxy - a);
    return (Rect){ rect->minx, rect->maxy, rect->maxx, maxy };
}

// Shitty font setup
static float font_size;
static unsigned char ttf_buffer[1<<20];
static unsigned char temp_bitmap[512*512];
static stbtt_bakedchar cdata[96]; // ASCII 32..126 is 95 glyphs
static GLuint ftex;

static void set_color(void fn(float, float, float, float), uint32_t color) {
    fn(
        ((color >> 16) & 0xFF) / 255.0f,  ((color >> 8)  & 0xFF) / 255.0f,
        ((color >> 0)  & 0xFF) / 255.0f, ((color >> 24) & 0xFF) / 255.0f
    );
}

static void draw_rect2(uint32_t color, float x, float y, float w, float h) {
    float x1 = x + w;
    float y1 = y + h;

    glBegin(GL_QUADS);
    set_color(glColor4f, color);
    glVertex2f(x, y),   glVertex2f(x1, y);
    glVertex2f(x1, y1), glVertex2f(x, y1);
    glEnd();
}

static void draw_rect(uint32_t color, Rect r) {
    glBegin(GL_QUADS);
    set_color(glColor4f, color);
    glVertex2f(r.minx, r.miny), glVertex2f(r.maxx, r.miny);
    glVertex2f(r.maxx, r.maxy), glVertex2f(r.minx, r.maxy);
    glEnd();
}

static void draw_text(uint32_t color, float x, float y, const char* text) {
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture(GL_TEXTURE_2D, ftex);

    glBegin(GL_QUADS);
    set_color(glColor4f, color);

    float xx = x, yy = 0.0f;
    for (; *text; text++) {
        if (*text >= 32) {
            stbtt_aligned_quad q;
            stbtt_GetBakedQuad(cdata, 512, 512, *text - 32, &xx, &yy, &q, 0);

            q.x0 = floorf(q.x0);
            q.x1 = floorf(q.x1);

            glTexCoord2f(q.s0,q.t0); glVertex2f(q.x0,y-q.y0);
            glTexCoord2f(q.s1,q.t0); glVertex2f(q.x1,y-q.y0);
            glTexCoord2f(q.s1,q.t1); glVertex2f(q.x1,y-q.y1);
            glTexCoord2f(q.s0,q.t1); glVertex2f(q.x0,y-q.y1);
        }
    }
    glEnd();

    glBindTexture(GL_TEXTURE_2D, 0);
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
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

    parse(dict, &(Parser){ .source = buffer }, root, false);
    infer(root);
    return -1000 - i;
}

int main(int argc, const char** argv) {
    is_main_thread = true;
    SetProcessDPIAware();

    cuik_init_terminal();
    cuik_init_timer_system();
    cuikperf_start("forth.spall", &spall_profiler, false);

    thrd_t jit_thread;
    if (thrd_create(&jit_thread, jit_thread_routine, NULL) != thrd_success) {
        fprintf(stderr, "error: could not create JIT thread");
        return EXIT_FAILURE;
    }

    HWND wnd = gimme_window(3200, 1600);
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

    Dictionary sandbox[2] = { 0 };
    int curr = 0; // which of the sandboxes we're in

    LiveCompiler live = { 0 };

    // initialize dictionary
    init_word_pool();
    dict_put(&root_dict, 1, "+",         OP_ADD);
    dict_put(&root_dict, 1, "-",         OP_SUB);
    dict_put(&root_dict, 1, "*",         OP_MUL);
    dict_put(&root_dict, 1, "/",         OP_DIV);
    dict_put(&root_dict, 1, ".",         OP_PRINT);
    dict_put(&root_dict, 3, "dup",       OP_DUP);
    dict_put(&root_dict, 4, "drop",      OP_DROP);
    dict_put(&root_dict, 4, "swap",      OP_SWAP);
    dict_put(&root_dict, 4, "emit",      OP_EMIT);
    dict_put(&root_dict, 4, "dump",      OP_DUMP);
    dict_put(&root_dict, 3, "key",       OP_KEY);
    dict_put(&root_dict, 3, "if{",       OP_IF);
    dict_put(&root_dict, 6, "}else{",    OP_ELSE);
    dict_put(&root_dict, 1, "}",         OP_CLOSE);
    dict_put(&root_dict, 4, "tail",      OP_TAIL);
    dict_put(&root_dict, 9, "draw-rect", OP_RECT);

    {
        font_size = 36.0;

        FILE* f = fopen("C:\\Windows\\Fonts\\consola.ttf", "rb");
        fread(ttf_buffer, 1, 1<<20, f);
        stbtt_BakeFontBitmap(ttf_buffer,0, font_size, temp_bitmap,512,512, 32,96, cdata); // no guarantee this fits!
        // can free ttf_buffer at this point
        glGenTextures(1, &ftex);
        glBindTexture(GL_TEXTURE_2D, ftex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, 512,512, 0, GL_ALPHA, GL_UNSIGNED_BYTE, temp_bitmap);
        // can free temp_bitmap at this point
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glBindTexture(GL_TEXTURE_2D, 0);
        fclose(f);
    }

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
            CUIK_TIMED_BLOCK("load code") {
                int old = curr;
                curr = (curr + 1) % 2;

                sandbox[curr].parent = &root_dict;
                WordIndex root_word = load_forth(&sandbox[curr], "rect.forth");
                assert(root_word < 0);

                // move any old values which match in name to new values
                dict_migrate(&sandbox[curr], &sandbox[old]);
                dict_free(&sandbox[old]);

                // run init code (maybe we shouldn't do this multiple times?)
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
        WordIndex update_word = dict_get(&sandbox[curr], -1, "update");
        if (update_word < 0) {
            push(&env, w);
            push(&env, h);
            interp(&env, &words.entries[-1000 - update_word]);
            assert(env.head == 0 && "stack \"leak\"?");
        }

        /*Rect window = { 0, 0, w, h };
        {
            Rect code_panel = cut_left(&window, window.maxx / 3.0f);
            draw_rect(0xFF332E44, code_panel);
        }*/

        SwapBuffers(dc);
    }

    exit_program:
    // wait for JIT thread to stop
    jit.running = false;
    thrd_join(&jit_thread, NULL);
    cuikperf_stop();

    return 0;
}
