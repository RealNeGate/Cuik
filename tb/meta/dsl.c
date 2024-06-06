#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <inttypes.h>
#include <stdatomic.h>
#include <sys/stat.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat64
#define stat _stat64
#endif

#define FOR_N(i, start, end) for (ptrdiff_t i = start, _end_ = (end); i < _end_; i++)

static char* read_entire_file(const char* filepath, size_t* out_length) {
    FILE* file = fopen(filepath, "rb");
    if (file == NULL) {
        return NULL;
    }
    int descriptor = fileno(file);

    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        return NULL;
    }

    size_t length = file_stats.st_size;
    char* data = malloc(length + 1);

    fseek(file, 0, SEEK_SET);
    size_t length_read = fread(data, 1, length, file);
    data[length_read] = '\0';
    fclose(file);

    if (out_length) {
        *out_length = length_read;
    }
    return data;
}

enum {
    // primitive types
    VAL_REF,  // ptr to (car: Val, cdr: Val)
    VAL_STR,  // ptr to (car: i32, cdr: nil, data: []char)
    VAL_SYM,  // ptr to (car: i32, cdr: nil, data: []char)
    VAL_I32,  // int32_t
};

typedef struct {
    uintptr_t ptr : 48;
    uintptr_t tag : 16;
} Val;

typedef struct {
    Val car, cdr;
    char data[];
} Obj;

static Obj* NIL;
static Val pack_i32(uint32_t i)  { return (Val){ .ptr = i, .tag = VAL_I32 }; }

static bool val_eq(Val a, Val b) { return memcmp(&a, &b, sizeof(a)) == 0; }
static Obj* val_unpack(Val ref) { return (Obj*) ref.ptr; }
static Val  val_pack(void* o, int tag) { return (Val){ .ptr = (uintptr_t) o, .tag = tag }; }

static void* gc_new(size_t size) {
    enum { MEMORY_SIZE = 64*1024*1024 };

    static char* memory;
    static size_t memory_used;

    if (memory == NULL) {
        memory = VirtualAlloc(NULL, MEMORY_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    }

    // align all objects to 8bytes
    size = (size + 7) & ~7;

    size_t pos = memory_used;
    if (pos + size >= MEMORY_SIZE) {
        abort();
    }

    char* ptr = &memory[pos];
    memset(ptr, 0, size);
    memory_used += size;
    return ptr;
}

static Val gc_cons(Val car, Val cdr) {
    Obj* o = gc_new(sizeof(Obj));
    o->car = car;
    o->cdr = cdr;
    return val_pack(o, VAL_REF);
}

static Val gc_new_str(int tag, size_t len, const char* str, Val cdr) {
    Obj* o = gc_new(sizeof(Obj) + len + 1);
    o->car = pack_i32(len);
    o->cdr = cdr;
    memcpy(o->data, str, len);
    o->data[len] = 0;
    return val_pack(o, tag);
}

////////////////////////////////
// Symbol interning
////////////////////////////////
static Val sym_list;

// common symbols
static Val s_do, s_quote, s_defnode, s_let, s_if, s_lambda;

static Val sym_intern(size_t len, const char* str) {
    Obj* o = val_unpack(sym_list);
    while (o != NIL) {
        assert(o->car.tag == VAL_SYM);
        Obj* o_str = val_unpack(o->car);

        assert(o_str->car.tag == VAL_I32);
        if (o_str->car.ptr == len && memcmp(o_str->data, str, len) == 0) {
            return o->car;
        }
        o = val_unpack(o->cdr);
    }

    Val newstr = gc_new_str(VAL_SYM, len, str, sym_list);
    sym_list = gc_cons(newstr, sym_list);
    return newstr;
}

////////////////////////////////
// S-expression parser
////////////////////////////////
static bool ws(int ch)   { return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'; }
static bool num(int ch)  { return ch >= '0' && ch <= '9'; }
static bool atom(int ch) { return ch > 32 && ch != '(' && ch != ')'; }
static const char* parse(const char* str, Val* out_val) {
    Obj *list = NULL, *tail = NULL;

    while (*str) {
        loop: {
            // skip ws
            if (ws(*str)) {
                do { str++; } while (ws(*str));
                goto loop;
            }
            // skip comments
            if (*str == ';') {
                do { str++; } while (*str && *str != '\n');
                goto loop;
            }
        }

        Val v;
        int c = *str;
        if (c == 0) { break; }
        else if (c == '(') { str = parse(str + 1, &v); }
        else if (c == ')') { str++; break; }
        else if (num(c) || (c == '-' && num(str[1]))) {
            bool neg = false;
            if (c == '-') {
                neg = true, str++;
            }

            int n = 0;
            while (num(*str)) { n *= 10, n += (*str++ - '0'); }

            v = pack_i32(neg ? -n : n);
        } else if (atom(c)) {
            const char* start = str;
            while (atom(*str)) { str++; }
            v = sym_intern(str - start, start);
        }

        // append to list
        Obj* node = gc_new(sizeof(Obj));
        node->car = v;
        node->cdr = val_pack(NIL, VAL_REF);
        if (tail) {
            tail->cdr = val_pack(node, VAL_REF);
            tail = node;
        } else {
            list = tail = node;
        }
    }

    *out_val = val_pack(list, VAL_REF);
    return str;
}

static void print(Val v) {
    if (v.tag == VAL_REF) {
        Obj* o = val_unpack(v);

        printf("(");
        bool f = true;
        while (o != NIL) {
            if (f) { f = false; } else { printf(" "); }
            print(o->car);
            o = val_unpack(o->cdr);
        }
        printf(")");
    } else if (v.tag == VAL_I32) {
        printf("%d", (int32_t) v.ptr);
    } else if (v.tag == VAL_SYM) {
        Obj* o = val_unpack(v);
        printf("%.*s", (int) o->car.ptr, o->data);
    } else if (v.tag == VAL_STR) {
        Obj* o = val_unpack(v);
        printf("'%.*s'", (int) o->car.ptr, o->data);
    }
}

////////////////////////////////
// Le Compiler
////////////////////////////////
static const char* get_keyword(Val v) {
    if (v.tag != VAL_SYM) {
        return NULL;
    }

    Obj* o = val_unpack(v);
    return o->data[0] == ':' ? &o->data[1] : NULL;
}

static const char* get_symbol(Val v) {
    return v.tag == VAL_SYM ? val_unpack(v)->data : NULL;
}

static void compile_type(Val v) {
    if (v.tag == VAL_SYM) {
        const char* str = val_unpack(v)->data;
        if (strcmp(str, "Node") == 0) {
            printf("TB_Node*");
        }
    }
}

static void compile_func(Val v);
static void compile_expr(Val v) {
    Obj* o = val_unpack(v);
    if (val_eq(o->car, s_let)) {
        Val name = val_unpack(o->cdr)->car;
        Val init = val_unpack(val_unpack(o->cdr)->cdr)->car;

        printf("auto ");
        print(name);
        printf(" = ");
        compile_expr(init);
        printf(";\n");

        __debugbreak();
    } else if (val_eq(o->car, s_quote)) {
        // don't evaluate, just refer to the list
        print(o->cdr);
        __debugbreak();
    } else if (val_eq(o->car, s_defnode)) {
        Val name = val_unpack(o->cdr)->car;

        printf("// Node: ");
        print(name);
        printf("\n");

        // parse the rest of the params
        Obj* body = val_unpack(val_unpack(o->cdr)->cdr);
        while (body != NIL) {
            // we're looking for keywords to tell us wtf the next arg means
            const char* key = get_keyword(body->car);
            assert(key);

            body = val_unpack(body->cdr);
            if (strcmp(key, "identity") == 0) {
                compile_func(body->car);
            } else {
                assert(0 && "todo");
            }

            body = val_unpack(body->cdr);
        }

        __debugbreak();
    } else if (v.tag == VAL_SYM) {
        const char* str = o->data;
        __debugbreak();
    } else if (v.tag == VAL_REF) {
        // function calls, builtins, and array accesses
        Val target = o->car;
        print(target);

        bool builtin = false;
        if (target.tag == VAL_SYM) {
            const char* str = val_unpack(target)->data;
            if (strcmp(str, "get-type") == 0) {
                printf("latuni_get(_f_, ");
                builtin = true;
            }
        }

        if (!builtin) {
            compile_expr(target);
            printf("(");
        }

        Obj* args = val_unpack(o->cdr);
        while (args != NIL) {
            compile_expr(args->car);
            args = val_unpack(args->cdr);
        }
        printf(")");
    }
}

static void compile_func(Val v) {
    Obj* o = val_unpack(v);
    if (!val_eq(o->car, s_lambda)) {
        assert(0 && "expected lambda");
    }

    static int lambda_cnt = 0;
    printf("static void lambda%d(TB_Function* _f_", lambda_cnt++);

    Obj* args = val_unpack(val_unpack(o->cdr)->car);
    while (args != NIL) {
        const char* name = get_symbol(val_unpack(args->car)->car);
        Val type = val_unpack(val_unpack(args->car)->cdr)->car;

        printf(", ");
        compile_type(type);
        printf(" %s", name);

        args = val_unpack(args->cdr);
    }

    printf(") {\n");

    Obj* body = val_unpack(val_unpack(val_unpack(o->cdr)->cdr)->car);
    while (body != NIL) {
        print(body->car);
        printf("\n");

        compile_expr(body->car);
        body = val_unpack(body->cdr);
    }

    __debugbreak();
}

static void compile_list(Val v) {
    compile_expr(v);
}

int main() {
    NIL = gc_new(sizeof(Obj));
    NIL->car = val_pack(NIL, VAL_REF);
    NIL->cdr = val_pack(NIL, VAL_REF);
    sym_list = val_pack(NIL, VAL_REF);

    s_do      = sym_intern(2, "do");
    s_quote   = sym_intern(5, "quote");
    s_defnode = sym_intern(7, "defnode");
    s_let     = sym_intern(3, "let");
    s_if      = sym_intern(2, "if");
    s_lambda  = sym_intern(6, "lambda");

    char* source = read_entire_file("tb/meta/cool.lsp", NULL);

    Val v;
    parse(source, &v);
    // parse("(do (+ 2 (* 4 2) (* 2 4)) (eval (quote + 2 1))", &v);

    print(v);
    printf("\n");

    print(sym_list);
    printf("\n");

    compile_list(v);

    __debugbreak();
    return 0;
}

