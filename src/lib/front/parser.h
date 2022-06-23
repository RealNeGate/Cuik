#pragma once
#include "arena.h"
#include "atoms.h"
#include "big_array.h"
#include "common.h"
#include "diagnostic.h"
#include "memory.h"
#include "settings.h"
#include <cuik.h>
#include <preproc/lexer.h>
#include <ext/threadpool.h>
#include <ext/threads.h>

#include <tb.h>

// the Cuik AST types are all exposed to the public interface
#include <cuik_ast.h>

#define MAX_LOCAL_SYMBOLS (1 << 20)
#define MAX_LOCAL_TAGS (1 << 16)

typedef struct ConstValue {
    bool is_signed;
    union {
        intmax_t signed_value;
        uintmax_t unsigned_value;
    };
} ConstValue;

typedef struct Decl {
    Cuik_Type* type;
    Atom name;
    SourceLocIndex loc;
} Decl;

typedef enum StorageClass {
    STORAGE_NONE,

    STORAGE_STATIC_FUNC,  // .text
    STORAGE_STATIC_VAR,   // .data
    STORAGE_STATIC_CONST, // .rdata

    STORAGE_FUNC,
    STORAGE_PARAM,
    STORAGE_GLOBAL,
    STORAGE_LOCAL,
    STORAGE_ENUM,
    STORAGE_TYPEDEF
} StorageClass;

typedef struct Symbol {
    Atom name;
    Cuik_Type* type;
    SourceLocIndex loc;
    StorageClass storage_class;

    union {
        // used if storage_class == STORAGE_PARAM
        int param_num;

        // used if storage_class == STORAGE_ENUM, refers to an index in the entries table
        int enum_value;

        Stmt* stmt;
    };

    // when handling global symbols and delaying their parsing
    // we want to be able to store what the position of the symbol's
    // "value" aka function bodies and intitial expressions
    int current;
    int terminator;
} Symbol;

typedef struct {
    Atom key;
    Stmt* value;
} ExportedSymbolEntry;

struct TranslationUnit {
    // circular references amirite...
    struct CompilationUnit* parent;

    TB_Module* ir_mod;
    const char* filepath;

    // token stream
    TokenStream tokens;

    // chain of TUs for the compilation unit
    struct TranslationUnit* next;
    atomic_int id_gen;

    const Cuik_TargetDesc* target_desc;

    mtx_t arena_mutex;
    Arena ast_arena;
    Arena type_arena;

    // stb_ds array
    // NOTE(NeGate): should this be an stb_ds array?
    Stmt** top_level_stmts;
};

// builtin types at the start of the type table
enum {
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_CHAR,
    TYPE_SHORT,
    TYPE_INT,
    TYPE_LONG,
    TYPE_UCHAR,
    TYPE_USHORT,
    TYPE_UINT,
    TYPE_ULONG,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    BUILTIN_TYPE_COUNT,
};
extern Cuik_Type builtin_types[BUILTIN_TYPE_COUNT];

Cuik_Type* new_func(TranslationUnit* tu);
Cuik_Type* new_enum(TranslationUnit* tu);
Cuik_Type* new_blank_type(TranslationUnit* tu);
Cuik_Type* new_record(TranslationUnit* tu, bool is_union);
Cuik_Type* copy_type(TranslationUnit* tu, Cuik_Type* base);
Cuik_Type* new_pointer(TranslationUnit* tu, Cuik_Type* base);
Cuik_Type* new_typeof(TranslationUnit* tu, Expr* src);
Cuik_Type* new_array(TranslationUnit* tu, Cuik_Type* base, int count);
Cuik_Type* new_vector(TranslationUnit* tu, Cuik_Type* base, int count);
Cuik_Type* get_common_type(TranslationUnit* tu, Cuik_Type* ty1, Cuik_Type* ty2);
bool type_equal(TranslationUnit* tu, Cuik_Type* a, Cuik_Type* b);
size_t type_as_string(TranslationUnit* tu, size_t max_len, char* buffer, Cuik_Type* type_index);

void type_layout(TranslationUnit* restrict tu, Cuik_Type* type);

Stmt* resolve_unknown_symbol(TranslationUnit* tu, Expr* e);
ConstValue const_eval(TranslationUnit* tu, const Expr* e);
bool const_eval_try_offsetof_hack(TranslationUnit* tu, const Expr* e, uint64_t* out);

// if thread_pool is NULL, the semantics are done single threaded
void sema_pass(TranslationUnit* restrict tu, Cuik_IThreadpool* restrict thread_pool);
