#pragma once
#include <stdatomic.h>
#include "arena.h"
#include "atoms.h"
#include "big_array.h"
#include "common.h"
#include "diagnostic.h"
#include "memory.h"
#include "settings.h"
#include <cuik.h>
#include <preproc/lexer.h>
#include <threads.h>

#include <tb.h>

// the Cuik AST types are all exposed to the public interface
#define NL_STRING_MAP_INLINE
#include <cuik_ast.h>
#include <string_map.h>

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

struct TranslationUnit {
    // circular references amirite...
    struct CompilationUnit* parent;
    // chain of TUs for the compilation unit
    struct TranslationUnit* next;

    void* user_data;
    atomic_int ref_count;

    TB_Module* ir_mod;
    const char* filepath;
    Cuik_ErrorStatus* errors;

    // token stream
    TokenStream tokens;
    atomic_int id_gen;

    // common settings
    bool is_windows_long;

    Cuik_Entrypoint entrypoint_status;
    Cuik_Target target;

    mtx_t arena_mutex;
    Arena ast_arena;
    Arena type_arena;

    // stb_ds array
    // NOTE(NeGate): should this be an stb_ds array?
    Stmt** top_level_stmts;

    // parser state
    NL_Strmap(Cuik_Type*) global_tags;
    NL_Strmap(Symbol) global_symbols;
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
Cuik_Type* new_qualified_type(TranslationUnit* tu, Cuik_Type* base, int align, bool is_atomic, bool is_const);
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
bool const_eval_try_offsetof_hack(TranslationUnit* tu, const Expr* e, uint64_t* out);

Expr* cuik__optimize_ast(TranslationUnit* tu, Expr* e);
// if thread_pool is NULL, the semantics are done single threaded
void cuik__sema_pass(TranslationUnit* restrict tu, Cuik_IThreadpool* restrict thread_pool);
void cuik__function_analysis(TranslationUnit* restrict tu, Stmt* restrict s);
