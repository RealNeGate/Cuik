#pragma once
#include <cuik.h>
#include <cuik_ast.h>
#include <threads.h>
#include <stdatomic.h>
#include <dyn_array.h>

#ifdef CUIK_USE_TB
#include <tb.h>
#endif

#include "atoms.h"
#include <common.h>
#include "../arena.h"
#include "../diagnostic.h"
#include "../preproc/lexer.h"

#define NL_STRING_MAP_IMPL
#define NL_STRING_MAP_INLINE
#include "../string_map.h"

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
    Cuik_QualType type;
    Atom name;
    SourceRange loc;
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
    Cuik_QualType type;
    SourceRange loc;
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

// Variety of parser specific errors which are accumulated to
// make for a cleaner output
enum {
    DIAG_UNRESOLVED_SYMBOL,
};

typedef struct Diag_UnresolvedSymbol {
    struct Diag_UnresolvedSymbol* next;

    Atom name;
    SourceRange loc;
} Diag_UnresolvedSymbol;

struct TranslationUnit {
    // circular references amirite...
    struct CompilationUnit* parent;
    // chain of TUs for the compilation unit
    struct TranslationUnit* next;

    void* user_data;

    #ifdef CUIK_USE_TB
    TB_Module* ir_mod;
    #endif

    const char* filepath;
    const Cuik_Warnings* warnings;
    Cuik_ImportRequest* import_libs;

    // token stream
    TokenStream tokens;
    atomic_int id_gen;

    // common settings
    bool is_windows_long;
    bool has_tb_debug_info;

    Cuik_Entrypoint entrypoint_status;
    Cuik_Target target;

    mtx_t arena_mutex;
    Arena ast_arena;
    Arena type_arena;

    // DynArray(Stmt*)
    Stmt** top_level_stmts;

    mtx_t diag_mutex;
    NL_Strmap(Diag_UnresolvedSymbol*) unresolved_symbols;

    // parser state
    NL_Strmap(Cuik_Type*) global_tags;
    NL_Strmap(Symbol) global_symbols;
};

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

extern Cuik_Type cuik__builtin_void;
extern Cuik_Type cuik__builtin_bool;
extern Cuik_Type cuik__builtin_char;
extern Cuik_Type cuik__builtin_short;
extern Cuik_Type cuik__builtin_int;
extern Cuik_Type cuik__builtin_long;
extern Cuik_Type cuik__builtin_uchar;
extern Cuik_Type cuik__builtin_ushort;
extern Cuik_Type cuik__builtin_uint;
extern Cuik_Type cuik__builtin_ulong;
extern Cuik_Type cuik__builtin_float;
extern Cuik_Type cuik__builtin_double;

Cuik_Type* new_func(TranslationUnit* tu);
Cuik_Type* new_enum(TranslationUnit* tu);
Cuik_Type* new_blank_type(TranslationUnit* tu);
Cuik_Type* new_record(TranslationUnit* tu, bool is_union);
Cuik_Type* new_aligned_type(TranslationUnit* tu, Cuik_Type* base);
Cuik_Type* new_pointer(TranslationUnit* tu, Cuik_QualType base);
Cuik_Type* new_typeof(TranslationUnit* tu, Expr* src);
Cuik_Type* new_array(TranslationUnit* tu, Cuik_QualType base, int count);
Cuik_Type* new_vector(TranslationUnit* tu, Cuik_QualType base, int count);
Cuik_Type* get_common_type(TranslationUnit* tu, Cuik_Type* ty1, Cuik_Type* ty2);
bool type_equal(TranslationUnit* tu, Cuik_Type* a, Cuik_Type* b);

// TODO(NeGate): merge these soon
#define qual_type_as_string(max_len, buffer, type) type_as_string(max_len, buffer, cuik_canonical_type(type))
size_t type_as_string(size_t max_len, char* buffer, Cuik_Type* type);

// is needs_complete is false then the size and alignment don't need to be non-zero
void type_layout(TranslationUnit* restrict tu, Cuik_Type* type, bool needs_complete);

Stmt* resolve_unknown_symbol(TranslationUnit* tu, Expr* e);
bool const_eval_try_offsetof_hack(TranslationUnit* tu, const Expr* e, uint64_t* out);

Expr* cuik__optimize_ast(TranslationUnit* tu, Expr* e);
// if thread_pool is NULL, the semantics are done single threaded
void cuik__sema_pass(TranslationUnit* restrict tu, Cuik_IThreadpool* restrict thread_pool);
void cuik__function_analysis(TranslationUnit* restrict tu, Stmt* restrict s);
