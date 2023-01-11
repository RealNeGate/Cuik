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
    int token_start, token_end;
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

// Global symbols are handled using a string maps because they generally
// get huge enough to warrant it, for local symbols we do linear searches
// since it makes it easier to represent stack frames and lookup.
struct Cuik_GlobalSymbols {
    NL_Strmap(Cuik_Type*) tags;
    NL_Strmap(Symbol) symbols;
};

typedef struct Cuik_TypeTableSegment Cuik_TypeTableSegment;
struct Cuik_TypeTableSegment {
    size_t count;
    Cuik_TypeTableSegment* next;
    Cuik_Type _[];
};

enum {
    CUIK_TYPE_TABLE_SEGMENT_COUNT = ((16 * 1024 * 1024) - sizeof(Cuik_TypeTableSegment)) / sizeof(Cuik_Type)
};

typedef struct Cuik_TypeTable {
    // This is where we allocate all our Cuik_Types, well in theory they can
    // be allocated anywhere but the parser will use the arena for performance
    // and maintainability reasons.
    mtx_t mutex;
    Cuik_TypeTableSegment* base;
    Cuik_TypeTableSegment* top;
} Cuik_TypeTable;

#define CUIK_FOR_TYPES(it, types) \
for (Cuik_TypeTableSegment* _a_ = (types).base; _a_ != NULL; _a_ = _a_->next) \
for (Cuik_Type *it = _a_->_, *_end_ = &it[_a_->count]; it != _end_; it++)

typedef struct Cuik_Parser Cuik_Parser;

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
    Cuik_Target* target;
    const Cuik_Warnings* warnings;
    Cuik_ImportRequest* import_libs;

    // token stream
    TokenStream tokens;
    atomic_int id_gen;

    // common settings
    bool has_tb_debug_info;

    Cuik_Entrypoint entrypoint_status;

    Arena ast_arena;

    // DynArray(Stmt*)
    Stmt** top_level_stmts;

    mtx_t arena_mutex;
    mtx_t diag_mutex;
    NL_Strmap(Diag_UnresolvedSymbol*) unresolved_symbols;

    Cuik_TypeTable types;
    Cuik_GlobalSymbols globals;
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

Cuik_TypeTable init_type_table(void);
void free_type_table(Cuik_TypeTable* types);

Cuik_Type* new_aligned_type(Cuik_TypeTable* types, Cuik_Type* base);
Cuik_Type* get_common_type(Cuik_TypeTable* types, Cuik_Type* ty1, Cuik_Type* ty2);
bool type_equal(Cuik_Type* a, Cuik_Type* b);

Cuik_Type* cuik__new_blank_type(Cuik_TypeTable* types);
Cuik_Type* cuik__new_enum(Cuik_TypeTable* types);
Cuik_Type* cuik__new_typeof(Cuik_TypeTable* types, Expr* src);
Cuik_Type* cuik__new_record(Cuik_TypeTable* types, bool is_union);
Cuik_Type* cuik__new_vector(Cuik_TypeTable* types, Cuik_QualType base, int count);
Cuik_Type* cuik__new_func(Cuik_TypeTable* types);
Cuik_Type* cuik__new_pointer(Cuik_TypeTable* types, Cuik_QualType base);
Cuik_Type* cuik__new_array(Cuik_TypeTable* types, Cuik_QualType base, int count);

#define new_pointer(tu, base) cuik__new_pointer(&tu->types, base)

// TODO(NeGate): merge these soon
#define qual_type_as_string(max_len, buffer, type) type_as_string(max_len, buffer, cuik_canonical_type(type))
size_t type_as_string(size_t max_len, char* buffer, Cuik_Type* type);

// is needs_complete is false then the size and alignment don't need to be non-zero
void type_layout(TranslationUnit* restrict tu, Cuik_Type* type, bool needs_complete);
void type_layout2(Cuik_Parser* restrict parser, Cuik_Type* type, bool needs_complete);

bool const_eval_try_offsetof_hack(TranslationUnit* tu, const Expr* e, uint64_t* out);

Expr* cuik__optimize_ast(TranslationUnit* tu, Expr* e);
void cuik__function_analysis(TranslationUnit* restrict tu, Stmt* restrict s);
