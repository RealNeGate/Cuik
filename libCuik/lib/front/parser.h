#pragma once
#include <cuik.h>
#include <cuik_ast.h>
#include <threads.h>
#include <stdatomic.h>
#include <dyn_array.h>
#include <inttypes.h>

#ifdef CUIK_USE_TB
#include <tb.h>
#endif

#include "atoms.h"
#include <common.h>
#include "../diagnostic.h"
#include "../preproc/lexer.h"

#define MAX_LOCAL_SYMBOLS (1 << 20)
#define MAX_LOCAL_TAGS (1 << 16)

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

typedef struct Cuik_TypeTable {
    Cuik_Target* target;
    Arena* arena;
    DynArray(Cuik_Type*) tracked;
} Cuik_TypeTable;

struct TranslationUnit {
    // circular references amirite...
    struct CompilationUnit* parent;
    // chain of TUs for the compilation unit
    struct TranslationUnit* next;

    Arena* arena;
    void* user_data;
    bool is_free;

    #ifdef CUIK_USE_TB
    TB_Module* ir_mod;
    #endif

    Cuik_Version version;

    const char* filepath;
    Cuik_Target* target;
    const Cuik_Warnings* warnings;
    Cuik_ImportRequest* import_libs;

    // token stream
    TokenStream tokens;

    // common settings
    bool has_tb_debug_info;
    Cuik_Entrypoint entrypoint_status;

    // DynArray(Stmt*)
    Stmt** top_level_stmts;

    mtx_t diag_mutex;
    NL_Strmap(Diag_UnresolvedSymbol*) unresolved_symbols;

    Cuik_Type* va_list;
    struct {
        Stmt* va_arg_gp;
        Stmt* va_arg_fp;
        Stmt* va_arg_mem;
    } sysv_abi;

    Cuik_TypeTable types;
};

struct CompilationUnit {
    mtx_t lock;
    size_t count;

    #ifdef CUIK_USE_TB
    TB_Module* ir_mod;
    NL_Strmap(TB_Symbol*) export_table;
    #endif

    // linked list of all TUs referenced
    TranslationUnit* head;
    TranslationUnit* tail;
};

extern Cuik_Type cuik__builtin_void;
extern Cuik_Type cuik__builtin_bool;
extern Cuik_Type cuik__builtin_float;
extern Cuik_Type cuik__builtin_double;

Cuik_TypeTable init_type_table(Cuik_Target* target);
void free_type_table(Cuik_TypeTable* types);

Cuik_Type* new_aligned_type(Cuik_TypeTable* types, Cuik_Type* base);
Cuik_Type* get_common_type(Cuik_TypeTable* types, Cuik_Type* ty1, Cuik_Type* ty2);
bool type_equal(Cuik_Type* a, Cuik_Type* b);

#define new_pointer(tu, base) cuik__new_pointer(&tu->types, base)

// TODO(NeGate): merge these soon
#define qual_type_as_string(max_len, buffer, type) type_as_string(max_len, buffer, cuik_canonical_type(type))
size_t type_as_string(size_t max_len, char* buffer, Cuik_Type* type);

void type_layout2(Cuik_Parser* restrict parser, TokenStream* restrict tokens, Cuik_Type* type);
