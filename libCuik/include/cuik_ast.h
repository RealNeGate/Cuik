////////////////////////////////////////////
// Cuik AST interface
////////////////////////////////////////////
// This module handles AST construction, printing, and walking.
//
// AST Overview:
//
//   Cuik_QualType - This refers to a type but also holds qualifiers (const, restrict, volatile)
//
//   Cuik_Type     - This represents all C types, qualified types currently act as a type
//                   as opposed to a view of a type (refactor coming soon)
//
//   Stmt          - statements (declarations count here) generally introduce sequence
//                   points.
//
//   Expr          - all expressions have some type and during type checking a new
//                   "cast type" is produced which represents the potential type
//                   promotion and is the type actually used by the parent node.
//
// TODO:
//
//  * rename all the bare types to use Cuik_
//
//  * implement VLAs
//
//  * add qualifiers to type pretty printer
//
#pragma once
#include "cuik_prelude.h"

typedef char* Atom;
typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Cuik_Type Cuik_Type;

typedef enum Cuik_Qualifiers {
    CUIK_QUAL_CONST    = (1u << 0u),
    CUIK_QUAL_VOLATILE = (1u << 1u),
    CUIK_QUAL_RESTRICT = (1u << 2u),
    CUIK_QUAL_ATOMIC   = (1u << 3u),

    // this is what's masked out when we convert to a pointer
    CUIK_QUAL_FLAG_BITS = 0b1111,
} Cuik_Qualifiers;

// This is a trick stolen from Clang, we store the qualifiers as
// the bottom 4 bits of the pointer value, because of this we need
// to enforce 16 byte alignment
typedef struct Cuik_QualType {
    uintptr_t raw;
} Cuik_QualType;

typedef enum Cuik_TypeKind {
    KIND_VOID,
    KIND_BOOL,
    KIND_CHAR,
    KIND_SHORT,
    KIND_INT,
    KIND_ENUM,
    KIND_LONG,
    KIND_LLONG,
    KIND_FLOAT,
    KIND_DOUBLE,
    KIND_PTR,
    KIND_FUNC,
    KIND_ARRAY,
    KIND_VLA, // variable-length array
    KIND_STRUCT,
    KIND_UNION,
    KIND_VECTOR,

    // used when the type isn't resolved so we shouldn't clone it just yet
    KIND_CLONE,

    // these are inferred as typedefs but don't map to anything yet
    KIND_PLACEHOLDER,

    // weird typeof(expr) type that gets resolved in the semantics pass
    // this is done to enable typeof to work with out of order decls...
    // it's a mess but it's worth it
    KIND_TYPEOF
} Cuik_TypeKind;

typedef struct Cuik_Attribute {
    struct Cuik_Attribute* prev;
    SourceRange loc;

    Atom name;
    // TODO(NeGate): implement parameter list
} Cuik_Attribute;

// Used by unions and structs
typedef struct {
    Cuik_QualType type;
    Atom name;

    SourceRange loc;
    int align;
    int offset;

    // Bitfield
    int bit_offset;
    int bit_width;
    bool is_bitfield;
} Member;

typedef struct {
    bool is_static  : 1;
    bool is_typedef : 1;
    bool is_inline  : 1;
    bool is_extern  : 1;
    bool is_tls     : 1;

    // NOTE(NeGate): In all honesty, this should probably not be
    // here since it's used in cases that aren't relevant to attribs.
    // mainly STMT_DECL, it keeps track of if anyone's referenced it
    // so we can garbage collect the symbol later.
    bool is_root : 1;
    bool is_used : 1;
} Attribs;

typedef struct {
    Atom key;

    // lexer_pos is non-zero if the enum value has it's compilation delayed
    int lexer_pos;
    int value;
} EnumEntry;

typedef struct {
    Cuik_QualType type;
    Atom name;
} Param;

typedef struct {
    Cuik_QualType key;
    Expr* value;
} C11GenericEntry;

struct Cuik_Type {
    Cuik_TypeKind kind;
    int size;  // sizeof
    int align; // _Alignof
    SourceRange loc;

    Atom also_known_as;

    #ifdef CUIK_USE_TB
    TB_DebugType* debug_type;
    #else
    void* user_data;
    #endif

    // used by cycle checking
    bool is_complete : 1;
    bool is_progress : 1;

    union {
        // Integers
        bool is_unsigned;

        // Arrays
        struct {
            Cuik_QualType array_of;
            int array_count;

            // if non-zero, then we must execute an expression
            // parser to resolve it
            int array_count_lexer_pos;
        };

        struct {
            Cuik_Type* of;
        } clone;

        // Pointers
        struct {
            Cuik_QualType ptr_to;
        };

        // Function
        struct {
            Atom name;
            Cuik_QualType return_type;

            size_t param_count;
            Param* param_list;

            bool has_varargs : 1;
        } func;

        // Structs/Unions
        struct Cuik_TypeRecord {
            Atom name;

            int kid_count;
            Member* kids;

            // this is the one used in type comparisons
            Cuik_Type* nominal;
        } record;

        // Enumerators
        struct {
            Atom name;
            int count;

            EnumEntry* entries;
        } enumerator;

        struct {
            int count;
            Cuik_Type* base;
        } vector_;

        // Typeof
        struct {
            Expr* src;
        } typeof_;

        struct {
            Atom name;

            // if non-NULL we've got a linked list to walk :)
            Cuik_Type* next;
        } placeholder;
    };
} __attribute__((aligned(16)));
_Static_assert(_Alignof(Cuik_Type) == 16, "we need 16byte alignment for Cuik_QualType");

// designated initializer member
// .x = 5
// [4] = 6
typedef enum InitNodeDesignator {
    INIT_NONE,
    INIT_ARRAY,
    INIT_MEMBER
} InitNodeDesignator;

typedef struct InitNode {
    // the children are directly after this
    // node, if the value is 0 then there's
    // no children and we are expected to
    // find an expression here.
    int kids_count;
    SourceRange loc;

    // Fully resolved members with a kid_count of 0 will have
    // a proper offset and type after the type checking
    uint32_t offset;
    Cuik_QualType type;

    Expr* expr;
    InitNodeDesignator mode;

    // we're lisp-pilled:
    //   root
    //    |
    //   kid 1 -> kid 2 -> kid 3
    struct InitNode* kid;
    struct InitNode* next;

    union {
        // INIT_MEMBER
        Atom member_name;

        // INIT_ARRAY
        struct {
            int start, count;
        };
    };
} InitNode;

typedef enum StmtOp {
    STMT_NONE,

    STMT_COMPOUND,
    STMT_DECL,

    // It's a normal decl but global
    STMT_GLOBAL_DECL,
    STMT_FUNC_DECL,

    STMT_LABEL,
    STMT_EXPR,

    STMT_IF,
    STMT_DO_WHILE,
    STMT_GOTO,
    STMT_FOR,
    STMT_WHILE,

    STMT_SWITCH,
    STMT_CASE,
    STMT_DEFAULT,

    STMT_BREAK,
    STMT_CONTINUE,

    STMT_RETURN
} StmtOp;

typedef enum ExprOp {
    EXPR_NONE,

    EXPR_INT,
    EXPR_ENUM,
    EXPR_FLOAT32,
    EXPR_FLOAT64,

    EXPR_WCHAR,
    EXPR_CHAR,
    EXPR_WSTR,
    EXPR_STR,

    EXPR_BUILTIN_SYMBOL,
    EXPR_UNKNOWN_SYMBOL,
    EXPR_SYMBOL,
    EXPR_GENERIC, // C11's _Generic
    EXPR_VA_ARG,

    EXPR_FUNCTION, // function literal
    EXPR_INITIALIZER,

    EXPR_CAST,
    EXPR_PARAM, // special case of EXPR_VAR
    EXPR_ASSIGN,
    EXPR_PLUS_ASSIGN,
    EXPR_MINUS_ASSIGN,
    EXPR_TIMES_ASSIGN,
    EXPR_SLASH_ASSIGN,
    EXPR_PERCENT_ASSIGN,
    EXPR_AND_ASSIGN,
    EXPR_OR_ASSIGN,
    EXPR_XOR_ASSIGN,
    EXPR_SHL_ASSIGN,
    EXPR_SHR_ASSIGN,

    EXPR_PLUS,
    EXPR_MINUS,
    EXPR_TIMES,
    EXPR_SLASH,
    EXPR_PERCENT,
    EXPR_AND,
    EXPR_OR,
    EXPR_XOR,
    EXPR_SHL,
    EXPR_SHR,

    EXPR_CMPEQ,
    EXPR_CMPNE,
    EXPR_CMPGE,
    EXPR_CMPLE,
    EXPR_CMPGT,
    EXPR_CMPLT,

    // these are resolved by semantics pass
    EXPR_PTRADD,
    EXPR_PTRSUB,
    EXPR_PTRDIFF,

    EXPR_TERNARY,
    EXPR_COMMA,

    EXPR_LOGICAL_NOT,
    EXPR_LOGICAL_AND,
    EXPR_LOGICAL_OR,

    EXPR_DEREF,
    EXPR_ADDR,
    EXPR_NEGATE,
    EXPR_NOT,
    EXPR_SUBSCRIPT,
    EXPR_DOT,
    EXPR_ARROW,
    EXPR_DOT_R,
    EXPR_ARROW_R,
    EXPR_CALL,

    EXPR_SIZEOF_T, // on type
    EXPR_SIZEOF,   // on expr

    EXPR_ALIGNOF_T, // on type
    EXPR_ALIGNOF,   // on expr

    EXPR_PRE_INC,
    EXPR_PRE_DEC,
    EXPR_POST_INC,
    EXPR_POST_DEC,

    EXPR_MAX
} ExprOp;

typedef enum {
    STMT_FLAGS_HAS_IR_BACKING = 1,
} StmtFlags;

struct Stmt {
    struct {
        StmtOp op;
        StmtFlags flags;
        SourceRange loc;
        Cuik_Attribute* attr_list;

        // Used by the backend for backend-y things
        union {
            #ifdef CUIK_USE_TB
            TB_Node* r;
            TB_Node* loop[2];
            TB_Function* f;
            TB_Symbol* s;
            TB_Global* g;
            TB_External* e;

            void* user_data;
            #else
            void* user_data;
            #endif /* CUIK_USE_TB */
        } backing;
    };
    union {
        struct StmtCompound {
            Stmt** kids;
            int kids_count;
        } compound;
        struct StmtExpr {
            Expr* expr;
        } expr;
        struct StmtReturn {
            Expr* expr;
        } return_;
        struct StmtContinue {
            Stmt* target; // loop
        } continue_;
        struct StmtBreak {
            Stmt* target; // either a loop or switch
        } break_;
        struct StmtGoto {
            Expr* target;
        } goto_;
        struct StmtLabel {
            Atom name;
            bool placed;
        } label;
        struct StmtCase {
            int64_t key;
            Stmt* body;
            Stmt* next;
        } case_;
        struct StmtDefault {
            Stmt* body;
            Stmt* next;
        } default_;
        struct StmtSwitch {
            Expr* condition;
            Stmt* body;

            // points to the first case or default,
            // and those point to next and so on,
            // linked lists
            Stmt* next;
        } switch_;
        struct StmtDecl {
            Cuik_QualType type;
            Atom name;

            // acceleration structure for scrubbing for symbols
            // it's a linked list
            Expr* first_symbol;

            // NOTE(NeGate): This represents a stmtindex if it's a
            // FUNC_DECL
            union {
                Stmt* initial_as_stmt;
                Expr* initial;
            };

            Attribs attrs;
        } decl;
        struct StmtFor {
            Stmt* first;
            Expr* cond;
            Stmt* body;
            Expr* next;
        } for_;
        struct StmtWhile {
            Expr* cond;
            Stmt* body;
        } while_;
        struct StmtDoWhile {
            Expr* cond;
            Stmt* body;
        } do_while;
        struct StmtIf {
            Expr* cond;
            Stmt* body;
            Stmt* next;
        } if_;
    };
};

struct Expr {
    ExprOp op : 8;

    // some flags:
    int has_parens : 1;
    int has_visited : 1;

    SourceRange loc;
    Cuik_QualType type;

    // this is the type it'll be desugared into
    // for example:
    //
    //   a + b where a and b are 16bit
    //
    //   their cast_type might be int because of C's
    //   promotion rules.
    Cuik_QualType cast_type;

    union {
        struct {
            Atom name;

            // aliases with next_symbol_in_chain
            Expr* next_symbol_in_chain;
        } builtin_sym;

        struct {
            Atom unknown_sym;

            // aliases with next_symbol_in_chain
            Expr* next_symbol_in_chain2;
        };

        // TODO(NeGate): rename this unnamed struct to 'symbol'
        struct {
            // linked list of symbols within a function used to
            // analyze used symbols more easily
            Stmt* symbol;
            Expr* next_symbol_in_chain;

            // are we currently iterating a symbol with is_resolving_symbol
            bool is_resolving_symbol;
        };

        // EXPR_PARAM
        int param_num;

        struct ExprEnum {
            EnumEntry* num;
        } enum_val;
        struct {
            Expr* src;
            Cuik_QualType type;
        } va_arg_;
        struct {
            Expr* src;
            Cuik_QualType type;
        } cast;
        struct {
            Expr* left;
            Expr* middle;
            Expr* right;
        } ternary_op;
        struct {
            Expr* left;
            Expr* right;
        } bin_op;
        struct {
            Expr* base;
            Expr* index;
        } subscript;
        struct {
            Expr* src;
        } unary_op;
        struct {
            Expr* controlling_expr;

            // if case_count == 0, then controlling_expr is the matched expression
            int case_count;
            C11GenericEntry* cases;
        } generic_;
        struct {
            Expr* base;
            uint32_t offset;

            // during semantics we'll switch between DOT or ARROW to DOT_R or ARROW_R
            // which means we use the member field
            union {
                Member* member;
                Atom name;
            };
        } dot_arrow;
        struct {
            Expr* target;
            int param_count;

            Expr** param_start;
        } call;
        // represent both quoted literals
        struct {
            const unsigned char* start;
            const unsigned char* end;
        } str;
        // either sizeof(T) or _Alignof(T)
        struct {
            Cuik_QualType type;
        } x_of_type;
        // either sizeof(expr) or _Alignof(expr)
        struct {
            Expr* expr;
        } x_of_expr;
        struct {
            Cuik_QualType type;
            InitNode* root;
        } init;
        struct {
            Stmt* src;
        } func;

        int char_lit;
        double float_num;
        struct ExprInt {
            unsigned long long num;
            Cuik_IntSuffix suffix;
        } int_num;
    };
};
_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, next_symbol_in_chain2), "these should be aliasing");
_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, builtin_sym.next_symbol_in_chain), "these should be aliasing");

static bool cuik_type_is_signed(const Cuik_Type* t) { return (t->kind >= KIND_CHAR && t->kind <= KIND_LLONG) && !t->is_unsigned; }
static bool cuik_type_is_unsigned(const Cuik_Type* t) { return (t->kind >= KIND_CHAR && t->kind <= KIND_LLONG) && t->is_unsigned; }

static bool cuik_type_is_integer(const Cuik_Type* t) { return t->kind >= KIND_CHAR && t->kind <= KIND_LLONG; }
static bool cuik_type_is_integer_or_bool(const Cuik_Type* t) { return t->kind >= KIND_BOOL && t->kind <= KIND_LLONG; }
static bool cuik_type_is_float(const Cuik_Type* t) { return t->kind >= KIND_FLOAT && t->kind <= KIND_DOUBLE; }

static bool cuik_type_is_bool(const Cuik_Type* t) { return t->kind == KIND_BOOL; }
static bool cuik_type_is_pointer(const Cuik_Type* t) { return t->kind == KIND_PTR; }

// [https://www.sigbus.info/n1570#6.2.5p21]
// Arithmetic types and pointer types are collectively called scalar types.
// Array and structure types are collectively called aggregate types.
static bool cuik_type_is_scalar(const Cuik_Type* t) { return t->kind >= KIND_BOOL && t->kind <= KIND_FUNC; }
static bool cuik_type_is_aggregate(const Cuik_Type* t) { return t->kind >= KIND_ARRAY && t->kind <= KIND_UNION; }

// [https://www.sigbus.info/n1570#6.2.5p18]
// Integer and floating types are collectively called arithmetic types.
static bool cuik_type_is_arithmatic(const Cuik_Type* t) { return t->kind >= KIND_BOOL && t->kind <= KIND_FUNC; }

static bool cuik_type_can_deref(const Cuik_Type* t) { return t->kind == KIND_PTR || t->kind == KIND_ARRAY; }

// takes in Cuik_QualType
#define CUIK_QUAL_TYPE_NULL        (Cuik_QualType){ 0 }
#define CUIK_QUAL_TYPE_IS_NULL(a)  ((a).raw == 0)
#define CUIK_QUAL_TYPE_HAS(a, b)   (((a).raw & (b)) != 0)
#define CUIK_QUAL_TYPE_ONLY(a, b)  (((a).raw & (b)) == (b))

static Cuik_QualType cuik_make_qual_type(Cuik_Type* t, Cuik_Qualifiers quals) {
    uintptr_t p = (uintptr_t) t;
    assert((p & CUIK_QUAL_FLAG_BITS) == 0);
    assert((quals & ~CUIK_QUAL_FLAG_BITS) == 0);

    return (Cuik_QualType){ p | quals };
}

static Cuik_Qualifiers cuik_get_quals(Cuik_QualType t) {
    return t.raw & CUIK_QUAL_FLAG_BITS;
}

static Cuik_QualType cuik_uncanonical_type(const Cuik_Type* t) {
    uintptr_t p = (uintptr_t) t;
    assert((p & CUIK_QUAL_FLAG_BITS) == 0);
    return (Cuik_QualType){ p };
}

static Cuik_Type* cuik_canonical_type(const Cuik_QualType t) {
    return (Cuik_Type*) (t.raw & ~((uintptr_t) CUIK_QUAL_FLAG_BITS));
}

// if level is non-NULL, *level stores the indirection level.
//
// example:
//   cuik_get_direct_type(int****)
//     return int, *level is 4
//
//   cuik_get_direct_type(T)
//     return T, *level is 0
//
//   cuik_get_direct_type(T*)
//     return T, *level is 1
//
static Cuik_QualType cuik_get_direct_type(Cuik_QualType type, int* level) {
    int l = 0;
    while (cuik_canonical_type(type)->kind == KIND_PTR) {
        type = cuik_canonical_type(type)->ptr_to;
        l += 1;
    }

    if (level != NULL) *level = l;
    return type;
}

CUIK_API const char* cuik_stmt_decl_name(Stmt* stmt);
CUIK_API Cuik_QualType cuik_stmt_decl_type(Stmt* stmt);

////////////////////////////////////////////
// Type checker
////////////////////////////////////////////
// This represents a compiled source file
typedef struct TranslationUnit TranslationUnit;

// returns the number of errors in this phase.
// if thread_pool is non-NULL the type checking will be parallelized
CUIK_API int cuiksema_run(TranslationUnit* restrict tu, Cuik_IThreadpool* restrict thread_pool);

////////////////////////////////////////////
// Pretty printers
////////////////////////////////////////////
CUIK_API void cuik_dump_translation_unit(FILE* stream, TranslationUnit* tu, bool minimalist);

CUIK_API void cuik_dump(FILE* stream, size_t count, Stmt** top_level, bool minimalist);
CUIK_API void cuik_dump_expr(FILE* stream, Expr* e, int depth);

////////////////////////////////////////////
// Iterators
////////////////////////////////////////////
typedef struct {
    // public
    Expr* expr;

    // internal
    size_t index_;
    Expr* parent_;
} Cuik_ExprIter;

typedef struct {
    // public
    Stmt* stmt;

    // internal
    size_t index_;
    Stmt* parent_;
} Cuik_StmtIter;

#define CUIK_FOR_KID_IN_STMT(it, parent_) \
for (Cuik_StmtIter it = { .parent = (parent_) }; cuik_next_stmt_kid(&it);)

#define CUIK_FOR_KID_IN_EXPR(it, parent_) \
for (Cuik_ExprIter it = { .parent = (parent_) }; cuik_next_expr_kid(&it);)

CUIK_API bool cuik_next_expr_kid(Cuik_ExprIter* it);
CUIK_API bool cuik_next_stmt_kid(Cuik_StmtIter* it);
