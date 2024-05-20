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
// TODO:
//
//  * rename all the bare types to use Cuik_
//
//  * implement VLAs
//
//  * add qualifiers to type pretty printer
//
#ifndef CUIK_AST_H
#define CUIK_AST_H

#include "cuik_prelude.h"

typedef char* Atom;
typedef struct Cuik_Expr Cuik_Expr;
typedef struct Subexpr Subexpr;
typedef struct Stmt Stmt;
typedef struct Cuik_Type Cuik_Type;

typedef uint64_t Cuik_ConstInt;

typedef struct {
    enum { CUIK_CONST_NONE, CUIK_CONST_INT, CUIK_CONST_FLOAT, CUIK_CONST_ADDR } tag;
    union {
        uint64_t i;
        double f;

        // symbols refer to their creator + an offset
        struct {
            uint32_t base;
            int32_t offset;
        } s;
    };
} Cuik_ConstVal;

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
    bool is_noret   : 1;

    // NOTE(NeGate): In all honesty, this should probably not be
    // here since it's used in cases that aren't relevant to attribs.
    // mainly STMT_DECL, it keeps track of if anyone's referenced it
    // so we can garbage collect the symbol later.
    bool is_root : 1;
    bool is_used : 1;
} Attribs;

typedef struct {
    Atom key;
    bool init;

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
    Cuik_Expr* value;
} C11GenericEntry;

typedef enum {
    // used by cycle checking
    CUIK_TYPE_FLAG_COMPLETE = 1,
    CUIK_TYPE_FLAG_PROGRESS = 2,
} Cuik_TypeFlags;

struct Cuik_Type {
    Cuik_TypeKind kind;
    int size;  // sizeof
    int align; // _Alignof
    Cuik_TypeFlags flags;
    SourceRange loc;

    Atom also_known_as;

    #ifdef CUIK_USE_TB
    _Atomic(TB_DebugType*) debug_type;
    #else
    void* user_data;
    #endif

    union {
        char _;

        // Integers
        bool is_unsigned;

        // Arrays
        struct {
            Cuik_QualType of;
            int count;

            // if non-zero, then we must execute an expression
            // parser to resolve it
            int count_lexer_pos;
        } array;

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

            int kid_count, pad;
            Member* kids;

            // this is the one used in type comparisons
            Cuik_Type* nominal;
        } record;

        // Enumerators
        struct {
            Atom name;
            int count, pad;

            EnumEntry* entries;
        } enumerator;

        struct {
            Cuik_Type* base;
            int count, pad;
        } vector;

        // Typeof
        struct {
            Cuik_Expr* src;
        } typeof_;

        struct {
            Atom name;

            // if non-NULL we've got a linked list to walk :)
            Cuik_Type* next;
        } placeholder;
    };
} __attribute__((aligned(16)));
_Static_assert(_Alignof(Cuik_Type) == 16, "we need 16byte alignment for Cuik_QualType");

#define CUIK_TYPE_IS_COMPLETE(t) (((t)->flags & CUIK_TYPE_FLAG_COMPLETE) != 0)
#define CUIK_TYPE_IS_PROGRESS(t) (((t)->flags & CUIK_TYPE_FLAG_PROGRESS) != 0)

typedef enum {
    CUIK_GLSL_STORAGE_UNKNOWN,
    CUIK_GLSL_STORAGE_IN,
    CUIK_GLSL_STORAGE_INOUT,
    CUIK_GLSL_STORAGE_OUT,

    // constant buffers
    CUIK_GLSL_STORAGE_UNIFORM,

    // storage buffers
    CUIK_GLSL_STORAGE_BUFFER,
} Cuik_GlslStorageQuals;

typedef enum {
    CUIK_GLSL_QUALS_COHERENT  = 1,
    CUIK_GLSL_QUALS_VOLATILE  = 2,
    CUIK_GLSL_QUALS_RESTRICT  = 4,
    CUIK_GLSL_QUALS_READONLY  = 8,
    CUIK_GLSL_QUALS_WRITEONLY = 16,
} Cuik_GlslMemQuals;

typedef enum {
    CUIK_GLSL_PRECISION_LOWP,
    CUIK_GLSL_PRECISION_MEDIUMP,
    CUIK_GLSL_PRECISION_HIGHP,
} Cuik_GlslPrecision;

typedef enum {
    CUIK_GLSL_FLAT,
    CUIK_GLSL_SMOOTH,
    CUIK_GLSL_NOPERSPECTIVE,
} Cuik_GlslInterpolation;

typedef enum {
    CUIK_GLSL_LAYOUT_UNKNOWN,
    CUIK_GLSL_LAYOUT_140,
    CUIK_GLSL_LAYOUT_430,
} Cuik_GlslLayout;

typedef enum {
    CUIK_GLSL_SWIZZLE_UNKNOWN,

    CUIK_GLSL_SWIZZLE_XYZW,
    CUIK_GLSL_SWIZZLE_STUV,
    CUIK_GLSL_SWIZZLE_RGBA,
} Cuik_GlslSwizzle;

typedef struct {
    Cuik_GlslMemQuals mem;
    Cuik_GlslStorageQuals storage;
    Cuik_GlslInterpolation interp;

    bool flat;

    // layout qualifier
    Cuik_GlslLayout layout;
    int binding;
    int location;
    int offset;
} Cuik_GlslQuals;

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

    Cuik_Expr* expr;
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
    STMT_DISCARD,

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
    EXPR_CONSTRUCTOR,
    EXPR_GENERIC, // C11's _Generic
    EXPR_VA_ARG,

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

    EXPR_SWIZZLE, // GLSL stuff, .xyxy

    EXPR_SIZEOF_T, // on type
    EXPR_SIZEOF,   // on expr
    EXPR_ALIGNOF_T,// on type

    EXPR_PRE_INC,
    EXPR_PRE_DEC,
    EXPR_POST_INC,
    EXPR_POST_DEC,

    EXPR_MAX
} ExprOp;

typedef enum {
    STMT_FLAGS_HAS_IR_BACKING = 1,
    STMT_FLAGS_IS_EXPORTED    = 2,
    STMT_FLAGS_IS_RESOLVING   = 4,
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
            TB_Node* n;
            TB_Node* loop[2];
            TB_Function* f;
            TB_Symbol* s;
            TB_Global* g;
            TB_External* e;
            #endif /* CUIK_USE_TB */

            void* user_data;
        } backing;
    };
    union {
        struct StmtCompound {
            Stmt** kids;
            int kids_count;
        } compound;
        struct StmtExpr {
            Cuik_Expr* expr;
        } expr;
        struct StmtReturn {
            Cuik_Expr* expr;
        } return_;
        struct StmtContinue {
            Stmt* target; // loop
        } continue_;
        struct StmtBreak {
            Stmt* target; // either a loop or switch
        } break_;
        struct StmtGoto {
            Cuik_Expr* target;
        } goto_;
        struct StmtLabel {
            Atom name;
            bool placed;
        } label;
        struct StmtCase {
            Stmt* next;
            int64_t key;
            int64_t key_max;
            Stmt* body;
        } case_;
        struct StmtDefault {
            Stmt* next;
            Stmt* body;
        } default_;
        struct StmtSwitch {
            // points to the first case or default,
            // and those point to next and so on,
            // linked lists
            Stmt* next;

            Cuik_Expr* condition;
            Stmt* body;
        } switch_;
        struct StmtDecl {
            Cuik_QualType type;
            Atom name;
            Cuik_GlslQuals* glsl_quals;

            // acceleration structure for scrubbing for symbols
            // it's a linked list
            Cuik_Expr* first_symbol;

            // NOTE(NeGate): This represents a stmtindex if it's a
            // FUNC_DECL
            union {
                Stmt* initial_as_stmt;
                Cuik_Expr* initial;
            };

            Attribs attrs;
            uint32_t local_ordinal;
        } decl;
        struct StmtFor {
            Stmt* first;
            Cuik_Expr* cond;
            Stmt* body;
            Cuik_Expr* next;
        } for_;
        struct StmtWhile {
            Cuik_Expr* cond;
            Stmt* body;
        } while_;
        struct StmtDoWhile {
            Cuik_Expr* cond;
            Stmt* body;
        } do_while;
        struct StmtIf {
            Cuik_Expr* cond;
            Stmt* body;
            Stmt* next;
        } if_;
    };
};

struct Subexpr {
    SourceRange loc;
    ExprOp op : 8;

    // some flags:
    int has_parens : 1;
    int has_visited : 1;

    union {
        uint32_t char_lit;
        double float_lit;

        struct {
            uint64_t lit;
            Cuik_IntSuffix suffix;
        } int_lit;

        struct {
            uint8_t len;
            uint8_t indices[4];
        } swizzle;

        struct {
            Cuik_Type* type;
        } constructor;

        struct {
            Atom name;
            ptrdiff_t next_symbol;
        } unknown_sym;

        struct {
            Atom name;
        } builtin_sym;

        struct {
            Stmt* stmt;
            ptrdiff_t next_symbol;
        } sym;

        // EXPR_PARAM
        int param_num;

        struct {
            Cuik_QualType type;
            EnumEntry* num;
        } enum_val;

        struct {
            Cuik_QualType type;
        } va_arg_;

        struct {
            Cuik_QualType type;
        } cast;

        // represent both quoted literals
        struct {
            const unsigned char* start;
            const unsigned char* end;
        } str;

        // either sizeof(T) or _Alignof(T)
        struct {
            Cuik_QualType type;
        } x_of_type;

        struct {
            Cuik_QualType type;
            InitNode* root;
        } init;

        struct {
            // tells us the base is on the right side
            bool flipped;
        } ptrop;

        struct {
            // if case_count == 0, then controlling_expr is the matched expression
            int case_count;
            C11GenericEntry* cases;
        } generic_;

        struct {
            uint32_t offset;

            // during semantics we'll switch between DOT or ARROW to DOT_R or ARROW_R
            // which means we use the member field
            union {
                Member* member;
                Atom name;
            };
        } dot_arrow;

        struct {
            // the sides aren't in the same Cuik_Expr because they're conditionally run
            Cuik_Expr *left, *right;
        } ternary;

        struct {
            // the sides aren't in the same Cuik_Expr because they're conditionally run
            Cuik_Expr *left, *right;
        } logical_binop;

        struct {
            int param_count;
        } call;
    };
};

// # COMPRESSED EXPRESSIONS
//   To represent a metric shitload of expressions in Cuik we compact them
//   using a postfix notation. Instead of using pointers to refer to inputs
//   it's implicit to this position in the expression stream.
//
//     1 x y * +                     +
//                                  / \
//                    versus       1   *
//                                    / \
//                                   x   y
//
// # CAST TYPE
//   an Expr's cast_type is the type it'll be desugared into.
//
//     a + b where a and b are 16bit
//
//   their cast_type might be int because of C's
//   promotion rules.
//
// # SYMBOL CHAIN
//   an EXPR_SYMBOL or EXPR_UNKNOWN_SYMBOL will be part of a linked list which
//   is used for knowing when symbols are in use.
//
struct Cuik_Expr {
    size_t count;
    bool visited;

    ptrdiff_t first_symbol;
    Cuik_Expr* next_in_chain;

    // constructed during type checking
    Cuik_QualType* types;
    Cuik_QualType* cast_types;

    // constructed during parse time
    Subexpr* exprs;
};

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

static bool cuik_type_implicit_ptr(const Cuik_Type* t) { return t->kind == KIND_FUNC || t->kind == KIND_ARRAY; }

static bool cuik_type_can_swizzle(const Cuik_Type* t) {
    if (t->kind == KIND_VECTOR && t->vector.base->kind != KIND_VECTOR && cuik_type_can_swizzle(t->vector.base)) {
        return true;
    }

    return t->kind == KIND_INT || t->kind == KIND_FLOAT || t->kind == KIND_DOUBLE;
}

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
CUIK_API Cuik_QualType cuik_get_direct_type(Cuik_QualType type, int* level);

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

CUIK_API int cuik_get_expr_arity(Subexpr* e);
CUIK_API const char* cuik_get_expr_name(Subexpr* e);

#endif // CUIK_AST_H

#ifdef CUIK_AST_IMPL
#undef CUIK_AST_IMPL // dumb style of include guard

Cuik_QualType cuik_get_direct_type(Cuik_QualType type, int* level) {
    int l = 0;
    while (cuik_canonical_type(type)->kind == KIND_PTR) {
        type = cuik_canonical_type(type)->ptr_to;
        l += 1;
    }

    if (level != NULL) *level = l;
    return type;
}

#endif // CUIK_AST_IMPL
