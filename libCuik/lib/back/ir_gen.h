#pragma once
#ifdef CUIK_USE_TB

#include <arena.h>
#include <common.h>
#include <threads.h>
#include "../front/parser.h"

#include <tb.h>

typedef enum IRValType {
    RVALUE,
    RVALUE_PHI,

    LVALUE,
    LVALUE_SYM,
    LVALUE_BITS,
    LVALUE_EXPR,
    LVALUE_LABEL,
} IRValType;

typedef struct IRVal {
    IRValType value_type;
    Cuik_QualType type, cast_type;

    union {
        TB_Node* reg;
        Stmt* sym;
        Subexpr* e;
        struct {
            TB_Node* reg;

            short offset;
            short width;
        } bits;
        struct {
            TB_Node* if_true;
            TB_Node* if_false;
            TB_Node* merger;
        } phi;
    };
} IRVal;

static TB_DataType ctype_to_tbtype(const Cuik_Type* t) {
    switch (t->kind) {
        case KIND_VOID:
        return TB_TYPE_VOID;
        case KIND_BOOL:
        return TB_TYPE_BOOL;

        case KIND_CHAR:
        case KIND_SHORT:
        case KIND_INT:
        case KIND_LONG:
        case KIND_LLONG:
        return TB_TYPE_INTN(t->size * 8);

        case KIND_FLOAT:
        return TB_TYPE_F32;
        case KIND_DOUBLE:
        return TB_TYPE_F64;
        case KIND_ENUM:
        return TB_TYPE_I32;

        case KIND_PTR:
        case KIND_FUNC:
        case KIND_ARRAY:
        return TB_TYPE_PTR;

        case KIND_STRUCT:
        case KIND_UNION:
        return TB_TYPE_PTR;

        default:
        abort(); // TODO
    }
}

TB_Node* cvt2rval(TranslationUnit* tu, TB_Function* func, IRVal* v);
int count_max_tb_init_objects(InitNode* root_node);
TB_DebugType* cuik__as_tb_debug_type(TB_Module* mod, Cuik_Type* t);

static void irgen_stmt(TranslationUnit* tu, TB_Function* func, Stmt* restrict s);
static TB_Node* irgen_as_rvalue(TranslationUnit* tu, TB_Function* func, Cuik_Expr* e);
static IRVal irgen_expr(TranslationUnit* tu, TB_Function* func, Cuik_Expr* e);

#endif
