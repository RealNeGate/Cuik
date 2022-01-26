#pragma once
#include <common.h>
#include <arena.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

#include "tb.h"

extern TokenStream ir_gen_tokens;
extern FILE* tbir_output_file;
extern TB_Module* mod;

typedef enum IRValType {
	RVALUE,
	
	LVALUE,
	LVALUE_BITS,
	LVALUE_LABEL,
	LVALUE_FUNC,
	LVALUE_EFUNC,
	
	// if value is a special kind of rvalue
	// it essentially represents a phi node
	// where it's true on one path and false
	// on the other.
	RVALUE_PHI
} IRValType;

typedef struct IRVal {
	IRValType value_type;
	TypeIndex type;
	
	union {
		TB_Register reg;
		TB_Function* func;
		TB_ExternalID ext;
		struct {
			TB_Register reg;
			
			short offset;
			short width;
		} bits;
		struct {
			TB_Label if_true;
			TB_Label if_false;
		} phi;
		TB_Label label;
	};
} IRVal;

static TB_DataType ctype_to_tbtype(const Type* t) {
	switch (t->kind) {
		case KIND_VOID: return TB_TYPE_VOID;
		case KIND_BOOL: return TB_TYPE_BOOL;
		case KIND_CHAR: return TB_TYPE_I8;
		case KIND_SHORT: return TB_TYPE_I16;
		case KIND_INT: return TB_TYPE_I32;
		case KIND_LONG: return TB_TYPE_I64;
		case KIND_FLOAT: return TB_TYPE_F32;
		case KIND_DOUBLE: return TB_TYPE_F64;
		case KIND_ENUM: return TB_TYPE_I32;
		
		case KIND_PTR: 
		case KIND_FUNC:
		case KIND_ARRAY: 
		return TB_TYPE_PTR;
		
		case KIND_STRUCT:
		case KIND_UNION: {
			if (t->record.intrin_type.type != TB_VOID) return t->record.intrin_type;
			return TB_TYPE_PTR;
		}
		
		default: abort(); // TODO
	}
}

// logging
// TODO(NeGate): consider merging this with the rest of the logging
_Noreturn void irgen_fatal(SourceLocIndex loc, const char* fmt, ...);
void irgen_warn(SourceLocIndex loc, const char* fmt, ...);

InitNode* count_max_tb_init_objects(int node_count, InitNode* node, int* out_count);

// func is NULL then it's not allowed to compute any dynamic initializer expressions
InitNode* eval_initializer_objects(TranslationUnit* tu, TB_Function* func, TB_InitializerID init, TB_Register addr, TypeIndex t, int node_count, InitNode* node, int* offset);

TB_Register irgen_as_rvalue(TranslationUnit* tu, TB_Function* func, ExprIndex e);
IRVal irgen_expr(TranslationUnit* tu, TB_Function* func, ExprIndex e);
void irgen_stmt(TranslationUnit* tu, TB_Function* func, StmtIndex s);
void irgen_top_level_stmt(TranslationUnit* tu, StmtIndex s);
