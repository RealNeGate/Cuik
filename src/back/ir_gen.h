#pragma once
#include <common.h>
#include <arena.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

#include "tb.h"

extern TokenStream ir_gen_tokens;
extern FILE* tbir_output_file;
extern TB_Module* mod;

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
		case KIND_STRUCT:
		case KIND_UNION:
		return TB_TYPE_PTR;
		
		default: abort(); // TODO
	}
}

void gen_ir(TopLevel tl, size_t i);
