#include "parser.h"

impl_arena(Type, type_arena)
impl_arena(Member, member_arena)
impl_arena(Arg, arg_arena)

TypeIndex new_func() {
	TypeIndex t = push_type_arena(1);
	type_arena.data[t] = (Type){
		.kind = KIND_FUNC,
		.size = 8,
		.align = 8
	};
	
	return t;
}

TypeIndex copy_type(TypeIndex base) {
	TypeIndex t = push_type_arena(1);
	type_arena.data[t] = type_arena.data[base];
	
	return t;
}

TypeIndex new_struct() {
	TypeIndex t = push_type_arena(1);
	type_arena.data[t] = (Type){
		.kind = KIND_STRUCT
	};
	
	return t;
}

TypeIndex new_pointer(TypeIndex base) {
	TypeIndex t = push_type_arena(1);
	type_arena.data[t] = (Type){
		.kind = KIND_PTR,
		.size = 8,
		.align = 8,
		.ptr_to = base
	};
	
	return t;
}

TypeIndex new_array(TypeIndex base, int count) {
	TypeIndex t = push_type_arena(1);
	int size = type_arena.data[base].size;
	int align = type_arena.data[base].align;
	
	type_arena.data[t] = (Type){
		.kind = KIND_ARRAY,
		.size = size * count,
		.align = align,
		.array_of = base
	};
	
	return t;
}

// https://github.com/rui314/chibicc/blob/main/type.c
TypeIndex get_common_type(TypeIndex a, TypeIndex b) {
	if (a == b) return a;
	
	Type* types = type_arena.data;
	Type* restrict ty1 = &types[a];
	Type* restrict ty2 = &types[b];
	
	// implictly convert arrays into pointers
	if (ty1->kind == KIND_ARRAY)
		return new_pointer(ty1->array_of);
	
	// implictly convert functions into function pointers
	if (ty1->kind == KIND_FUNC)
		return new_pointer(a);
	if (ty2->kind == KIND_FUNC)
		return new_pointer(b);
	
	// operations with floats promote up to floats
	// e.g. 1 + 2.0 = 3.0
	if (ty1->kind == KIND_DOUBLE || ty2->kind == KIND_DOUBLE)
		return TYPE_DOUBLE;
	if (ty1->kind == KIND_FLOAT || ty2->kind == KIND_FLOAT)
		return TYPE_FLOAT;
	
	// promote any small integral types into ints
	if (ty1->size < 4) {
		a = TYPE_INT;
		ty1 = &types[a];
	}
	
	if (ty2->size < 4) {
		b = TYPE_INT;
		ty2 = &types[b];
	}
	
	// if the types don't match pick the bigger one
	if (ty1->size != ty2->size)
		return (ty1->size < ty2->size) ? b : a;
	
	// unsigned types are preferred
	if (ty2->kind >= KIND_CHAR && ty2->kind <= KIND_LONG && ty2->is_unsigned)
		return b;
	
	return a;
}
