#include "parser.h"
#include "../ext/threads.h"

impl_arena(Type, type_arena)
impl_arena(Member, member_arena)
impl_arena(Arg, arg_arena)
impl_arena(EnumEntry, enum_entry_arena)

static mtx_t type_mutex;

void init_types() {
	init_arg_arena(4 * 1024);
	init_type_arena(4 * 1024);
	init_member_arena(4 * 1024);
	
	const static Type default_types[] = {
		[TYPE_NONE] = { KIND_VOID, 0, 0 },
		
		[TYPE_VOID] = { KIND_VOID, 0, 0 },
		[TYPE_BOOL] = { KIND_BOOL, 1, 1 },
		
		[TYPE_CHAR] = { KIND_CHAR, 1, 1 },
		[TYPE_SHORT] = { KIND_SHORT, 2, 2 },
		[TYPE_INT] = { KIND_INT, 4, 4 },
		[TYPE_LONG] = { KIND_LONG, 8, 8 },
		
		[TYPE_UCHAR] = { KIND_CHAR, 1, 1, .is_unsigned = true },
		[TYPE_USHORT] = { KIND_SHORT, 2, 2, .is_unsigned = true },
		[TYPE_UINT] = { KIND_INT, 4, 4, .is_unsigned = true },
		[TYPE_ULONG] = { KIND_LONG, 8, 8, .is_unsigned = true },
		
		[TYPE_FLOAT] = { KIND_FLOAT, 4, 4 },
		[TYPE_DOUBLE] = { KIND_DOUBLE, 8, 8 }
	};
	
	memcpy(type_arena.data, default_types, sizeof(default_types));
	type_arena.count = sizeof(default_types) / sizeof(default_types[0]);
	
	mtx_init(&type_mutex, mtx_plain);
}

TypeIndex new_enum() {
	TypeIndex t = push_type_arena(1);
	type_arena.data[t] = (Type){
		.kind = KIND_ENUM,
		.size = 4,
		.align = 4
	};
	
	return t;
}

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

TypeIndex new_record(bool is_union) {
	TypeIndex t = push_type_arena(1);
	type_arena.data[t] = (Type){
		.kind = is_union ? KIND_UNION : KIND_STRUCT
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
		.array_of = base,
		.array_count = count
	};
	
	return t;
}

TypeIndex new_pointer_locked(TypeIndex base) {
	mtx_lock(&type_mutex);
	TypeIndex t = push_type_arena(1);
	type_arena.data[t] = (Type){
		.kind = KIND_PTR,
		.size = 8,
		.align = 8,
		.ptr_to = base
	};
	mtx_unlock(&type_mutex);
	
	return t;
}

// https://github.com/rui314/chibicc/blob/main/type.c
//
// NOTE(NeGate): this function is called within the IR gen
// code which is parallel so we need a mutex to avoid messing
// up the type arena.
TypeIndex get_common_type(TypeIndex a, TypeIndex b) {
	if (a == b) return a;
	
	Type* types = type_arena.data;
	Type* restrict ty1 = &types[a];
	Type* restrict ty2 = &types[b];
	
	// implictly convert arrays into pointers
	if (ty1->kind == KIND_ARRAY) {
		return new_pointer_locked(ty1->array_of);
	}
	
	// implictly convert functions into function pointers
	if (ty1->kind == KIND_FUNC) {
		return new_pointer_locked(a);
	}
	
	if (ty2->kind == KIND_FUNC) {
		return new_pointer_locked(b);
	}
	
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

bool type_equal(TypeIndex a, TypeIndex b) {
	if (a == b) return true;
	
	Type* types = type_arena.data;
	Type* restrict ty1 = &types[a];
	Type* restrict ty2 = &types[b];
	
	// just because they match kind doesn't necessarily
	// mean they're equivalent but if they don't match
	// then it means they definetely aren't the same.
	if (ty1->kind != ty2->kind) return false;
	
	if (ty1->kind == KIND_FUNC) {
		// match arg count
		ArgIndex arg_count1 = ty1->func.arg_end - ty1->func.arg_start;
		ArgIndex arg_count2 = ty2->func.arg_end - ty2->func.arg_start;
		if (arg_count1 != arg_count2) return false;
		
		// match var args
		if (ty1->func.has_varargs != ty2->func.has_varargs) return false;
		
		// match args exactly
		ArgIndex arg_start1 = ty1->func.arg_start;
		ArgIndex arg_start2 = ty2->func.arg_start;
		for (ArgIndex i = 0; i < arg_count1; i++) {
			if (!type_equal(arg_arena.data[arg_start1 + i].type,
							arg_arena.data[arg_start2 + i].type)) {
				return false;
			}
		}
		
		return true;
	} else if (ty1->kind == KIND_PTR) {
		return type_equal(ty1->ptr_to, ty2->ptr_to);
	}
	
	// but by default kind matching is enough
	// like for integers, booleans and floats
	return true;
}
