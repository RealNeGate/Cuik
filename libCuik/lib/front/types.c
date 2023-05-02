#include <threads.h>
#include "parser.h"
#include "../targets/targets.h"

Cuik_Type cuik__builtin_void = { KIND_VOID,  0, 0, .is_complete = true };
Cuik_Type cuik__builtin_bool = { KIND_BOOL,  1, 1, .is_complete = true };
Cuik_Type cuik__builtin_float  = { KIND_FLOAT,  4, 4, .is_complete = true };
Cuik_Type cuik__builtin_double = { KIND_DOUBLE, 8, 8, .is_complete = true };

// we allocate nodes from here but once the threaded parsing stuff is complete we'll stitch this
// to the original AST arena so that it may be freed later
thread_local static Arena local_ast_arena;

Cuik_TypeTable init_type_table(Cuik_Target* target) {
    Cuik_TypeTable t = { 0 };
    t.target = target;
    t.exp = 16;
    t.table = cuik__valloc((1u << t.exp) * sizeof(Cuik_Type*));
    return t;
}

void free_type_table(Cuik_TypeTable* types) {
    cuik__vfree(types->table, (1u << types->exp) * sizeof(Cuik_Type*));
}

// this is used for primitives types such that they're not allocating new slots
static void type_insert(Cuik_TypeTable* types, Cuik_Type* src) {
    assert(((uintptr_t)src & CUIK_QUAL_FLAG_BITS) == 0);

    // check for interning
    uint32_t mask = (1 << types->exp) - 1;
    uint32_t hash = murmur3_32(src, sizeof(Cuik_Type));
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - types->exp)) | 1;
        i = (i + step) & mask;

        if (types->table[i] == NULL) {
            // empty slot
            assert(types->count <= mask && "how tf did you run out now?");

            types->count++;
            types->table[i] = src;
            break;
        } else if (memcmp(src, types->table[i], sizeof(Cuik_Type)) == 0) {
            types->table[i] = src;
            break;
        }
    }
}

static Cuik_Type* type_intern(Cuik_TypeTable* types, const Cuik_Type* src) {
    Cuik_Type* result = NULL;

    // check for interning
    uint32_t mask = (1 << types->exp) - 1;
    uint32_t hash = murmur3_32(src, sizeof(Cuik_Type));

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - types->exp)) | 1;
        i = (i + step) & mask;

        if (types->table[i] == NULL) {
            // empty slot
            if (types->count > mask) {
                printf("Type arena: out of memory!\n");
                abort();
            }

            result = arena_alloc(&local_ast_arena, sizeof(Cuik_Type), 16);
            *result = *src;

            assert(((uintptr_t)result & CUIK_QUAL_FLAG_BITS) == 0);

            types->count++;
            types->table[i] = result;
            break;
        } else if (memcmp(src, types->table[i], sizeof(Cuik_Type)) == 0) {
            result = types->table[i];
            break;
        }
    }

    return result;
}

// this isn't placed into the type table just yet, it will be later on via type_insert
static Cuik_Type* type_placeholder(Cuik_TypeTable* types) {
    return arena_alloc(&local_ast_arena, sizeof(Cuik_Type), 16);
}

Cuik_Type* cuik__new_pointer(Cuik_TypeTable* types, Cuik_QualType base) {
    // TODO(NeGate): this code depends on pointers being 8bytes, plz stop doing that
    return type_intern(types, &(Cuik_Type){
            .kind = KIND_PTR,
            .size = 8,
            .align = 8,
            .is_complete = true,
            .ptr_to = base,
        });
}

Cuik_Type* cuik__new_array(Cuik_TypeTable* types, Cuik_QualType base, int count) {
    Cuik_Type* canon = cuik_canonical_type(base);
    if (count == 0) {
        // these zero-sized arrays don't actually care about incomplete element types
        return type_intern(types, &(Cuik_Type){
                .kind = KIND_ARRAY,
                .size = 0,
                .align = canon->align,
                .array_of = base,
                .array_count = 0,
            });
    }

    int size = canon->size;
    int align = canon->align;

    int dst;
    if (__builtin_mul_overflow(size, count, &dst)) {
        assert(0 && "Overflow on new_array with big inputs");
    }

    assert(align != 0);
    return type_intern(types, &(Cuik_Type){
            .kind = KIND_ARRAY,
            .size = dst,
            .align = align,
            .is_complete = true,
            .array_of = base,
            .array_count = count,
        });
}

Cuik_Type* cuik__new_vector(Cuik_TypeTable* types, Cuik_QualType base, int count) {
    return type_intern(types, &(Cuik_Type){
            .kind = KIND_VECTOR,
            .size = 0,
            .align = cuik_canonical_type(base)->align,
            .is_complete = true,
            .array_of = base,
            .array_count = 0,
        });
}

// https://github.com/rui314/chibicc/blob/main/type.c
//
// NOTE(NeGate): this function is called within the IR gen
// code which is parallel so we need a mutex to avoid messing
// up the type arena.
Cuik_Type* get_common_type(Cuik_TypeTable* types, Cuik_Type* ty1, Cuik_Type* ty2) {
    // implictly convert arrays into pointers
    if (ty1->kind == KIND_ARRAY) {
        return cuik__new_pointer(types, ty1->array_of);
    }

    if (ty1->kind == KIND_VOID || ty2->kind == KIND_VOID) {
        return &cuik__builtin_void;
    }

    // implictly convert functions into function pointers
    if (ty1->kind == KIND_FUNC) {
        return cuik__new_pointer(types, cuik_make_qual_type(ty1, 0));
    }

    if (ty2->kind == KIND_FUNC) {
        return cuik__new_pointer(types, cuik_make_qual_type(ty2, 0));
    }

    // operations with floats promote up to floats
    // e.g. 1 + 2.0 = 3.0
    if (ty1->kind == KIND_DOUBLE || ty2->kind == KIND_DOUBLE)
        return &cuik__builtin_double;
    if (ty1->kind == KIND_FLOAT || ty2->kind == KIND_FLOAT)
        return &cuik__builtin_float;

    // promote any small integral types into ints
    Cuik_Type* int_type = &types->target->signed_ints[CUIK_BUILTIN_INT];
    if (ty1->size < int_type->size) ty1 = int_type;
    if (ty2->size < int_type->size) ty2 = int_type;

    // if the types don't match pick the bigger one
    if (ty1->size != ty2->size) return (ty1->size < ty2->size) ? ty2 : ty1;

    // unsigned types are preferred
    if (ty2->kind >= KIND_CHAR && ty2->kind <= KIND_LONG && ty2->is_unsigned) return ty2;

    return ty1;
}

bool type_equal(Cuik_Type* ty1, Cuik_Type* ty2) {
    if (ty1 == ty2) return true;

    // just because they match kind doesn't necessarily
    // mean they're equivalent but if they don't match
    // then it means they definetely aren't the same.
    if (ty1->kind != ty2->kind) return false;

    if (ty1->kind == KIND_FUNC) {
        // match parameters count
        size_t param_count1 = ty1->func.param_count;
        size_t param_count2 = ty2->func.param_count;

        // if there's no params on it then just pretend like it matches
        // it helps get stuff like FARPROC to compile properly
        if (param_count1 == 0 || param_count2 == 0) return true;
        if (param_count1 != param_count2) return false;

        // match var args
        if (ty1->func.has_varargs != ty2->func.has_varargs) return false;

        // match paramaeters exactly
        Param* param_list1 = ty1->func.param_list;
        Param* param_list2 = ty2->func.param_list;
        for (size_t i = 0; i < param_count1; i++) {
            if (!type_equal(cuik_canonical_type(param_list1[i].type), cuik_canonical_type(param_list2[i].type))) {
                return false;
            }
        }

        return true;
    } else if (ty1->kind == KIND_STRUCT || ty2->kind == KIND_UNION) {
        return (ty1 == ty2);
    } else if (ty1->kind == KIND_PTR) {
        return type_equal(cuik_canonical_type(ty1->ptr_to), cuik_canonical_type(ty2->ptr_to));
    }

    // but by default kind matching is enough
    // like for integers, booleans and floats
    return true;
}

size_t type_as_string(size_t max_len, char* buffer, Cuik_Type* type) {
    if (type == NULL) {
        size_t i = cstr_copy(max_len, buffer, "nil");
        buffer[i] = '\0';
        return i;
    }

    size_t i = 0;
    if (type->also_known_as != NULL) {
        i += cstr_copy(max_len - i, &buffer[i], (char*)type->also_known_as);
        i += cstr_copy(max_len - i, &buffer[i], " (aka ");
    }

    switch (type->kind) {
        case KIND_VOID:
        i += cstr_copy(max_len - i, &buffer[i], "void");
        break;
        case KIND_BOOL:
        i += cstr_copy(max_len - i, &buffer[i], "_Bool");
        break;
        case KIND_CHAR: {
            if (type->is_unsigned) i += cstr_copy(max_len - i, &buffer[i], "unsigned ");

            i += cstr_copy(max_len - i, &buffer[i], "char");
            break;
        }
        case KIND_SHORT: {
            if (type->is_unsigned) i += cstr_copy(max_len - i, &buffer[i], "unsigned ");

            i += cstr_copy(max_len - i, &buffer[i], "short");
            break;
        }
        case KIND_INT: {
            if (type->is_unsigned) i += cstr_copy(max_len - i, &buffer[i], "unsigned ");

            i += cstr_copy(max_len - i, &buffer[i], "int");
            break;
        }
        case KIND_LONG: {
            if (type->is_unsigned) i += cstr_copy(max_len - i, &buffer[i], "unsigned ");

            i += cstr_copy(max_len - i, &buffer[i], "long");
            break;
        }
        case KIND_LLONG: {
            if (type->is_unsigned) i += cstr_copy(max_len - i, &buffer[i], "unsigned ");

            i += cstr_copy(max_len - i, &buffer[i], "long long");
            break;
        }
        case KIND_FLOAT:
        i += cstr_copy(max_len - i, &buffer[i], "float");
        break;
        case KIND_DOUBLE:
        i += cstr_copy(max_len - i, &buffer[i], "double");
        break;
        case KIND_ENUM: {
            i += cstr_copy(max_len - i, &buffer[i], "enum ");

            if (type->enumerator.name) {
                i += cstr_copy(max_len - i, &buffer[i], (char*)type->enumerator.name);
            } else {
                i += cstr_copy(max_len - i, &buffer[i], "unnamed");
            }
            break;
        }
        case KIND_UNION: {
            i += cstr_copy(max_len - i, &buffer[i], "union ");

            if (type->record.name) {
                i += cstr_copy(max_len - i, &buffer[i], (char*)type->record.name);
            } else {
                i += cstr_copy(max_len - i, &buffer[i], "unnamed");
            }
            break;
        }
        case KIND_STRUCT: {
            i += cstr_copy(max_len - i, &buffer[i], "struct ");

            if (type->record.name) {
                i += cstr_copy(max_len - i, &buffer[i], (char*)type->record.name);
            } else {
                i += cstr_copy(max_len - i, &buffer[i], "unnamed");
            }
            break;
        }
        case KIND_PTR: {
            i += type_as_string(max_len - i, &buffer[i], cuik_canonical_type(type->ptr_to));
            buffer[i++] = '*';
            break;
        }
        case KIND_ARRAY: {
            i += type_as_string(max_len - i, &buffer[i], cuik_canonical_type(type->array_of));

            if (i + 12 < max_len) {
                buffer[i++] = '[';

                i += snprintf(&buffer[i], max_len - i, "%d", type->array_count);

                buffer[i++] = ']';
            } else {
                abort();
            }
            break;
        }
        case KIND_FUNC: {
            Param* param_list = type->func.param_list;
            size_t param_count = type->func.param_count;

            i += type_as_string(max_len - i, &buffer[i], cuik_canonical_type(type->func.return_type));

            assert(i < max_len);
            buffer[i++] = '(';

            for (size_t j = 0; j < param_count; j++) {
                if (j) buffer[i++] = ',';

                i += type_as_string(max_len - i, &buffer[i], cuik_canonical_type(param_list[j].type));
            }

            assert(i < max_len);
            buffer[i++] = ')';
            break;
        }
        case KIND_TYPEOF: {
            // TODO(NeGate): give some nicer look to this crap
            i += cstr_copy(max_len - i, &buffer[i], "typeof(?)");
            break;
        }
        default:
        abort();
    }

    if (type->also_known_as != NULL) {
        i += cstr_copy(max_len - i, &buffer[i], ")");
    }
    buffer[i] = '\0';
    return i;
}
