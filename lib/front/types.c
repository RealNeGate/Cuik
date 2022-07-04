#include <threads.h>
#include "parser.h"

Cuik_Type builtin_types[] = {
    // crap
    [TYPE_VOID] = {KIND_VOID, 0, 0},
    [TYPE_BOOL] = {KIND_BOOL, 1, 1},
    // signed
    [TYPE_CHAR] = {KIND_CHAR, 1, 1},
    [TYPE_SHORT] = {KIND_SHORT, 2, 2},
    [TYPE_INT] = {KIND_INT, 4, 4},
    [TYPE_LONG] = {KIND_LONG, 8, 8},
    // unsigned
    [TYPE_UCHAR] = {KIND_CHAR, 1, 1, .is_unsigned = true},
    [TYPE_USHORT] = {KIND_SHORT, 2, 2, .is_unsigned = true},
    [TYPE_UINT] = {KIND_INT, 4, 4, .is_unsigned = true},
    [TYPE_ULONG] = {KIND_LONG, 8, 8, .is_unsigned = true},
    // floats
    [TYPE_FLOAT] = {KIND_FLOAT, 4, 4},
    [TYPE_DOUBLE] = {KIND_DOUBLE, 8, 8},
};

static Cuik_Type* alloc_type(TranslationUnit* tu, const Cuik_Type* src) {
    mtx_lock(&tu->arena_mutex);
    Cuik_Type* dst = ARENA_ALLOC(&tu->type_arena, Cuik_Type);
    mtx_unlock(&tu->arena_mutex);

    memcpy(dst, src, sizeof(Cuik_Type));
    return dst;
}

Cuik_Type* new_enum(TranslationUnit* tu) {
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_ENUM,
        });
}

Cuik_Type* new_func(TranslationUnit* tu) {
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_FUNC,
            .size = 1,
            .align = 1,
        });
}

Cuik_Type* copy_type(TranslationUnit* tu, Cuik_Type* base) {
    return alloc_type(tu, base);
}

Cuik_Type* new_qualified_type(TranslationUnit* tu, Cuik_Type* base, int align, bool is_atomic, bool is_const) {
    assert(base != NULL);
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_QUALIFIED_TYPE,
            .size = base->size,
            .align = base->align,
            .qualified_ty = base,
            .is_atomic = is_atomic,
            .is_const = is_const
        });
}

Cuik_Type* new_record(TranslationUnit* tu, bool is_union) {
    return alloc_type(tu, &(Cuik_Type){
            .kind = is_union ? KIND_UNION : KIND_STRUCT,
        });
}

Cuik_Type* new_pointer(TranslationUnit* tu, Cuik_Type* base) {
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_PTR,
            .size = 8,
            .align = 8,
            .ptr_to = base,
        });
}

Cuik_Type* new_typeof(TranslationUnit* tu, Expr* src) {
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_TYPEOF,

            // ideally if you try using these it'll crash because things
            // do sanity checks on align, hopefully none trigger because
            // we resolve it properly.
            .size = 0,
            .align = 0,

            .typeof_.src = src,
        });
}

Cuik_Type* new_array(TranslationUnit* tu, Cuik_Type* base, int count) {
    if (count == 0) {
        // these zero-sized arrays don't actually care about incomplete element types
        return alloc_type(tu, &(Cuik_Type){
                .kind = KIND_ARRAY,
                .size = 0,
                .align = base->align,
                .array_of = base,
                .array_count = 0,
            });
    }

    int size = base->size;
    int align = base->align;

    int dst;
    if (__builtin_mul_overflow(size, count, &dst)) {
        assert(0 && "Overflow on new_array with big inputs");
    }

    assert(align != 0);
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_ARRAY,
            .size = dst,
            .align = align,
            .array_of = base,
            .array_count = count,
        });
}

Cuik_Type* new_vector(TranslationUnit* tu, Cuik_Type* base, int count) {
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_VECTOR,
            .size = 0,
            .align = base->align,
            .array_of = base,
            .array_count = 0,
        });
}

Cuik_Type* new_blank_type(TranslationUnit* tu) {
    return alloc_type(tu, &(Cuik_Type){
            .kind = KIND_PLACEHOLDER});
}

// https://github.com/rui314/chibicc/blob/main/type.c
//
// NOTE(NeGate): this function is called within the IR gen
// code which is parallel so we need a mutex to avoid messing
// up the type arena.
Cuik_Type* get_common_type(TranslationUnit* tu, Cuik_Type* ty1, Cuik_Type* ty2) {
    // implictly convert arrays into pointers
    if (ty1->kind == KIND_ARRAY) {
        return new_pointer(tu, ty1->array_of);
    }

    // implictly convert functions into function pointers
    if (ty1->kind == KIND_FUNC) {
        return new_pointer(tu, ty1);
    }

    if (ty2->kind == KIND_FUNC) {
        return new_pointer(tu, ty2);
    }

    // operations with floats promote up to floats
    // e.g. 1 + 2.0 = 3.0
    if (ty1->kind == KIND_DOUBLE || ty2->kind == KIND_DOUBLE)
        return &builtin_types[TYPE_DOUBLE];
    if (ty1->kind == KIND_FLOAT || ty2->kind == KIND_FLOAT)
        return &builtin_types[TYPE_FLOAT];

    // promote any small integral types into ints
    if (ty1->size < 4) ty1 = &builtin_types[TYPE_INT];
    if (ty2->size < 4) ty2 = &builtin_types[TYPE_INT];

    // if the types don't match pick the bigger one
    if (ty1->size != ty2->size) return (ty1->size < ty2->size) ? ty2 : ty1;

    // unsigned types are preferred
    if (ty2->kind >= KIND_CHAR && ty2->kind <= KIND_LONG && ty2->is_unsigned) return ty2;

    return ty1;
}

bool type_equal(TranslationUnit* tu, Cuik_Type* ty1, Cuik_Type* ty2) {
    if (ty1 == ty2) return true;

    while (ty1->kind == KIND_QUALIFIED_TYPE) ty1 = ty1->qualified_ty;
    while (ty2->kind == KIND_QUALIFIED_TYPE) ty2 = ty2->qualified_ty;

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
            if (!type_equal(tu, param_list1[i].type, param_list2[i].type)) {
                return false;
            }
        }

        return true;
    } else if (ty1->kind == KIND_PTR) {
        return type_equal(tu, ty1->ptr_to, ty2->ptr_to);
    }

    // but by default kind matching is enough
    // like for integers, booleans and floats
    return true;
}

size_t type_as_string(TranslationUnit* tu, size_t max_len, char* buffer, Cuik_Type* type) {
    if (type == NULL) {
        size_t i = cstr_copy(max_len, buffer, "(null)");
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
            i += type_as_string(tu, max_len - i, &buffer[i], type->ptr_to);
            buffer[i++] = '*';
            break;
        }
        case KIND_ARRAY: {
            i += type_as_string(tu, max_len - i, &buffer[i], type->array_of);

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

            i += type_as_string(tu, max_len - i, &buffer[i], type->func.return_type);

            assert(i < max_len);
            buffer[i++] = '(';

            for (size_t j = 0; j < param_count; j++) {
                if (j) buffer[i++] = ',';

                i += type_as_string(tu, max_len - i, &buffer[i], param_list[j].type);
            }

            assert(i < max_len);
            buffer[i++] = ')';
            break;
        }
        case KIND_TYPEOF: {
            // TODO(NeGate): give some nicer look to this crap
            i += cstr_copy(max_len - i, &buffer[i], "typeof(???)");
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
