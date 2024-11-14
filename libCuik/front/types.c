#include <threads.h>
#include "parser.h"
#include "../targets/targets.h"

Cuik_Type cuik__builtin_void = { KIND_VOID,  0, 0, CUIK_TYPE_FLAG_COMPLETE };
Cuik_Type cuik__builtin_bool = { KIND_BOOL,  1, 1, CUIK_TYPE_FLAG_COMPLETE };
Cuik_Type cuik__builtin_float  = { KIND_FLOAT,  4, 4, CUIK_TYPE_FLAG_COMPLETE };
Cuik_Type cuik__builtin_double = { KIND_DOUBLE, 8, 8, CUIK_TYPE_FLAG_COMPLETE };

Cuik_TypeTable init_type_table(Cuik_Target* target) {
    Cuik_TypeTable t = { 0 };
    t.target = target;
    t.tracked = dyn_array_create(Cuik_Type*, 1024);
    return t;
}

// if track is false, it's not type checked later (because it's complete)
static Cuik_Type* type_alloc(Cuik_TypeTable* types, bool track) {
    Cuik_Type* t = tb_arena_alloc(types->arena, sizeof(Cuik_Type));
    if (track && types->tracked) {
        dyn_array_put(types->tracked, t);
    }
    return t;
}

static Cuik_Type* type_clone(Cuik_TypeTable* types, Cuik_Type* src, Atom new_name) {
    if (!CUIK_TYPE_IS_COMPLETE(src)) {
        Cuik_Type* cloned = type_alloc(types, true);
        *cloned = (Cuik_Type){
            .kind = KIND_CLONE,
            .loc = src->loc,
            .also_known_as = new_name,
            .clone.of = src
        };

        return cloned;
    }

    Cuik_Type* cloned = type_alloc(types, true);
    *cloned = *src;
    cloned->also_known_as = new_name;
    if (src->kind == KIND_STRUCT || src->kind == KIND_UNION) {
        cloned->record.nominal = src->record.nominal;
    }

    return cloned;
}

Cuik_Type* cuik__new_pointer(Cuik_TypeTable* types, Cuik_QualType base) {
    // TODO(NeGate): this code depends on pointers being 8bytes, plz stop doing that
    Cuik_Type* t = type_alloc(types, false);
    *t = (Cuik_Type){
        .kind   = KIND_PTR,
        .size   = types->target->pointer_byte_size,
        .align  = types->target->pointer_byte_size,
        .flags  = CUIK_TYPE_FLAG_COMPLETE,
        .ptr_to = base,
    };
    return t;
}

Cuik_Type* cuik__new_array(Cuik_TypeTable* types, Cuik_QualType base, int count) {
    Cuik_Type* canon = cuik_canonical_type(base);
    Cuik_Type* t = type_alloc(types, true);
    if (count == 0) {
        // these zero-sized arrays don't actually care about incomplete element types
        *t = (Cuik_Type){
            .kind = KIND_ARRAY,
            .size = 0,
            .align = canon->align,
            .array.of = base,
            .array.count = 0,
        };
        return t;
    }

    int size = canon->size;
    int align = canon->align;

    int dst;
    if (__builtin_mul_overflow(size, count, &dst)) {
        assert(0 && "Overflow on new_array with big inputs");
    }

    assert(align != 0);
    *t = (Cuik_Type){
        .kind = KIND_ARRAY,
        .size = dst,
        .align = align,
        .flags = CUIK_TYPE_FLAG_COMPLETE,
        .array.of = base,
        .array.count = count,
    };
    return t;
}

Cuik_Type* cuik__new_vector(Cuik_TypeTable* types, Cuik_QualType base, int count) {
    Cuik_Type* t = type_alloc(types, false);
    *t = (Cuik_Type){
        .kind = KIND_VECTOR,
        .size = cuik_canonical_type(base)->size * count,
        .align = cuik_canonical_type(base)->align,
        .flags = CUIK_TYPE_FLAG_COMPLETE,
        .vector.base = cuik_canonical_type(base),
        .vector.count = count,
    };
    return t;
}

Cuik_Type* cuik__new_vector2(Cuik_TypeTable* types, Cuik_Type* base, int count) {
    Cuik_Type* t = type_alloc(types, false);
    *t = (Cuik_Type){
        .kind = KIND_VECTOR,
        .size = base->size * count,
        .align = base->align,
        .flags = CUIK_TYPE_FLAG_COMPLETE,
        .vector.base = base,
        .vector.count = count,
    };
    return t;
}

// https://github.com/rui314/chibicc/blob/main/type.c
//
// NOTE(NeGate): this function is called within the IR gen
// code which is parallel so we need a mutex to avoid messing
// up the type arena.
Cuik_Type* get_common_type(Cuik_TypeTable* types, Cuik_Type* ty1, Cuik_Type* ty2) {
    // implictly convert arrays into pointers
    if (ty1->kind == KIND_ARRAY) {
        return cuik__new_pointer(types, ty1->array.of);
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
    } else if (ty1->kind == KIND_STRUCT || ty1->kind == KIND_UNION) {
        return (ty1->record.nominal == ty2->record.nominal);
    } else if (ty1->kind == KIND_PTR) {
        return type_equal(cuik_canonical_type(ty1->ptr_to), cuik_canonical_type(ty2->ptr_to));
    }

    // but by default kind matching is enough
    // like for integers, booleans and floats
    return true;
}

char* str_append(char* dst, const char* src, size_t len) {
    memcpy(dst, src, len);
    return dst + len;
}

char* cuik_typedef_str(char* dst, Cuik_Type* type, const char* name) {
    int depth = 0;
    Cuik_Type* base = type;
    while (base->kind == KIND_PTR) {
        base = cuik_canonical_type(base->ptr_to);
        depth++;
    }

    // write "unsigned"
    if ((type->kind >= KIND_BOOL && type->kind <= KIND_LLONG) && type->is_unsigned) {
        dst = str_append(dst, "unsigned ", sizeof("unsigned"));
    }

    if (type->kind == KIND_ARRAY) {

    } else if (type->kind == KIND_FUNC) {

    } else {
        const char* str = NULL;
        switch (type->kind) {
            case KIND_VOID:  str = "void ";      break;
            case KIND_BOOL:  str = "_Bool ";     break;
            case KIND_CHAR:  str = "char ";      break;
            case KIND_SHORT: str = "short ";     break;
            case KIND_INT:   str = "int ";       break;
            case KIND_LONG:  str = "long ";      break;
            case KIND_LLONG: str = "long long "; break;
            case KIND_FLOAT: str = "float ";     break;
            case KIND_DOUBLE:str = "double ";    break;
            case KIND_STRUCT:str = "struct ";    break;
            case KIND_UNION: str = "union ";     break;
            case KIND_ENUM:  str = "enum ";      break;
            default: abort();
        }

        if (str) {
            dst = str_append(dst, str, strlen(str));
            *dst++ = ' ';
        }

        if (type->kind == KIND_ARRAY || type->kind == KIND_FUNC) {
            *dst++ = '(';
        }

        while (depth--) {
            *dst++ = '*';
        }

        if (name) {
            dst = str_append(dst, name, strlen(name));
        }

        if (type->kind == KIND_ARRAY || type->kind == KIND_FUNC) {
            *dst++ = ')';
        }
    }

    if (type->kind == KIND_ARRAY) {
        *dst++ = '[';
        dst += snprintf(dst, 10, "%d", type->array.count);
        *dst++ = ']';
    } else if (type->kind == KIND_FUNC) {
        Param* param_list = type->func.param_list;
        size_t param_count = type->func.param_count;

        *dst++ = '(';
        for (size_t j = 0; j < param_count; j++) {
            if (j) *dst++ = ',';

            dst = cuik_typedef_str(dst, cuik_canonical_type(param_list[j].type), NULL);
        }
        *dst++ = ')';
    }

    return dst;
}

// returns the number of bytes written
static size_t cstr_copy(size_t len, char* dst, const char* src) {
    size_t i = 0;
    while (src[i]) {
        assert(i < len);

        dst[i] = src[i];
        i += 1;
    }
    return i;
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
        case KIND_VECTOR: {
            Cuik_Type* base = type->vector.base;
            switch (base->kind) {
                case KIND_INT:    i += snprintf(&buffer[i], max_len - i, "%cvec%d", base->is_unsigned ? 'u' : 'i', type->vector.count); break;
                case KIND_FLOAT:  i += snprintf(&buffer[i], max_len - i, "vec%d",   type->vector.count); break;
                case KIND_DOUBLE: i += snprintf(&buffer[i], max_len - i, "dvec%d",  type->vector.count); break;
                default: assert(0 && "TODO");
            }
            break;
        }
        case KIND_ARRAY: {
            i += type_as_string(max_len - i, &buffer[i], cuik_canonical_type(type->array.of));

            if (i + 12 < max_len) {
                buffer[i++] = '[';
                i += snprintf(&buffer[i], max_len - i, "%d", type->array.count);
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
