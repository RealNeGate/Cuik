#include "sema.h"
#include "settings.h"

#include <back/ir_gen.h>
#include <stdarg.h>
#include <targets/targets.h>

#define SEMA_MUNCH_SIZE (131072)

typedef struct {
    // shared state, every run of sema_task will decrement this by one
    atomic_size_t* tasks_remaining;
    size_t start, end;
    TranslationUnit* tu;
} SemaTaskInfo;

// when you're not in the semantic phase, we don't
// rewrite the contents of the DOT and ARROW exprs
// because it may screw with things
thread_local bool in_the_semantic_phase;
static thread_local Stmt* function_stmt;

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];

void sema_stmt(TranslationUnit* tu, Stmt* restrict s);

static bool is_scalar_type(TranslationUnit* tu, Cuik_Type* type) {
    return (type->kind >= KIND_BOOL && type->kind <= KIND_FUNC);
}

static bool is_constant_zero(TranslationUnit* tu, const Expr* e) {
    return e->op == EXPR_INT && e->int_num.num == 0;
}

// doesn't do implicit casts
bool type_very_compatible(TranslationUnit* tu, Cuik_Type* src, Cuik_Type* dst) {
    if (src == dst) return true;
    if (src->kind != dst->kind) return false;

    switch (src->kind) {
        case KIND_BOOL:
        case KIND_CHAR:
        case KIND_SHORT:
        case KIND_INT:
        case KIND_LONG:
        return src->is_unsigned == dst->is_unsigned;

        case KIND_FLOAT:
        case KIND_DOUBLE:
        return true;

        case KIND_PTR:
        return type_very_compatible(tu, src->ptr_to, dst->ptr_to);
        case KIND_FUNC:
        return type_equal(tu, src, dst);

        case KIND_ARRAY:
        if (!type_very_compatible(tu, src->array_of, dst->array_of)) {
            return false;
        }
        return src->array_count == dst->array_count;

        default:
        return true;
    }
}

// Also checks if expression is an integer literal because we
// have a special case for 0 to pointer conversions.
bool type_compatible(TranslationUnit* tu, Cuik_Type* src, Cuik_Type* dst, Expr* a_expr) {
    if (src == dst) return true;

    // zero can convert into whatever
    if (a_expr->op == EXPR_INT && a_expr->int_num.num == 0 && is_scalar_type(tu, dst)) {
        return true;
    }

    // implictly convert arrays into pointers
    if (src->kind == KIND_ARRAY && dst->kind == KIND_PTR) {
        src = new_pointer(tu, src->array_of);
    }

    if (src->kind != dst->kind) {
        if (src->kind >= KIND_BOOL &&
            src->kind <= KIND_LONG &&
            dst->kind >= KIND_BOOL &&
            dst->kind <= KIND_LONG) {
            #if 0
            // we allow for implicit up-casts (char -> long)
            if (dst->kind >= src->kind) return true;
            else if (dst->kind == KIND_BOOL) return true;
            else if (a_expr->op == EXPR_INT) {
                // allow integer literals to represent any integer
                return true;
            }
            #else
            // just all integer casts are good
            return true;
            #endif
        } else if (src->kind >= KIND_BOOL &&
            src->kind <= KIND_LONG &&
            dst->kind == KIND_PTR) {
            if (a_expr->op == EXPR_INT &&
                a_expr->int_num.num == 0) {
                return true;
            }
        } else if (src->kind == KIND_FLOAT ||
            dst->kind == KIND_DOUBLE) {
            return true;
        } else if (src->kind == KIND_DOUBLE ||
            dst->kind == KIND_FLOAT) {
            return true;
        } else if (src->kind == KIND_PTR &&
            dst->kind == KIND_BOOL) {
            return true;
        } else if (src->kind == KIND_FUNC &&
            dst->kind == KIND_BOOL) {
            return true;
        } else if (src->kind == KIND_FUNC &&
            dst->kind == KIND_PTR) {
            if (dst->ptr_to->kind == KIND_FUNC) {
                return type_equal(tu, src, dst->ptr_to);
            }
        }

        return false;
    }

    if (src->kind == KIND_FUNC) {
        if (dst->kind == KIND_PTR && dst->ptr_to->kind == KIND_FUNC) {
            dst = dst->ptr_to;
        }

        return type_equal(tu, src, dst);
    } else if (src->kind == KIND_PTR) {
        // get base types
        while (src->kind == KIND_PTR) src = src->ptr_to;
        while (dst->kind == KIND_PTR) dst = dst->ptr_to;

        // void -> T is fine
        if (src->kind == KIND_VOID) {
            return true;
        }

        // T -> void is fine
        if (dst->kind == KIND_VOID) {
            return true;
        }

        return type_equal(tu, src, dst);
    }

    // but by default kind matching is enough
    // like for integers, booleans and floats
    return true;
}

static bool implicit_conversion(TranslationUnit* tu, Cuik_Type* src, Cuik_Type* dst, Expr* src_e) {
    // implictly convert arrays into pointers
    if (dst->kind == KIND_ARRAY) {
        dst = new_pointer(tu, dst->array_of);
    }

    if (warnings.data_loss) {
        // data loss warning applies to int and float conversions
        if (src->kind >= KIND_CHAR && src->kind <= KIND_DOUBLE &&
            dst->kind >= KIND_CHAR && dst->kind <= KIND_DOUBLE) {
            bool is_src_float = TYPE_IS_FLOAT(src);
            bool is_dst_float = TYPE_IS_FLOAT(dst);

            if (is_src_float == is_dst_float) {
                if (!is_src_float && src->is_unsigned != dst->is_unsigned) {
                    REPORT_EXPR(WARNING, src_e, "Implicit conversion %s signedness", src->is_unsigned ? "adds" : "drops");
                }

                if (src->kind > dst->kind) {
                    type_as_string(tu, sizeof(temp_string0), temp_string0, src);
                    type_as_string(tu, sizeof(temp_string1), temp_string1, dst);

                    REPORT_EXPR(WARNING, src_e, "Implicit conversion from '%s' to '%s' may lose data.", temp_string0, temp_string1);
                }
            } else {
                type_as_string(tu, sizeof(temp_string0), temp_string0, src);
                type_as_string(tu, sizeof(temp_string1), temp_string1, dst);

                REPORT_EXPR(WARNING, src_e, "Implicit conversion from '%s' to '%s' may lose data.", temp_string0, temp_string1);
            }
        }
    }

    if (!type_compatible(tu, src, dst, src_e)) {
        type_as_string(tu, sizeof(temp_string0), temp_string0, src);
        type_as_string(tu, sizeof(temp_string1), temp_string1, dst);

        REPORT_EXPR(ERROR, src_e, "could not implicitly convert type %s into %s.", temp_string0, temp_string1);
        return false;
    }

    return true;
}

bool cuik__type_check_args(TranslationUnit* tu, Expr* e, int arg_count, Expr** args) {
    bool failed = false;

    for (size_t i = 0; i < arg_count; i++) {
        Cuik_Type* arg_type = sema_expr(tu, args[i]);
        if (!implicit_conversion(tu, arg_type, args[i]->cast_type, args[i])) {
            failed = true;
        }
    }

    return !failed;
}

typedef struct {
    Member* member;
    int index;
    int offset;
} InitSearchResult;

// this figures out how many members are in one initializer's namespace
//   struct Foo {
//     struct {
//       int a, b;
//     };
//     int c;
//   };
//
// struct Foo would return 3 while
//   int a[6]
//
// would be 6 and scalars are just 1
static int compute_initializer_bounds(Cuik_Type* type) {
    // Identify boundaries:
    //   Scalar types are 1
    //   Records depend on the member count
    //   Arrays are based on array count
    //
    switch (type->kind) {
        case KIND_UNION:
        case KIND_STRUCT: {
            size_t bounds = 0;

            Member* kids = type->record.kids;
            size_t count = type->record.kid_count;

            // it should never be less than the original size since records
            // can't be empty
            bounds += count;

            for (size_t i = 0; i < count; i++) {
                Member* member = &kids[i];

                // unnamed members can be used
                if (member->name == NULL && (member->type->kind == KIND_STRUCT || member->type->kind == KIND_UNION)) {
                    bounds += compute_initializer_bounds(member->type) - 1;
                }
            }

            return bounds;
        }

        case KIND_ARRAY:
        return type->array_count;

        default:
        return 1;
    }
}

static InitSearchResult find_member_by_name(Cuik_Type* type, const char* name, int* base_index, int offset) {
    Member* kids = type->record.kids;
    size_t count = type->record.kid_count;

    for (size_t i = 0; i < count; i++) {
        Member* member = &kids[i];

        if (member->name != NULL) {
            if (cstr_equals(name, member->name)) {
                return (InitSearchResult){member, *base_index, offset + member->offset};
            }

            // only named members actually count to the indices
            *base_index += 1;
        } else if (member->type->kind == KIND_STRUCT || member->type->kind == KIND_UNION) {
            InitSearchResult search = find_member_by_name(member->type, name, base_index, offset + member->offset);
            if (search.member != NULL) {
                return search;
            }
        }
    }

    return (InitSearchResult){0};
}

static InitSearchResult get_next_member_in_type(Cuik_Type* type, int target, int* base_index, int offset, bool stop_at_struct) {
    Member* kids = type->record.kids;
    size_t count = type->record.kid_count;

    for (size_t i = 0; i < count; i++) {
        Member* member = &kids[i];

        // check kids
        if (member->name == NULL && (member->type->kind == KIND_STRUCT || member->type->kind == KIND_UNION)) {
            if (stop_at_struct && *base_index == target) {
                return (InitSearchResult){member, *base_index, offset + member->offset};
            }

            InitSearchResult search = get_next_member_in_type(
                member->type, target, base_index, offset + member->offset, stop_at_struct
            );

            if (search.member != NULL) {
                return search;
            }
        } else if (*base_index == target) {
            return (InitSearchResult){member, *base_index, offset + member->offset};
        }

        if (member->name != NULL) {
            // only named members actually count to the indices
            *base_index += 1;
        }
    }

    return (InitSearchResult){0};
}

static InitNode* walk_initializer_layer(
    TranslationUnit* tu, Cuik_Type* parent, int base_offset,
    int bounds /* max slots to fill */, InitNode* node, int* cursor,
    int* max_cursor, int* slots_left /* slots left in this layer */
) {
    ////////////////////////////////
    // manage any selectors
    ////////////////////////////////
    Cuik_Type* type = NULL;
    int relative_offset = 0;
    if (node->mode == INIT_MEMBER) {
        if (parent->kind != KIND_STRUCT && parent->kind != KIND_UNION) {
            type_as_string(tu, sizeof(temp_string0), temp_string0, parent);

            REPORT(ERROR, node->loc, "Member designator cannot be used on type %s", temp_string0);
            return NULL;
        }

        int index = 0;
        InitSearchResult search = find_member_by_name(parent, node->member_name, &index, 0);
        if (search.member == NULL) {
            return NULL;
        }

        type = search.member->type;
        relative_offset = search.offset;
        *cursor = search.index + 1;
    } else if (node->mode == INIT_ARRAY) {
        if (parent->kind != KIND_ARRAY) {
            type_as_string(tu, sizeof(temp_string0), temp_string0, parent);

            REPORT(ERROR, node->loc, "cannot apply array initializer to non-array (%s)", temp_string0);
            return NULL;
        }

        type = parent->array_of;
        relative_offset = node->start * type->size;
        *cursor = node->start + node->count;
    } else {
        *cursor += 1;
    }

    ////////////////////////////////
    // handle cursor
    ////////////////////////////////
    if (bounds > 0 && *cursor > bounds) {
        REPORT(ERROR, node->loc, "excess elements in initializer list (max %d)", bounds);
        return NULL;
    }

    // if it's a record then find the next member via weird tree walking
    // everything else is trivial
    if (type == NULL) {
        if (parent->kind == KIND_STRUCT || parent->kind == KIND_UNION) {
            int index = 0;
            InitSearchResult search = get_next_member_in_type(parent, *cursor - 1, &index, 0, node->kids_count > 0);
            assert(search.member != NULL);

            type = search.member->type;
            relative_offset = search.offset;
        } else if (parent->kind == KIND_ARRAY) {
            type = parent->array_of;
            relative_offset = (*cursor - 1) * type->size;
        } else {
            type = parent;
        }
    }

    if (*cursor > *max_cursor) {
        *max_cursor = *cursor;
    }

    // sometimes this is just not resolved yet?
    if (type->size == 0) type_layout(tu, type);

    uint32_t pos = base_offset + relative_offset;

    // store the byte position (relative to the root initializer) so it's
    // easier to do IR generation without reconstructing it
    node->offset = pos;
    node->type = type;

    ////////////////////////////////
    // type check its kids
    ////////////////////////////////
    // does it have brackets around the expressions?
    if (node->kids_count == 0) {
        Expr* e = node->expr;

        // if we try to initialize an array without brackets, it'll let us
        // access all the members without it.
        //
        // struct { int a[3]; } b;
        //
        // struct b f = { 1, 2, 3 };     valid
        // struct b f = { { 1 }, 2, 3 }; wrong
        //                ^^^^^
        //                this would be the array
        if (type->kind == KIND_ARRAY) {
            if (e != NULL) {
                if (e->op == EXPR_STR || e->op == EXPR_WSTR) {
                    Cuik_Type* expr_type = sema_expr(tu, e);

                    if (expr_type->kind == KIND_ARRAY && type->kind == KIND_ARRAY &&
                        type_equal(tu, expr_type->array_of, type->array_of)) {
                        // check if it fits properly
                        if (expr_type->array_count > type->array_count) {
                            REPORT_EXPR(ERROR, e, "initializer-string too big for the initializer (%d elements out of %d)", expr_type->array_count, type->array_count);
                        }

                        *slots_left -= 1;
                        return node + 1;
                    } else {
                        type_as_string(tu, sizeof(temp_string0), temp_string0, type->array_of);
                        REPORT_EXPR(ERROR, e, "Could not use %sinitializer-string on array of %s", (node->expr->op == EXPR_WSTR) ? "wide " : "", temp_string0);
                        return NULL;
                    }
                }
            }

            int array_count = type->array_count;
            if (array_count == 0) {
                while (*slots_left) {
                    node = walk_initializer_layer(
                        tu, type, pos, 0, node, cursor, max_cursor, slots_left
                    );

                    // failure so we unwind
                    if (node == NULL) return NULL;
                }
            } else {
                for (int i = 0; i < array_count && *slots_left; i++) {
                    node = walk_initializer_layer(
                        tu, type, pos, array_count, node, cursor, max_cursor, slots_left
                    );

                    // failure so we unwind
                    if (node == NULL) return NULL;
                }
            }

            return node;
        } /*else if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
            REPORT_EXPR(ERROR, e, "Cannot write initializer for struct/union without surrounding brackets");
            return node + 1;
        }*/ else {
            if (node->expr == NULL) {
                REPORT(ERROR, node->loc, "ASSERT");
                return NULL;
            }

            // normal ass scalar
            Cuik_Type* expr_type = sema_expr(tu, e);
            e = node->expr = cuik__optimize_ast(tu, e);

            // zero is allowed for everything, so don't do the normal checks in that case
            if (!(e->op == EXPR_INT && e->int_num.num == 0)) {
                // it throws it's own errors and we don't really need
                // any complex recovery for it since it'll exit at the
                // end of type checking so it's not like the error will
                // spread well
                implicit_conversion(tu, expr_type, type, e);
            }

            e->cast_type = type;
            *slots_left -= 1;
            return node + 1;
        }
    } else {
        // compound literals can be used on both scalars and aggregates.
        int kid_cursor = 0, kid_max_cursor, kid_slots_left = node->kids_count, node_count = node->kids_count;
        node += 1;

        if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
            // TODO(NeGate): unions will actually skip all the other entries once
            // you've picked once which is something we should keep in mind later
            int member_count = compute_initializer_bounds(type);
            for (size_t i = 0; i < node_count; i++) {
                node = walk_initializer_layer(
                    tu, type, pos, member_count, node, &kid_cursor, &kid_max_cursor, &kid_slots_left
                );

                // failure so we unwind
                if (node == NULL) return NULL;
            }
        } else if (type->kind == KIND_ARRAY) {
            int array_count = compute_initializer_bounds(type);
            for (int i = 0; i < node_count; i++) {
                node = walk_initializer_layer(
                    tu, type, pos, array_count, node, &kid_cursor, &kid_max_cursor, &kid_slots_left
                );

                // failure so we unwind
                if (node == NULL) return NULL;
            }
        } else {
            if (node->kids_count != 0) {
                REPORT(ERROR, node->loc, "cannot have multiple elements in scalar initalizer");
                return NULL;
            }

            // scalars
            node = walk_initializer_layer(
                tu, type, pos, 1, node, &kid_cursor, &kid_max_cursor, &kid_slots_left
            );
        }

        *slots_left -= 1;
        return node;
    }
}

static InitNode* sema_infer_initializer_array_count(TranslationUnit* tu, int node_count, InitNode* node, int depth, int* out_array_count) {
    size_t cursor = 0;
    size_t max = 0;

    for (int i = 0; i < node_count; i++) {
        if (depth == 0) {
            // members shouldn't be here :p
            if (node->mode == INIT_MEMBER) {
                return 0;
            } else if (node->mode == INIT_ARRAY) {
                cursor = node->start + node->count;
                if (cursor > max) max = cursor;
            } else if (node->mode == INIT_NONE) {
                cursor++;
                if (cursor > max) max = cursor;
            }
        }

        if (node->kids_count == 0) {
            node += 1;
        } else {
            node = sema_infer_initializer_array_count(tu, node->kids_count, node + 1, depth + 1, NULL);
        }
    }

    if (depth == 0) {
        assert(max == (int)max);

        *out_array_count = max;
    }
    return node;
}

static InitNode* walk_initializer_for_sema(TranslationUnit* tu, Cuik_Type* type, int node_count, InitNode* node, int base_offset) {
    /*if (type->kind == KIND_STRUCT &&
        type->record.name != NULL &&
        cstr_equals(type->record.name, "Expr")) __debugbreak();*/

    int cursor = 0, max_cursor = 0, slots_left = node_count, bounds = compute_initializer_bounds(type);
    if (bounds > 0) {
        for (int i = 0; i < bounds && slots_left; i++) {
            node = walk_initializer_layer(tu, type, 0, bounds, node, &cursor, &max_cursor, &slots_left);
        }
    } else {
        for (int i = 0; slots_left; i++) {
            node = walk_initializer_layer(tu, type, 0, bounds, node, &cursor, &max_cursor, &slots_left);
        }

        if (type->array_count == 0) {
            type->array_count = max_cursor;
            type_layout(tu, type);
        }
    }

    return node;
}

static void try_resolve_typeof(TranslationUnit* tu, Cuik_Type* ty) {
    if (ty->kind == KIND_TYPEOF) {
        // spoopy...
        *ty = *sema_expr(tu, ty->typeof_.src);
    }
}

static bool is_assignable_expr(TranslationUnit* tu, Expr* restrict e) {
    switch (e->op) {
        case EXPR_DEREF:
        case EXPR_SUBSCRIPT:
        case EXPR_ARROW:
        case EXPR_DOT:
        return true;

        case EXPR_SYMBOL:
        case EXPR_PARAM:
        // TODO(NeGate): const-check
        return true;

        default:
        return false;
    }
}

Member* sema_traverse_members(TranslationUnit* tu, Cuik_Type* record_type, Atom name, uint32_t* out_offset) {
    Member* kids = record_type->record.kids;
    size_t count = record_type->record.kid_count;

    for (size_t i = 0; i < count; i++) {
        Member* member = &kids[i];

        // TODO(NeGate): String interning would be nice
        if (member->name == NULL) {
            // unnamed fields are traversed as well
            Cuik_Type* child = member->type;
            assert(child->kind == KIND_STRUCT || child->kind == KIND_UNION);

            Member* search = sema_traverse_members(tu, child, name, out_offset);
            if (search) {
                *out_offset += member->offset;
                return search;
            }
        } else if (cstr_equals(name, member->name)) {
            *out_offset += member->offset;
            return member;
        }
    }

    return NULL;
}

Member* sema_resolve_member_access(TranslationUnit* tu, Expr* restrict e, uint32_t* out_offset) {
    bool is_arrow = (e->op == EXPR_ARROW);
    Cuik_Type* base_type = sema_expr(tu, e->dot_arrow.base);

    Cuik_Type* record_type = NULL;
    if (is_arrow) {
        if (base_type->kind != KIND_PTR && base_type->kind != KIND_ARRAY) {
            REPORT_EXPR(ERROR, e, "Cannot do arrow operator on non-pointer type.");
            return NULL;
        }

        record_type = base_type->ptr_to;
    } else {
        record_type = base_type;

        // Implicit dereference
        if (record_type->kind == KIND_PTR) {
            record_type = record_type->ptr_to;

            if (0 /* pedantic */) {
                REPORT_EXPR(ERROR, e, "Implicit dereference is a non-standard extension (disable -P to allow it).");
                return NULL;
            }
        }
    }

    if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
        REPORT_EXPR(ERROR, e, "Cannot get the member of a non-record type.");
        return NULL;
    }

    if (record_type->size == 0) {
        type_layout(tu, record_type);

        if (record_type->size == 0) {
            REPORT_EXPR(ERROR, e, "Cannot access members in incomplete type");
            return NULL;
        }
    }

    uint32_t offset = 0;
    Member* search = sema_traverse_members(tu, record_type, e->dot_arrow.name, &offset);
    if (search) {
        *out_offset += offset;
        return search;
    }

    type_as_string(tu, sizeof(temp_string0), temp_string0, record_type);
    REPORT_EXPR(ERROR, e->dot_arrow.base, "Could not find member called '%s' for type '%s'", e->dot_arrow.name, temp_string0);
    return NULL;
}

Cuik_Type* sema_expr(TranslationUnit* tu, Expr* restrict e) {
    switch (e->op) {
        case EXPR_UNKNOWN_SYMBOL: {
            return (e->type = &builtin_types[TYPE_VOID]);
        }
        case EXPR_VA_ARG: {
            Cuik_Type* va_list_type = sema_expr(tu, e->va_arg_.src);
            if (va_list_type->kind != KIND_PTR && va_list_type->ptr_to->kind != KIND_CHAR) {
                type_as_string(tu, sizeof(temp_string0), temp_string0, va_list_type);
                REPORT_EXPR(ERROR, e, "va_arg must take in a va_list in the first argument (got %s)", temp_string0);
            }

            Cuik_Type* type = e->va_arg_.type;
            int size = type->size;
            if (size < builtin_types[TYPE_INT].size) {
                REPORT_EXPR(WARNING, e, "Warning, va_arg used on a value smaller than int");
            }

            return (e->type = type);
        }
        case EXPR_INT: {
            switch (e->int_num.suffix) {
                case INT_SUFFIX_NONE: {
                    unsigned int original = (unsigned int)e->int_num.num;
                    unsigned long long expected = (unsigned long long)e->int_num.num;

                    if (original != expected) {
                        REPORT_EXPR(ERROR, e, "Could not represent integer literal as int. (%llu or %llx)", expected, expected);
                    }

                    return (e->type = &builtin_types[TYPE_INT]);
                }

                case INT_SUFFIX_U: {
                    unsigned int original = (unsigned int)e->int_num.num;
                    unsigned long long expected = (unsigned long long)e->int_num.num;

                    if (original != expected) {
                        REPORT_EXPR(ERROR, e, "Could not represent integer literal as unsigned int.");
                    }

                    return (e->type = &builtin_types[TYPE_UINT]);
                }

                case INT_SUFFIX_L:
                return (e->type = &builtin_types[tu->is_windows_long ? TYPE_INT : TYPE_LONG]);
                case INT_SUFFIX_UL:
                return (e->type = &builtin_types[tu->is_windows_long ? TYPE_UINT : TYPE_ULONG]);

                case INT_SUFFIX_LL:
                return (e->type = &builtin_types[TYPE_LONG]);
                case INT_SUFFIX_ULL:
                return (e->type = &builtin_types[TYPE_ULONG]);

                default:
                REPORT_EXPR(ERROR, e, "Could not represent integer literal.");
                return (e->type = &builtin_types[TYPE_VOID]);
            }
        }
        case EXPR_ENUM: {
            return (e->type = &builtin_types[TYPE_INT]);
        }
        case EXPR_FLOAT32: {
            return (e->type = &builtin_types[TYPE_FLOAT]);
        }
        case EXPR_FLOAT64: {
            return (e->type = &builtin_types[TYPE_DOUBLE]);
        }
        case EXPR_CHAR: {
            return (e->type = &builtin_types[TYPE_INT]);
        }
        case EXPR_WCHAR: {
            return (e->type = &builtin_types[TYPE_SHORT]);
        }
        case EXPR_WSTR: {
            if (e->str.resolved) {
                return e->type;
            }

            const char* in = (const char*)(e->str.start + 1);
            size_t len = ((const char*)e->str.end - 1) - in;

            // it can't be bigger than the original
            wchar_t* out = arena_alloc(&thread_arena, (len + 1) * 2, 1);

            size_t out_i = 0, in_i = 0;
            while (in_i < len) {
                int ch;
                intptr_t distance = parse_char(len - in_i, &in[in_i], &ch);
                if (distance < 0) abort();

                assert(ch < 0x80);
                out[out_i++] = ch;
                in_i += distance;
            }

            assert(out_i <= len);
            out[out_i++] = '\0';

            e->str.start = (unsigned char*)&out[0];
            e->str.end = (unsigned char*)&out[out_i];
            e->str.resolved = true;

            return (e->type = new_array(tu, &builtin_types[TYPE_SHORT], out_i));
        }
        case EXPR_STR: {
            if (e->str.resolved) {
                return e->type;
            }

            const char* in = (const char*)(e->str.start + 1);
            size_t len = ((const char*)e->str.end - 1) - in;

            // it can't be bigger than the original
            char* out = arena_alloc(&thread_arena, len + 1, 1);

            size_t out_i = 0, in_i = 0;
            while (in_i < len) {
                int ch;
                intptr_t distance = parse_char(len - in_i, &in[in_i], &ch);
                if (distance < 0) abort();

                out[out_i++] = ch;
                in_i += distance;
            }

            assert(out_i <= len);
            out[out_i++] = '\0';

            e->str.start = (unsigned char*)out;
            e->str.end = (unsigned char*)(out + out_i);
            e->str.resolved = true;

            return (e->type = new_array(tu, &builtin_types[TYPE_CHAR], out_i));
        }
        case EXPR_SIZEOF: {
            Cuik_Type* src = sema_expr(tu, e->x_of_expr.expr);

            //assert(src->size && "Something went wrong...");
            *e = (Expr){
                .op = EXPR_INT,
                .type = &builtin_types[TYPE_ULONG],
                .int_num = {src->size, INT_SUFFIX_ULL}};
            return (e->type = &builtin_types[TYPE_ULONG]);
        }
        case EXPR_ALIGNOF: {
            Cuik_Type* src = sema_expr(tu, e->x_of_expr.expr);

            //assert(src->align && "Something went wrong...");
            *e = (Expr){
                .op = EXPR_INT,
                .type = &builtin_types[TYPE_ULONG],
                .int_num = {src->align, INT_SUFFIX_ULL}};
            return (e->type = &builtin_types[TYPE_ULONG]);
        }
        case EXPR_SIZEOF_T: {
            try_resolve_typeof(tu, e->x_of_type.type);

            if (e->x_of_type.type->kind == KIND_FUNC) {
                REPORT_EXPR(WARNING, e, "sizeof(function type) is undefined (Cuik will always resolve it to 1)");
            }

            assert(e->x_of_type.type->size && "Something went wrong...");
            *e = (Expr){
                .op = EXPR_INT,
                .type = &builtin_types[TYPE_ULONG],
                .int_num = {e->x_of_type.type->size, INT_SUFFIX_NONE}};
            return (e->type = &builtin_types[TYPE_ULONG]);
        }
        case EXPR_ALIGNOF_T: {
            try_resolve_typeof(tu, e->x_of_type.type);

            if (e->x_of_type.type->kind == KIND_FUNC) {
                REPORT_EXPR(WARNING, e, "alignof(function type) is undefined (Cuik will always resolve it to 1)");
            }

            assert(e->x_of_type.type->align && "Something went wrong...");
            *e = (Expr){
                .op = EXPR_INT,
                .type = &builtin_types[TYPE_ULONG],
                .int_num = {e->x_of_type.type->align, INT_SUFFIX_NONE}};
            return (e->type = &builtin_types[TYPE_ULONG]);
        }
        case EXPR_FUNCTION: {
            // e->type is already set for this bad boy... maybe we
            // shouldn't do that for consistency sake...
            return e->type;
        }
        case EXPR_INITIALIZER: {
            try_resolve_typeof(tu, e->init.type);
            Cuik_Type* type = e->init.type;

            /*if (type->kind == KIND_ARRAY) {
                int old_array_count = type->array_count;

                // if it's 0, then it's unsized and anything goes
                if (old_array_count != 0) {
                    // verify that everything fits correctly
                    if (old_array_count < new_array_count) {
                        REPORT_EXPR(ERROR, e, "Array cannot fit into declaration (needs %d, got %d)", old_array_count, new_array_count);
                    }
                } else {
                    e->init.type = new_array(tu, type->array_of, new_array_count);
                }
            }*/

            walk_initializer_for_sema(tu, type, e->init.count, e->init.nodes, 0);
            return (e->type = e->init.type);
        }
        case EXPR_LOGICAL_NOT: {
            /* Cuik_Type* src = */ sema_expr(tu, e->unary_op.src);

            e->unary_op.src->cast_type = &builtin_types[TYPE_BOOL];
            return (e->type = &builtin_types[TYPE_BOOL]);
        }
        case EXPR_NOT:
        case EXPR_NEGATE:
        case EXPR_PRE_INC:
        case EXPR_PRE_DEC:
        case EXPR_POST_INC:
        case EXPR_POST_DEC: {
            Cuik_Type* src = sema_expr(tu, e->unary_op.src);

            e->unary_op.src->cast_type = src;
            return (e->type = src);
        }
        case EXPR_ADDR: {
            uint64_t dst;
            if (in_the_semantic_phase && const_eval_try_offsetof_hack(tu, e->unary_op.src, &dst)) {
                *e = (Expr){
                    .op = EXPR_INT,
                    .type = &builtin_types[TYPE_ULONG],
                    .int_num = {dst, INT_SUFFIX_ULL},
                };
                return &builtin_types[TYPE_ULONG];
            }

            Cuik_Type* src = sema_expr(tu, e->unary_op.src);
            return (e->type = new_pointer(tu, src));
        }
        case EXPR_SYMBOL: {
            Stmt* restrict sym = e->symbol;
            if (e->is_resolving_symbol) {
                REPORT_STMT(ERROR, sym, "cycle in symbol", sym->decl.name);
                return (e->type = &builtin_types[TYPE_VOID]);
            }

            if (sym->op == STMT_LABEL) {
                if (!sym->label.placed) {
                    REPORT_STMT(ERROR, sym, "label '%s' is never defined.", sym->label.name);
                }

                return (e->type = &builtin_types[TYPE_VOID]);
            } else {
                Cuik_Type* type = sym->decl.type;

                if (type->kind == KIND_ARRAY) {
                    if (type->size == 0 && sym->op == STMT_GLOBAL_DECL) {
                        e->is_resolving_symbol = true;

                        // try to resolve the type since it's incomplete
                        sema_stmt(tu, sym);

                        e->is_resolving_symbol = false;
                        type = sym->decl.type;
                        assert(type->size != 0 && "Uhh... we fucked up");
                    }

                    // this is the only *current* example where something sets
                    // it's own cast_type it's an exception to the rules.
                    e->cast_type = new_pointer(tu, type->array_of);
                }

                return (e->type = type);
            }
        }
        case EXPR_PARAM: {
            int param_num = e->param_num;

            Param* param_list = function_stmt->decl.type->func.param_list;
            return (e->type = param_list[param_num].type);
        }
        case EXPR_GENERIC: {
            Cuik_Type* src = sema_expr(tu, e->generic_.controlling_expr);

            // _Generic's controlling expression does rvalue conversions so
            // an array is treated as a pointer not an array
            if (src->kind == KIND_ARRAY) {
                src = new_pointer(tu, src->array_of);
            } else if (src->kind == KIND_FUNC) {
                src = new_pointer(tu, src);
            }

            Expr* default_case = 0;
            Expr* match = 0;

            for (size_t i = 0; i < e->generic_.case_count; i++) {
                if (e->generic_.cases[i].key == 0) {
                    default_case = e->generic_.cases[i].value;
                } else if (type_very_compatible(tu, e->generic_.cases[i].key, src)) {
                    match = e->generic_.cases[i].value;
                }
            }

            if (match == 0) {
                if (default_case == 0) {
                    // if we didn't match anything and there's no default case, error out
                    REPORT_EXPR(ERROR, e, "Could not match _Generic against any cases");
                    return 0;
                }

                e->generic_.controlling_expr = default_case;
            } else {
                e->generic_.controlling_expr = match;
            }

            // once we set case_count to 0, we've resolved the _Generic
            e->generic_.cases = NULL;
            e->generic_.case_count = 0;

            return (e->type = sema_expr(tu, e->generic_.controlling_expr));
        }
        case EXPR_CAST: {
            try_resolve_typeof(tu, e->cast.type);

            /* Cuik_Type* src = */ sema_expr(tu, e->cast.src);

            // set child's cast type
            e->cast.src->cast_type = e->cast.type;
            return (e->type = e->cast.type);
        }
        case EXPR_SUBSCRIPT: {
            Cuik_Type* base = sema_expr(tu, e->subscript.base);
            Cuik_Type* index = sema_expr(tu, e->subscript.index);

            if (index->kind == KIND_PTR ||
                index->kind == KIND_ARRAY) {
                SWAP(base, index);
                SWAP(e->subscript.base, e->subscript.index);
            }

            if (base->kind == KIND_ARRAY) {
                base = new_pointer(tu, base->array_of);
            }

            if (base->kind != KIND_PTR) {
                type_as_string(tu, sizeof(temp_string0), temp_string0, base);
                REPORT_EXPR(ERROR, e, "Cannot perform subscript [] with base type '%s'", temp_string0);
                return (e->type = NULL);
            }

            e->subscript.base->cast_type = base;
            e->subscript.index->cast_type = &builtin_types[TYPE_LONG];
            return (e->type = base->ptr_to);
        }
        case EXPR_DEREF: {
            Cuik_Type* base = sema_expr(tu, e->unary_op.src);
            e->unary_op.src->cast_type = base;

            if (base->kind == KIND_PTR) {
                return (e->type = base->ptr_to);
            } else if (base->kind == KIND_ARRAY) {
                return (e->type = base->array_of);
            } else {
                type_as_string(tu, sizeof(temp_string0), temp_string0, base);
                REPORT_EXPR(ERROR, e, "Cannot dereference from non-pointer and non-array type (%s)", temp_string0);
                abort();
            }
        }
        case EXPR_CALL: {
            if (e->call.target->op == EXPR_BUILTIN_SYMBOL) {
                const char* name = (const char*)e->call.target->builtin_sym.name;

                Expr** args = e->call.param_start;
                int arg_count = e->call.param_count;

                Cuik_Type* ty = tu->target.arch->type_check_builtin(tu, e, name, arg_count, args);
                if (ty == NULL) ty = &builtin_types[TYPE_VOID];

                return (e->type = ty);
            }

            // Call function
            Cuik_Type* func_type = sema_expr(tu, e->call.target);

            // implicit dereference
            if (func_type->kind == KIND_PTR) {
                func_type = func_type->ptr_to;
            }

            e->call.target->cast_type = func_type;

            if (func_type->kind != KIND_FUNC) {
                type_as_string(tu, sizeof(temp_string0), temp_string0, func_type);

                REPORT_EXPR(ERROR, e->call.target, "function call target must be a function-type, got %s", temp_string0);
                goto failure;
            }

            Expr** args = e->call.param_start;
            int arg_count = e->call.param_count;

            Param* params = func_type->func.param_list;
            int param_count = func_type->func.param_count;

            if (func_type->func.has_varargs) {
                if (arg_count < param_count) {
                    REPORT_EXPR(ERROR, e, "Not enough arguments (expected at least %d, got %d)", param_count, arg_count);
                    goto failure;
                }

                // type-check the parameters with a known type
                for (size_t i = 0; i < param_count; i++) {
                    Cuik_Type* arg_type = sema_expr(tu, args[i]);

                    if (!implicit_conversion(tu, arg_type, params[i].type, args[i])) {
                        continue;
                    }

                    args[i]->cast_type = params[i].type;
                }

                // type-check the untyped arguments
                for (size_t i = param_count; i < arg_count; i++) {
                    Cuik_Type* src = sema_expr(tu, args[i]);

                    // all integers ranked lower than int are promoted to int
                    if (src->kind >= KIND_BOOL && src->kind < KIND_INT) {
                        src = &builtin_types[TYPE_INT];
                    }

                    // all floats ranked lower than double are promoted to double
                    if (src->kind == KIND_FLOAT) {
                        src = &builtin_types[TYPE_DOUBLE];
                    }

                    args[i]->cast_type = src;
                }
            } else {
                if (arg_count != param_count) {
                    REPORT_EXPR(ERROR, e, "Argument count mismatch (expected %d, got %d)", param_count, arg_count);
                    goto failure;
                }

                for (size_t i = 0; i < arg_count; i++) {
                    Cuik_Type* arg_type = sema_expr(tu, args[i]);

                    if (!implicit_conversion(tu, arg_type, params[i].type, args[i])) {
                        continue;
                    }

                    args[i]->cast_type = params[i].type;
                }
            }

            failure:
            return (e->type = func_type->func.return_type);
        }
        case EXPR_TERNARY: {
            Cuik_Type* cond_type = sema_expr(tu, e->ternary_op.left);
            if (!is_scalar_type(tu, cond_type)) {
                type_as_string(tu, sizeof(temp_string0), temp_string0, cond_type);

                REPORT_EXPR(ERROR, e, "Could not convert type %s into boolean.", temp_string0);
            }
            e->ternary_op.left->cast_type = &builtin_types[TYPE_BOOL];

            Cuik_Type* ty1 = sema_expr(tu, e->ternary_op.middle);
            Cuik_Type* ty2 = sema_expr(tu, e->ternary_op.right);

            // if either side is a zero then it's malleable
            if (!is_constant_zero(tu, e->ternary_op.middle) &&
                !is_constant_zero(tu, e->ternary_op.right)) {
                implicit_conversion(tu, ty1, ty2, e->ternary_op.middle);
            }

            Cuik_Type* type = NULL;
            if (ty1->kind == KIND_STRUCT || ty1->kind == KIND_UNION) {
                type = ty1;
            } else {
                type = get_common_type(tu, ty1, ty2);
            }

            e->ternary_op.middle->cast_type = type;
            e->ternary_op.right->cast_type = type;

            return (e->type = type);
        }
        case EXPR_COMMA: {
            sema_expr(tu, e->bin_op.left);

            return (e->type = sema_expr(tu, e->bin_op.right));
        }
        case EXPR_DOT:
        case EXPR_ARROW: {
            uint32_t offset = 0;
            Member* m = sema_resolve_member_access(tu, e, &offset);
            if (m != NULL) {
                if (in_the_semantic_phase) {
                    e->dot_arrow.base->cast_type = sema_expr(tu, e->dot_arrow.base);

                    // resolved
                    e->op = (e->op == EXPR_DOT ? EXPR_DOT_R : EXPR_ARROW_R);
                    e->dot_arrow.member = m;
                    e->dot_arrow.offset = offset;
                }

                return (e->type = m->type);
            }

            return (e->type = &builtin_types[TYPE_VOID]);
        }
        case EXPR_PTRADD:
        case EXPR_PTRSUB:
        case EXPR_PTRDIFF:
        case EXPR_DOT_R:
        case EXPR_ARROW_R: {
            return e->type;
        }
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR: {
            sema_expr(tu, e->bin_op.left);
            sema_expr(tu, e->bin_op.right);

            e->bin_op.left->cast_type = &builtin_types[TYPE_BOOL];
            e->bin_op.right->cast_type = &builtin_types[TYPE_BOOL];

            return (e->type = &builtin_types[TYPE_BOOL]);
        }
        case EXPR_PLUS:
        case EXPR_MINUS:
        case EXPR_TIMES:
        case EXPR_SLASH:
        case EXPR_PERCENT:
        case EXPR_AND:
        case EXPR_OR:
        case EXPR_XOR:
        case EXPR_SHL:
        case EXPR_SHR: {
            Cuik_Type* lhs = sema_expr(tu, e->bin_op.left);
            Cuik_Type* rhs = sema_expr(tu, e->bin_op.right);

            if ((e->op == EXPR_PLUS ||
                    e->op == EXPR_MINUS) &&
                (lhs->kind == KIND_PTR ||
                    lhs->kind == KIND_ARRAY ||
                    rhs->kind == KIND_PTR ||
                    rhs->kind == KIND_ARRAY)) {
                // Pointer arithmatic
                if (e->op == EXPR_PLUS && (rhs->kind == KIND_PTR || rhs->kind == KIND_ARRAY)) {
                    SWAP(lhs, rhs);
                    SWAP(e->bin_op.left, e->bin_op.right);
                }

                if (rhs->kind == KIND_PTR || rhs->kind == KIND_ARRAY) {
                    if (e->op == EXPR_MINUS) {
                        // ptr - ptr = ptrdiff_t
                        e->bin_op.left->cast_type = lhs;
                        e->bin_op.right->cast_type = rhs;

                        e->op = EXPR_PTRDIFF;
                        return (e->type = &builtin_types[TYPE_LONG]);
                    } else {
                        REPORT_EXPR(ERROR, e, "Cannot do pointer addition with two pointer operands, one must be an integral type.");
                        return (e->type = &builtin_types[TYPE_VOID]);
                    }
                } else {
                    e->bin_op.left->cast_type = lhs;
                    e->bin_op.right->cast_type = &builtin_types[TYPE_ULONG];

                    if (lhs->ptr_to->size == 0) {
                        REPORT_EXPR(ERROR, e, "Cannot do pointer arithmatic on incomplete type");
                    }

                    e->op = (e->op == EXPR_PLUS) ? EXPR_PTRADD : EXPR_PTRSUB;
                    return (e->type = lhs);
                }
            } else {
                if (!(lhs->kind >= KIND_BOOL &&
                        lhs->kind <= KIND_DOUBLE &&
                        rhs->kind >= KIND_BOOL &&
                        rhs->kind <= KIND_DOUBLE)) {
                    type_as_string(tu, sizeof(temp_string0), temp_string0, lhs);
                    type_as_string(tu, sizeof(temp_string1), temp_string1, rhs);

                    REPORT_EXPR(ERROR, e, "Cannot apply binary operator to %s and %s.", temp_string0, temp_string1);
                    return (e->type = &builtin_types[TYPE_VOID]);
                }

                Cuik_Type* type = get_common_type(tu, lhs, rhs);

                // Do we actually need to check both sides?
                implicit_conversion(tu, lhs, type, e->bin_op.left);
                implicit_conversion(tu, rhs, type, e->bin_op.right);

                e->bin_op.left->cast_type = type;
                e->bin_op.right->cast_type = type;

                return (e->type = type);
            }
        }
        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE: {
            Cuik_Type* type = get_common_type(tu,
                sema_expr(tu, e->bin_op.left),
                sema_expr(tu, e->bin_op.right));

            e->bin_op.left->cast_type = type;
            e->bin_op.right->cast_type = type;

            return (e->type = &builtin_types[TYPE_BOOL]);
        }
        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN: {
            if (!is_assignable_expr(tu, e->bin_op.left)) {
                REPORT_EXPR(WARNING, e->bin_op.left, "Left-hand side is not assignable");

                e->bin_op.left->cast_type = 0;
                e->bin_op.right->cast_type = 0;
                return (e->type = 0);
            }

            Cuik_Type* lhs = sema_expr(tu, e->bin_op.left);
            sema_expr(tu, e->bin_op.right);

            e->bin_op.left->cast_type = lhs;
            e->bin_op.right->cast_type = lhs;

            return (e->type = lhs);
        }
        default:
        break;
    }

    abort();
}

void sema_stmt(TranslationUnit* tu, Stmt* restrict s) {
    if (s == NULL) return;

    switch (s->op) {
        case STMT_NONE:
        break;
        case STMT_LABEL:
        break;
        case STMT_GOTO: {
            s->goto_.target->cast_type = sema_expr(tu, s->goto_.target);
            break;
        }
        case STMT_COMPOUND: {
            Stmt** kids = s->compound.kids;
            size_t count = s->compound.kids_count;

            Stmt* killer = 0;
            for (size_t i = 0; i < count; i++) {
                Stmt* kid = kids[i];
                sema_stmt(tu, kid);

                if (killer) {
                    if (kid->op == STMT_LABEL ||
                        kid->op == STMT_CASE ||
                        kid->op == STMT_DEFAULT) {
                        killer = 0;
                    } else {
                        REPORT_STMT(ERROR, kid, "Dead code");
                        REPORT_STMT(INFO, killer, "After");
                        goto compound_failure;
                    }
                } else {
                    if (kid->op == STMT_RETURN ||
                        kid->op == STMT_GOTO ||
                        kid->op == STMT_BREAK ||
                        kid->op == STMT_CONTINUE) {
                        killer = kid;
                    }
                }
            }

            compound_failure:
            break;
        }
        // global decl is only resolved here in the rare occasion where
        // const_eval is needing to resolve a type early
        case STMT_GLOBAL_DECL:
        case STMT_DECL: {
            if (s->decl.initial) {
                try_resolve_typeof(tu, s->decl.type);

                if (s->decl.initial->op == EXPR_INITIALIZER &&
                    s->decl.initial->init.type == 0) {
                    // give it something to go off of
                    s->decl.initial->init.type = s->decl.type;
                }

                Cuik_Type* decl_type = s->decl.type;
                Cuik_Type* expr_type = sema_expr(tu, s->decl.initial);

                Expr* e = s->decl.initial;
                if (e->op == EXPR_INITIALIZER) {
                    // Auto-detect array count from initializer
                    if (decl_type->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
                        if (decl_type->array_count != 0 && decl_type->array_count < expr_type->array_count) {
                            REPORT_STMT(ERROR, s, "Array initializer does not fit into declaration (expected %d, got %d)", decl_type->array_count, expr_type->array_count);
                        } else {
                            s->decl.type = expr_type;
                        }
                    }
                } else if (e->op == EXPR_STR || e->op == EXPR_WSTR) {
                    // Auto-detect array count from string
                    if (decl_type->kind == KIND_ARRAY && decl_type->array_count == 0) {
                        s->decl.type = expr_type;
                    }
                }

                e->cast_type = s->decl.type;
                if (!type_compatible(tu, expr_type, s->decl.type, s->decl.initial)) {
                    type_as_string(tu, sizeof(temp_string0), temp_string0, expr_type);
                    type_as_string(tu, sizeof(temp_string1), temp_string1, s->decl.type);

                    REPORT_STMT(ERROR, s, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
                }

                // constant fold the global expression such that it's easier to spot constant
                // expressions.
                s->decl.initial = cuik__optimize_ast(tu, s->decl.initial);
            }
            break;
        }
        case STMT_EXPR: {
            s->expr.expr->cast_type = sema_expr(tu, s->expr.expr);
            s->expr.expr = cuik__optimize_ast(tu, s->expr.expr);
            break;
        }
        case STMT_RETURN: {
            if (s->return_.expr) {
                Cuik_Type* expr_type = sema_expr(tu, s->return_.expr);
                Cuik_Type* return_type = function_stmt->decl.type->func.return_type;

                if (!type_compatible(tu, expr_type, return_type, s->return_.expr)) {
                    REPORT_EXPR(ERROR, s->return_.expr, "Value in return statement does not match function signature.");
                }

                s->return_.expr->cast_type = return_type;
            }
            break;
        }
        case STMT_IF: {
            Cuik_Type* cond_type = sema_expr(tu, s->if_.cond);
            if (!is_scalar_type(tu, cond_type)) {
                type_as_string(tu, sizeof(temp_string0), temp_string0, cond_type);

                REPORT_STMT(ERROR, s, "Could not convert type %s into boolean.", temp_string0);
            }
            s->if_.cond->cast_type = &builtin_types[TYPE_BOOL];

            sema_stmt(tu, s->if_.body);
            if (s->if_.next) {
                sema_stmt(tu, s->if_.next);
            }
            break;
        }
        case STMT_WHILE: {
            sema_expr(tu, s->while_.cond);
            s->while_.cond->cast_type = &builtin_types[TYPE_BOOL];

            if (s->while_.body) {
                sema_stmt(tu, s->while_.body);
            }
            break;
        }
        case STMT_DO_WHILE: {
            if (s->do_while.body) {
                sema_stmt(tu, s->do_while.body);
            }

            sema_expr(tu, s->do_while.cond);
            s->do_while.cond->cast_type = &builtin_types[TYPE_BOOL];
            break;
        }
        case STMT_FOR: {
            if (s->for_.first) {
                sema_stmt(tu, s->for_.first);
            }

            if (s->for_.cond) {
                sema_expr(tu, s->for_.cond);
                s->for_.cond->cast_type = &builtin_types[TYPE_BOOL];
            }

            if (s->for_.body) {
                sema_stmt(tu, s->for_.body);
            }

            if (s->for_.next) {
                s->for_.next->cast_type = sema_expr(tu, s->for_.next);
            }
            break;
        }
        case STMT_SWITCH: {
            Cuik_Type* type = sema_expr(tu, s->switch_.condition);
            s->switch_.condition->cast_type = type;

            if (!(type->kind >= KIND_CHAR && type->kind <= KIND_LONG)) {
                type_as_string(tu, sizeof(temp_string0), temp_string0, type);

                REPORT_STMT(ERROR, s, "Switch case type must be an integral type, got a '%s'", type);
                break;
            }

            sema_stmt(tu, s->switch_.body);
            break;
        }
        case STMT_CASE: {
            if (s->case_.body != NULL) {
                while (s->case_.body->op == STMT_CASE) {
                    s = s->case_.body;
                }

                sema_stmt(tu, s->case_.body);
            }
            break;
        }
        case STMT_DEFAULT: {
            sema_stmt(tu, s->default_.body);
            break;
        }
        case STMT_CONTINUE:
        case STMT_BREAK: {
            break;
        }

        default:
        assert(0);
    }
}

Cuik_Type* sema_guess_type(TranslationUnit* tu, Stmt* restrict s) {
    char* name = (char*)s->decl.name;

    Cuik_Type* type = s->decl.type;

    if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
        REPORT_STMT(ERROR, s, "Global declaration '%s' cannot be both static and extern.", name);
        return NULL;
    }

    if (type->is_incomplete) {
        if (type->kind == KIND_STRUCT) {
            REPORT_STMT(ERROR, s, "Incomplete type (struct %s) in declaration", type->record.name);
        } else if (type->kind == KIND_UNION) {
            REPORT_STMT(ERROR, s, "Incomplete type (union %s) in declaration", type->record.name);
        } else {
            REPORT_STMT(ERROR, s, "Incomplete type in declaration");
        }
    }

    if (s->decl.attrs.is_extern || type->kind == KIND_FUNC) {
        return NULL;
    }

    if (s->decl.initial) {
        Expr* e = s->decl.initial;

        if (type->kind == KIND_ARRAY && e->op == EXPR_INITIALIZER) {
            // check how many top level statements we have
            int array_count;
            sema_infer_initializer_array_count(tu, e->init.count, e->init.nodes, 0, &array_count);

            return new_array(tu, type->array_of, array_count);
        }
    }

    return s->decl.type;
}

static void sema_top_level(TranslationUnit* tu, Stmt* restrict s) {
    Cuik_Type* type = s->decl.type;

    char* name = (char*)s->decl.name;
    switch (s->op) {
        case STMT_FUNC_DECL: {
            assert(type->kind == KIND_FUNC);

            #ifdef CUIK_USE_TB
            s->backing.f = 0;
            #endif

            if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
                REPORT_STMT(ERROR, s, "Function '%s' cannot be both static and extern.", name);
                break;
            }

            if (s->decl.attrs.is_static && !s->decl.attrs.is_inline) {
                if (warnings.unused_funcs && !s->decl.attrs.is_used) {
                    REPORT(WARNING, s->loc, "Function '%s' is never used.", name);
                    break;
                }
            }

            if (s->decl.attrs.is_inline && !s->decl.attrs.is_used) {
                break;
            }

            #ifdef CUIK_USE_TB
            if (tu->ir_mod == NULL) {
                s->backing.f = 0;

                // type check function body
                function_stmt = s;
                sema_stmt(tu, s->decl.initial_as_stmt);
                function_stmt = 0;
                break;
            } else {
                TB_FunctionPrototype* proto = tu->target.arch->create_prototype(tu, type);
                TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;

                // TODO(NeGate): Fix this up because it's possibly wrong, essentially
                // inline linkage means all the definitions must match which isn't
                // necessarily the same as static where they all can share a name but
                // are different and internal.
                TB_Function* func;
                if (s->decl.attrs.is_inline) {
                    linkage = TB_LINKAGE_PRIVATE;

                    char temp[1024];
                    snprintf(temp, 1024, "_K%d_%s", tu->id_gen++, name ? name : "<unnamed>");

                    func = tb_prototype_build(tu->ir_mod, proto, temp, linkage);
                } else {
                    func = tb_prototype_build(tu->ir_mod, proto, name, linkage);
                }
                s->backing.f = func;

                // type check function body
                function_stmt = s;
                sema_stmt(tu, s->decl.initial_as_stmt);
                function_stmt = 0;
            }
            #else
            // type check function body
            function_stmt = s;
            sema_stmt(tu, s->decl.initial_as_stmt);
            function_stmt = 0;
            #endif /* CUIK_USE_TB */
            break;
        }
        case STMT_DECL:
        case STMT_GLOBAL_DECL: {
            if (name == NULL) break;
            if (!s->decl.attrs.is_used) break;
            if (s->decl.attrs.is_typedef) break;

            #ifdef CUIK_USE_TB
            s->backing.g = 0;
            #endif

            if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
                REPORT_STMT(ERROR, s, "Global declaration '%s' cannot be both static and extern.", name);
                break;
            }

            bool is_external_sym = (type->kind == KIND_FUNC && s->decl.initial_as_stmt == NULL);
            if (s->decl.attrs.is_extern) is_external_sym = true;

            if (type->kind != KIND_FUNC && s->decl.initial) {
                // constant fold the global expression such that it's easier to spot constant
                // expressions.
                s->decl.initial = cuik__optimize_ast(tu, s->decl.initial);
            }

            if (!is_external_sym) {
                if (s->decl.initial) {
                    if (s->decl.initial->op == EXPR_INITIALIZER &&
                        s->decl.initial->init.type == 0) {
                        // give it something to go off of
                        //
                        // doesn't have to be complete in terms of array count
                        // just enough to infer the rest in a sec
                        s->decl.initial->init.type = s->decl.type;
                    }

                    Cuik_Type* expr_type = sema_expr(tu, s->decl.initial);

                    if (s->decl.initial->op == EXPR_INITIALIZER ||
                        s->decl.initial->op == EXPR_STR ||
                        s->decl.initial->op == EXPR_WSTR) {
                        if (type->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
                            if (type_equal(tu, type->array_of, expr_type->array_of)) {
                                if (type->array_count != 0 && type->array_count < expr_type->array_count) {
                                    REPORT_STMT(ERROR, s, "Array initializer does not fit into declaration (expected %d, got %d)", type->array_count, expr_type->array_count);
                                } else {
                                    assert(expr_type->array_count);

                                    // preserve constness
                                    bool is_const = type->is_const;

                                    type = copy_type(tu, expr_type);
                                    type->is_const = is_const;

                                    s->decl.type = type;
                                }
                            } else {
                                type_as_string(tu, sizeof(temp_string0), temp_string0, expr_type->array_of);
                                type_as_string(tu, sizeof(temp_string1), temp_string1, type->array_of);

                                REPORT_STMT(ERROR, s, "Array initializer type mismatch (got '%s', expected '%s')", temp_string0, temp_string1);
                            }
                        }
                    }

                    if (!type_compatible(tu, expr_type, type, s->decl.initial)) {
                        type_as_string(tu, sizeof(temp_string0), temp_string0, type);
                        type_as_string(tu, sizeof(temp_string1), temp_string1, expr_type);

                        REPORT_STMT(ERROR, s, "Declaration type does not match (got '%s', expected '%s')", temp_string0, temp_string1);
                    }
                }

                if (type->is_incomplete) {
                    if (type->kind == KIND_STRUCT) {
                        REPORT_STMT(ERROR, s, "Incomplete type (struct %s) in declaration", type->record.name);
                    } else if (type->kind == KIND_UNION) {
                        REPORT_STMT(ERROR, s, "Incomplete type (union %s) in declaration", type->record.name);
                    } else {
                        REPORT_STMT(ERROR, s, "Incomplete type in declaration");
                    }
                }

                #ifdef CUIK_USE_TB
                if (tu->ir_mod != NULL) {
                    // if we have a TB module, fill it up with declarations
                    if (s->decl.attrs.is_tls && !atomic_flag_test_and_set(&irgen_defined_tls_index)) {
                        tb_module_set_tls_index(tu->ir_mod, tb_extern_create(tu->ir_mod, "_tls_index"));
                    }

                    TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
                    s->backing.g = tb_global_create(tu->ir_mod, name, s->decl.attrs.is_tls ? TB_STORAGE_TLS : TB_STORAGE_DATA, linkage);
                }
                #endif
            }
            break;
        }
        default:
        assert(0);
    }
}

static void sema_mark_children(TranslationUnit* tu, Expr* restrict e) {
    if (e->op == EXPR_BUILTIN_SYMBOL) return;

    assert(e->op == EXPR_SYMBOL);
    Stmt* restrict s = e->symbol;

    if (s->op == STMT_FUNC_DECL || s->op == STMT_DECL || s->op == STMT_GLOBAL_DECL) {
        if (!s->decl.attrs.is_used) {
            s->decl.attrs.is_used = true;
            Expr* sym = s->decl.first_symbol;

            while (sym != NULL) {
                sema_mark_children(tu, sym);
                sym = sym->next_symbol_in_chain;
            }
        }
    }
}

static void sema_task(void* arg) {
    SemaTaskInfo task = *((SemaTaskInfo*)arg);

    CUIK_TIMED_BLOCK("sema: %zu-%zu", task.start, task.end) {
        in_the_semantic_phase = true;

        for (size_t i = task.start; i < task.end; i++) {
            sema_top_level(task.tu, task.tu->top_level_stmts[i]);
        }

        in_the_semantic_phase = false;
        *task.tasks_remaining -= 1;
    }
}

void cuik__sema_pass(TranslationUnit* restrict tu, Cuik_IThreadpool* restrict thread_pool) {
    tls_init();
    size_t count = arrlen(tu->top_level_stmts);

    // simple mark and sweep to remove unused symbols
    CUIK_TIMED_BLOCK("sema: collection") {
        for (size_t i = 0; i < count; i++) {
            Stmt* restrict s = tu->top_level_stmts[i];
            assert(s->op == STMT_FUNC_DECL || s->op == STMT_DECL || s->op == STMT_GLOBAL_DECL);

            if (s->decl.attrs.is_root) {
                s->decl.attrs.is_used = true;

                Expr* sym = s->decl.first_symbol;
                while (sym != NULL) {
                    sema_mark_children(tu, sym);
                    sym = sym->next_symbol_in_chain;
                }
            }
        }
    }

    // go through all top level statements and type check
    CUIK_TIMED_BLOCK("sema: type check") {
        if (thread_pool != NULL) {
            // disabled until we change the tables to arenas
            size_t padded = (count + (SEMA_MUNCH_SIZE - 1)) & ~(SEMA_MUNCH_SIZE - 1);

            // passed to the threads to identify when things are done
            atomic_size_t tasks_remaining = (count + (SEMA_MUNCH_SIZE - 1)) / SEMA_MUNCH_SIZE;

            for (size_t i = 0; i < padded; i += SEMA_MUNCH_SIZE) {
                size_t limit = i + SEMA_MUNCH_SIZE;
                if (limit > count) limit = count;

                SemaTaskInfo* task = tls_push(sizeof(SemaTaskInfo));
                *task = (SemaTaskInfo){
                    .tasks_remaining = &tasks_remaining,
                    .start = i,
                    .end = limit,
                    .tu = tu
                };

                CUIK_CALL(thread_pool, submit, sema_task, task);
            }

            while (tasks_remaining != 0) {
                thrd_yield();
            }
        } else {
            in_the_semantic_phase = true;
            for (size_t i = 0; i < count; i++) {
                sema_top_level(tu, tu->top_level_stmts[i]);
            }
            in_the_semantic_phase = false;
        }
    }
}
