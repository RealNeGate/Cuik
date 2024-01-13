#include "sema.h"

#include "../back/ir_gen.h"
#include "../targets/targets.h"

thread_local Stmt* cuik__sema_function_stmt;

void sema_stmt(TranslationUnit* tu, Stmt* restrict s);

static bool is_scalar_type(TranslationUnit* tu, Cuik_Type* type) {
    return (type->kind >= KIND_BOOL && type->kind <= KIND_ARRAY);
}

static bool is_constant_zero(const Subexpr* e) {
    return e->op == EXPR_INT && e->int_lit.lit == 0;
}

const char* cuik_stmt_decl_name(Stmt* stmt) {
    assert(stmt->op == STMT_DECL || stmt->op == STMT_FUNC_DECL || stmt->op == STMT_GLOBAL_DECL);
    return stmt->decl.name;
}

static Subexpr* get_root_subexpr(Cuik_Expr* e) {
    return e ? &e->exprs[e->count - 1] : NULL;
}

static Cuik_QualType get_root_type(Cuik_Expr* e) {
    return e ? e->types[e->count - 1] : CUIK_QUAL_TYPE_NULL;
}

static Cuik_QualType get_root_cast(Cuik_Expr* e) {
    return e ? e->cast_types[e->count - 1] : CUIK_QUAL_TYPE_NULL;
}

static void set_root_cast(Cuik_Expr* e, Cuik_QualType ty) {
    e->cast_types[e->count - 1] = ty;
}

static void set_expr_type(Cuik_Expr* e, Subexpr* s, Cuik_QualType ty) {
    e->types[s - e->exprs] = ty;
}

static void set_expr_cast(Cuik_Expr* e, Subexpr* s, Cuik_QualType ty) {
    e->cast_types[s - e->exprs] = ty;
}

Cuik_QualType cuik_stmt_decl_type(Stmt* stmt) {
    assert(stmt->op == STMT_DECL || stmt->op == STMT_FUNC_DECL || stmt->op == STMT_GLOBAL_DECL);
    return stmt->decl.type;
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
        case KIND_LLONG:
        return src->is_unsigned == dst->is_unsigned;

        case KIND_FLOAT:
        case KIND_DOUBLE:
        return true;

        case KIND_PTR:
        return type_very_compatible(tu, cuik_canonical_type(src->ptr_to), cuik_canonical_type(dst->ptr_to));
        case KIND_FUNC:
        return type_equal(src, dst);

        case KIND_ARRAY:
        if (!type_very_compatible(tu, cuik_canonical_type(src->array.of), cuik_canonical_type(dst->array.of))) {
            return false;
        }
        return src->array.count == dst->array.count;

        default:
        return true;
    }
}

// Also checks if expression is an integer literal because we
// have a special case for 0 to pointer conversions.
bool type_compatible(TranslationUnit* tu, Cuik_Type* src, Cuik_Type* dst, Subexpr* a_expr) {
    if (src == dst) return true;

    // zero can convert into whatever
    if (a_expr->op == EXPR_INT && a_expr->int_lit.lit == 0 && is_scalar_type(tu, dst)) {
        return true;
    }

    // implictly convert arrays into pointers
    if (src->kind == KIND_ARRAY && dst->kind == KIND_PTR) {
        src = cuik__new_pointer(&tu->types, src->array.of);
    }

    if (src->kind != dst->kind) {
        if (cuik_type_is_integer(src) && cuik_type_is_integer(dst)) {
            // just all integer casts are good... for now
            return true;
        } else if (cuik_type_is_integer(src) && cuik_type_is_pointer(dst)) {
            if (a_expr->op == EXPR_INT && a_expr->int_lit.lit == 0) {
                return true;
            }
        } else if (cuik_type_is_scalar(src) && cuik_type_is_scalar(dst)) {
            return true;
        } else if (src->kind == KIND_FUNC && dst->kind == KIND_PTR) {
            Cuik_Type* dst_ptr_to = cuik_canonical_type(dst->ptr_to);

            if (dst_ptr_to->kind == KIND_FUNC) {
                return type_equal(src, dst_ptr_to);
            }
        }

        return false;
    }

    if (src->kind == KIND_FUNC) {
        if (dst->kind == KIND_PTR) {
            Cuik_Type* dst_ptr_to = cuik_canonical_type(dst->ptr_to);

            if (dst_ptr_to->kind == KIND_FUNC) dst = dst_ptr_to;
        }

        return type_equal(src, dst);
    } else if (src->kind == KIND_PTR) {
        // get base types
        while (src->kind == KIND_PTR && dst->kind == KIND_PTR) {
            src = cuik_canonical_type(src->ptr_to);
            dst = cuik_canonical_type(dst->ptr_to);
        }

        // void -> T is fine
        if (src->kind == KIND_VOID) {
            return true;
        }

        // T -> void is fine
        if (dst->kind == KIND_VOID) {
            return true;
        }

        return type_equal(src, dst);
    }

    // but by default kind matching is enough
    // like for integers, booleans and floats
    return true;
}

static bool implicit_conversion(TranslationUnit* tu, Cuik_QualType qsrc, Cuik_QualType qdst, Subexpr* src_e) {
    Cuik_Type* src = cuik_canonical_type(qsrc);
    Cuik_Type* dst = cuik_canonical_type(qdst);

    // Compare qualifiers
    /*if (cuik_get_quals(qsrc) != cuik_get_quals(qdst)) {
        // TODO(NeGate): fix up the qualifier printing in the diag_err
        diag_err(&tu->tokens, src_e->loc, "could not implicitly convert type %!T into %!T (qualifier mismatch)", src, dst);
        return false;
    }*/

    // implictly convert functions & arrays into pointers
    if (dst->kind == KIND_FUNC) {
        dst = cuik__new_pointer(&tu->types, cuik_uncanonical_type(dst));
    } else if (dst->kind == KIND_ARRAY) {
        dst = cuik__new_pointer(&tu->types, dst->array.of);
    }

    if (tu->warnings->data_loss) {
        // data loss warning applies to int and float conversions
        if (src->kind >= KIND_CHAR && src->kind <= KIND_DOUBLE &&
            dst->kind >= KIND_CHAR && dst->kind <= KIND_DOUBLE) {
            bool is_src_float = cuik_type_is_float(src);
            bool is_dst_float = cuik_type_is_float(dst);

            if (is_src_float == is_dst_float) {
                if (!is_src_float && src->is_unsigned != dst->is_unsigned) {
                    diag_warn(&tu->tokens, src_e->loc, "implicit conversion %s signedness", src->is_unsigned ? "adds" : "drops");
                }

                if (src->kind > dst->kind) {
                    diag_warn(&tu->tokens, src_e->loc, "implicit conversion from %!T to %!T may lose data", src, dst);
                }
            } else {
                diag_warn(&tu->tokens, src_e->loc, "implicit conversion from %!T to %!T may lose data", src, dst);
            }
        }
    }

    if (!type_compatible(tu, src, dst, src_e)) {
        diag_err(&tu->tokens, src_e->loc, "can't implicitly convert expression");
        diag_extra(&tu->tokens, "got:  %!T", src);
        diag_extra(&tu->tokens, "need: %!T", dst);
        return false;
    }

    return true;
}

typedef struct {
    Member* member;
    int index;
    int offset;

    int next_index;
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
                Cuik_Type* type = cuik_canonical_type(member->type);

                // unnamed members can be used
                if (member->name == NULL && (type->kind == KIND_STRUCT || type->kind == KIND_UNION)) {
                    bounds += compute_initializer_bounds(type) - 1;
                }
            }

            return bounds;
        }

        case KIND_ARRAY:
        return type->array.count;

        default:
        return 1;
    }
}

static InitSearchResult find_member_by_name(Cuik_Type* type, const char* name, int* base_index, int offset) {
    Member* kids = type->record.kids;
    size_t count = type->record.kid_count;

    for (size_t i = 0; i < count; i++) {
        Member* member = &kids[i];
        Cuik_Type* type = cuik_canonical_type(member->type);

        if (member->name != NULL) {
            if (strcmp(name, member->name) == 0) {
                return (InitSearchResult){member, *base_index, offset + member->offset};
            }

            // only named members actually count to the indices
            *base_index += 1;
        } else if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
            InitSearchResult search = find_member_by_name(type, name, base_index, offset + member->offset);
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
        Cuik_Type* type = cuik_canonical_type(member->type);

        // check kids
        if (member->name == NULL && (type->kind == KIND_STRUCT || type->kind == KIND_UNION)) {
            if (stop_at_struct && *base_index == target) {
                return (InitSearchResult){ member, *base_index, offset + member->offset };
            }

            int base = *base_index;
            InitSearchResult search = get_next_member_in_type(
                type, target, base_index, offset + member->offset, stop_at_struct
            );

            if (search.member != NULL) {
                if (type->kind == KIND_UNION) search.next_index = base + compute_initializer_bounds(type);
                return search;
            }
        } else if (*base_index == target) {
            return (InitSearchResult){ member, *base_index, offset + member->offset, *base_index + 1 };
        }

        if (member->name != NULL) {
            // only named members actually count to the indices
            *base_index += 1;
        }
    }

    return (InitSearchResult){ 0 };
}

static int walk_initializer_layer(TranslationUnit* tu, Cuik_Type* parent, int base_offset, int bounds /* max slots to fill */, InitNode* node, int* cursor, int* max_cursor) {
    ////////////////////////////////
    // manage any selectors
    ////////////////////////////////
    Cuik_Type* type = NULL;
    int relative_offset = 0;
    if (node->mode == INIT_MEMBER) {
        if (parent->kind != KIND_STRUCT && parent->kind != KIND_UNION) {
            diag_err(&tu->tokens, parent->loc, "Member designator cannot be used on type %!T", parent);
            return 0;
        }

        int index = 0;
        InitSearchResult search = find_member_by_name(parent, node->member_name, &index, 0);
        if (search.member == NULL) {
            diag_err(&tu->tokens, node->loc, "could not find member '%s' in record", node->member_name);
            return 0;
        }

        type = cuik_canonical_type(search.member->type);
        relative_offset = search.offset;
        *cursor = search.next_index;
    } else if (node->mode == INIT_ARRAY) {
        if (parent->kind != KIND_ARRAY) {
            diag_err(&tu->tokens, node->loc, "cannot apply array initializer to non-array %!T", parent);
            return 0;
        }

        type = cuik_canonical_type(parent->array.of);
        relative_offset = node->start * type->size;
        *cursor = node->start + node->count;
    } else {
        *cursor += 1;
    }

    ////////////////////////////////
    // handle cursor
    ////////////////////////////////
    if (bounds > 0 && *cursor > bounds) {
        diag_err(&tu->tokens, node->loc, "excess items in list (limit %d, got %d)", bounds, *cursor);
        return 0;
    }

    // if it's a record then find the next member via weird tree walking
    // everything else is trivial
    if (type == NULL) {
        if (parent->kind == KIND_STRUCT || parent->kind == KIND_UNION) {
            int index = 0;
            InitSearchResult search = get_next_member_in_type(parent, *cursor - 1, &index, 0, node->kids_count > 0);
            assert(search.member != NULL);

            type = cuik_canonical_type(search.member->type);
            relative_offset = search.offset;
            *cursor = search.next_index;
        } else if (parent->kind == KIND_ARRAY) {
            type = cuik_canonical_type(parent->array.of);
            relative_offset = (*cursor - 1) * type->size;
        } else {
            type = parent;
        }
    }

    if (*cursor > *max_cursor) {
        *max_cursor = *cursor;
    }

    // sometimes this is just not resolved yet?
    if (type->size == 0) {
        type_layout2(NULL, &tu->tokens, type);
    }

    uint32_t pos = base_offset + relative_offset;

    // store the byte position (relative to the root initializer) so it's
    // easier to do IR generation without reconstructing it
    node->offset = pos;
    node->type = cuik_uncanonical_type(type);

    ////////////////////////////////
    // type check its kids
    ////////////////////////////////
    // does it have brackets around the expressions?
    if (node->kids_count == 0) {
        Cuik_Expr* expr = node->expr;

        Cuik_QualType expr_qtype = cuik__sema_expr(tu, expr);
        Cuik_Type* expr_type = cuik_canonical_type(expr_qtype);
        Subexpr* e = &expr->exprs[expr->count - 1];

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
                    if (expr_type->kind == KIND_ARRAY && type->kind == KIND_ARRAY &&
                        type_equal(cuik_canonical_type(expr_type->array.of), cuik_canonical_type(type->array.of))) {
                        // check if it fits properly
                        if (expr_type->array.count == type->array.count + 1) {
                            // we chop off the null terminator
                            e->str.end -= (e->op == EXPR_STR ? 1 : 2);
                            expr_type->array.count = type->array.count;
                        } else if (expr_type->array.count > type->array.count) {
                            diag_err(&tu->tokens, e->loc, "initializer-string too big for the initializer (%d elements out of %d)", expr_type->array.count, type->array.count);
                        }
                    } else {
                        diag_err(&tu->tokens, e->loc, "Could not use %sinitializer-string on array of %!T", (e->op == EXPR_WSTR) ? "wide " : "", cuik_canonical_type(type->array.of));
                    }
                }
            }
        } else {
            /*if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
              diag_err(&tu->tokens, e->loc, "Cannot write initializer for struct/union without surrounding brackets");
              return node + 1;
            }*/
            assert(node->expr);

            Cuik_QualType cast_type = CUIK_QUAL_TYPE_NULL;
            if ((e->op == EXPR_STR  && cuik_canonical_type(node->type)->kind == KIND_CHAR) ||
                (e->op == EXPR_WSTR && cuik_canonical_type(node->type)->kind == KIND_SHORT)) {
                // { "hello" } can be used when the initializer is an array because reasons
                cast_type = node->type = expr_qtype;
            } else if (!(e->op == EXPR_INT && e->int_lit.lit == 0)) {
                // zero is allowed for everything, so don't do the normal checks in that case
                //
                // it throws it's own errors and we don't really need
                // any complex recovery for it since it'll exit at the
                // end of type checking so it's not like the error will
                // spread well
                implicit_conversion(tu, cuik_uncanonical_type(expr_type), node->type, e);
                cast_type = node->type;
            } else {
                cast_type = node->type;
            }

            expr->cast_types[expr->count - 1] = cast_type;
        }
    } else {
        // compound literals can be used on both scalars and aggregates.
        int kid_cursor = 0, kid_max_cursor, kid_slots_left = node->kids_count, node_count = node->kids_count;

        InitNode* n = node->kid;
        if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
            // TODO(NeGate): unions will actually skip all the other entries once
            // you've picked once which is something we should keep in mind later
            int member_count = compute_initializer_bounds(type);
            for (size_t i = 0; i < node_count; i++) {
                assert(n != NULL);

                walk_initializer_layer(tu, type, pos, member_count, n, &kid_cursor, &kid_max_cursor);
                n = n->next;
            }
        } else if (type->kind == KIND_ARRAY) {
            int array_count = compute_initializer_bounds(type);
            for (int i = 0; i < node_count; i++) {
                assert(n != NULL);

                walk_initializer_layer(tu, type, pos, array_count, n, &kid_cursor, &kid_max_cursor);
                n = n->next;
            }
        } else {
            if (node->kids_count != 0) {
                diag_err(&tu->tokens, node->loc, "cannot have multiple elements in scalar initalizer");
                return 0;
            }

            // scalars
            walk_initializer_layer(tu, type, pos, 1, node, &kid_cursor, &kid_max_cursor);
        }
    }

    return 1;
}

static size_t sema_infer_initializer_array_count(TranslationUnit* tu, InitNode* root_node) {
    size_t cursor = 0, max = 0;
    for (InitNode* n = root_node->kid; n != NULL; n = n->next) {
        if (n->mode == INIT_MEMBER) {
            // members shouldn't be here :p
            break;
        } else if (n->mode == INIT_ARRAY) {
            cursor = n->start + n->count;
            if (cursor > max) max = cursor;
        } else if (n->mode == INIT_NONE) {
            Subexpr* e = get_root_subexpr(n->expr);
            if (e != NULL && (e->op == EXPR_STR || e->op == EXPR_WSTR)) {
                Cuik_Type* src = cuik_canonical_type(cuik__sema_expr(tu, n->expr));
                cursor += src ? src->array.count : 1;
            } else {
                cursor++;
            }

            if (cursor > max) max = cursor;
        }
    }

    return max;
}

static void walk_initializer_for_sema(TranslationUnit* tu, Cuik_Type* type, InitNode* root, int base_offset) {
    InitNode* n = root->kid;
    int cursor = 0, max_cursor = 0, bounds = compute_initializer_bounds(type);

    while (n != NULL) {
        walk_initializer_layer(tu, type, 0, bounds, n, &cursor, &max_cursor);
        n = n->next;
    }

    if (type->array.count == 0) {
        type->array.count = max_cursor;
        type_layout2(NULL, &tu->tokens, type);
    }
}

static bool is_assignable_expr(Subexpr* e) {
    switch (e->op) {
        case EXPR_DEREF:
        case EXPR_SUBSCRIPT:
        case EXPR_ARROW:
        case EXPR_DOT:
        case EXPR_ARROW_R:
        case EXPR_DOT_R:
        case EXPR_SYMBOL:
        case EXPR_PARAM:
        return true;

        default:
        return false;
    }
}

Member* sema_traverse_members(Cuik_Type* record_type, Atom name, uint32_t* out_offset) {
    Member* kids = record_type->record.kids;
    size_t count = record_type->record.kid_count;

    for (size_t i = 0; i < count; i++) {
        Member* member = &kids[i];

        if (member->name == NULL) {
            // unnamed fields are traversed as well
            Cuik_Type* child = cuik_canonical_type(member->type);
            assert(child->kind == KIND_STRUCT || child->kind == KIND_UNION);

            Member* search = sema_traverse_members(child, name, out_offset);
            if (search) {
                *out_offset += member->offset;
                return search;
            }
        } else if (name == member->name) {
            *out_offset += member->offset;
            return member;
        }
    }

    return NULL;
}

static Cuik_Type* get_record_type(TranslationUnit* tu, Cuik_Type* base_type, SourceRange loc, bool is_arrow) {
    if (is_arrow) {
        if (base_type->kind != KIND_PTR && base_type->kind != KIND_ARRAY) {
            diag_err(&tu->tokens, loc, "Cannot do arrow operator on non-pointer type.");
            return NULL;
        }

        return cuik_canonical_type(base_type->ptr_to);
    } else {
        // Implicit dereference
        if (base_type->kind == KIND_PTR) {
            if (0 /* pedantic */) {
                diag_err(&tu->tokens, loc, "Implicit dereference is a non-standard extension (disable -P to allow it).");
                return NULL;
            }

            return cuik_canonical_type(base_type->ptr_to);
        }

        return base_type;
    }
}

#define SET_CAST(i, ty) (_->cast_types[args[i]] = (ty))
#define GET_TYPE(i)     (_->types[args[i]])
#define GET_EXPR(i)     (_->exprs[args[i]])

static Cuik_Type* builtin_char_type(Cuik_Type* target_signed_ints, char ch) {
    switch (ch) {
        case 'v': return &cuik__builtin_void;
        case 'b': return &cuik__builtin_bool;
        case 'c': return &target_signed_ints[CUIK_BUILTIN_CHAR];
        case 's': return &target_signed_ints[CUIK_BUILTIN_SHORT];
        case 'i': return &target_signed_ints[CUIK_BUILTIN_INT];
        case 'l': return &target_signed_ints[CUIK_BUILTIN_LONG];
        case 'L': return &target_signed_ints[CUIK_BUILTIN_LLONG];
        default:  return NULL;
    }
}

static Cuik_Type* sema_builtin(TranslationUnit* tu, Cuik_Expr* restrict _, const char* format, int arg_count, size_t* args) {
    Cuik_Type* target_signed_ints = tu->target->signed_ints;
    Cuik_Type* t_type = NULL;

    // parameters (if there's a T we need to deduce it)
    for (int i = 1; i < arg_count && *format != ' '; i++) {
        char ch = *format++;
        if (ch == '.') {
            format -= 1;
            SET_CAST(i, GET_TYPE(i));
            continue;
        } else if (ch == 'v' && *format == ' ') {
            break;
        }

        Subexpr* arg = &GET_EXPR(i);
        Cuik_QualType arg_type = GET_TYPE(i);

        Cuik_Type* expected = builtin_char_type(target_signed_ints, ch);

        // pointer types
        int expected_level = 0;
        while (*format == '*') {
            expected_level += 1, format += 1;
        }

        if (ch == 'T') {
            // deduce T
            if (t_type == NULL) {
                t_type = cuik_canonical_type(arg_type);

                for (int i = 0; i < expected_level; i++) {
                    if (t_type->kind != KIND_PTR) {
                        diag_err(&tu->tokens, arg->loc, "expected pointer for argument %!T", t_type);
                        return NULL;
                    }

                    t_type = cuik_canonical_type(t_type->ptr_to);
                }

                SET_CAST(i, GET_TYPE(i));
                continue;
            } else {
                expected = t_type;
            }
        } else if (ch == 'C') {
            // it's a constant integer, anything goes
            if (!cuik_type_is_integer(cuik_canonical_type(arg_type))) {
                diag_err(&tu->tokens, arg->loc, "expected integer for argument, got %!T", cuik_canonical_type(arg_type));
                return NULL;
            }

            SET_CAST(i, GET_TYPE(i));
            continue;
        }

        int level;
        Cuik_Type* base = cuik_canonical_type(cuik_get_direct_type(arg_type, &level));

        if (level == 0) {
            if (!type_compatible(tu, base, expected, arg)) {
                diag_err(&tu->tokens, arg->loc, "argument type doesn't match parameter type (got %!T, expected %!T)", base, expected);
                return NULL;
            }

            SET_CAST(i, arg_type);
        } else {
            if (expected->kind != KIND_VOID && !type_equal(base, expected)) {
                diag_err(&tu->tokens, arg->loc, "pointer argument's base type doesn't match parameter's (got %!T, expected %!T)", base, expected);
                return NULL;
            }

            for (size_t j = 0; j < level; j++) {
                expected = cuik__new_pointer(&tu->types, cuik_uncanonical_type(expected));
            }

            SET_CAST(i, cuik_uncanonical_type(expected));
        }

        if (level != expected_level) {
            diag_err(&tu->tokens, arg->loc, "pointer indirection mismatch (got %d, expected %d)", level, expected_level);
            return NULL;
        }
    }

    if (*format == '.') {
        format++;
    }

    assert(*format == ' ');
    format++;

    // pointer types
    Cuik_Type* ret_type = builtin_char_type(target_signed_ints, *format);
    if (*format == 'T') ret_type = t_type;

    while (*format == '*') {
        ret_type = cuik__new_pointer(&tu->types, cuik_uncanonical_type(ret_type));
        format++;
    }

    // parse return type
    return ret_type;
}

Cuik_QualType cuik__sema_subexpr(TranslationUnit* tu, Cuik_Expr* restrict _, Subexpr* restrict e, int arity, size_t* args) {
    switch (e->op) {
        case EXPR_INT: {
            const Cuik_Type* target_signed_ints = tu->target->signed_ints;
            const Cuik_Type* target_unsigned_ints = tu->target->unsigned_ints;

            switch (e->int_lit.suffix) {
                case INT_SUFFIX_NONE: {
                    unsigned int original = (unsigned int)e->int_lit.lit;
                    unsigned long long expected = (unsigned long long)e->int_lit.lit;

                    long long signed_form = expected;
                    if (signed_form > 0 && signed_form != (int) e->int_lit.lit) {
                        return cuik_uncanonical_type(&target_unsigned_ints[CUIK_BUILTIN_INT]);
                    } else if (original != expected) {
                        // diag_err(&tu->tokens, e->loc, "Could not represent integer literal as int. (%llu or %llx)", expected, expected);
                        return cuik_uncanonical_type(&target_signed_ints[CUIK_BUILTIN_LLONG]);
                    }

                    return cuik_uncanonical_type(&target_signed_ints[CUIK_BUILTIN_INT]);
                }

                case INT_SUFFIX_U: {
                    unsigned int original = (unsigned int)e->int_lit.lit;
                    unsigned long long expected = (unsigned long long)e->int_lit.lit;

                    if (original != expected) {
                        // diag_err(&tu->tokens, e->loc, "Could not represent integer literal as unsigned int.");
                        return cuik_uncanonical_type(&target_unsigned_ints[CUIK_BUILTIN_LLONG]);
                    }

                    return cuik_uncanonical_type(&target_unsigned_ints[CUIK_BUILTIN_INT]);
                }

                case INT_SUFFIX_L:   return cuik_uncanonical_type(&target_signed_ints[CUIK_BUILTIN_LONG]);
                case INT_SUFFIX_UL:  return cuik_uncanonical_type(&target_unsigned_ints[CUIK_BUILTIN_LONG]);
                case INT_SUFFIX_LL:  return cuik_uncanonical_type(&target_signed_ints[CUIK_BUILTIN_LLONG]);
                case INT_SUFFIX_ULL: return cuik_uncanonical_type(&target_unsigned_ints[CUIK_BUILTIN_LLONG]);

                default:
                diag_err(&tu->tokens, e->loc, "could not represent integer literal.");
                return CUIK_QUAL_TYPE_NULL;
            }
        }
        case EXPR_ENUM:        return cuik_uncanonical_type(&tu->target->signed_ints[CUIK_BUILTIN_INT]);
        case EXPR_FLOAT32:     return cuik_uncanonical_type(&cuik__builtin_float);
        case EXPR_FLOAT64:     return cuik_uncanonical_type(&cuik__builtin_double);
        case EXPR_CHAR:        return cuik_uncanonical_type(&tu->target->signed_ints[CUIK_BUILTIN_INT]);
        case EXPR_WCHAR:       return cuik_uncanonical_type(&tu->target->signed_ints[CUIK_BUILTIN_SHORT]);
        case EXPR_CONSTRUCTOR: return cuik_uncanonical_type(e->constructor.type);

        case EXPR_CAST: {
            // set child's cast type
            SET_CAST(0, e->cast.type);
            return e->cast.type;
        }

        case EXPR_SIZEOF: {
            Cuik_Type* src = cuik_canonical_type(GET_TYPE(0));
            if (src->size == 0) {
                diag_err(&tu->tokens, e->loc, "cannot get the sizeof an incomplete type");
            }

            return cuik_uncanonical_type(&tu->target->size_type);
        }

        case EXPR_SIZEOF_T: {
            Cuik_Type* src = cuik_canonical_type(e->x_of_type.type);
            if (src->size == 0) {
                diag_err(&tu->tokens, e->loc, "cannot get the sizeof an incomplete type");
            }

            return cuik_uncanonical_type(&tu->target->size_type);
        }

        case EXPR_ADDR: {
            uint64_t dst;
            Cuik_QualType src = GET_TYPE(0);
            Cuik_QualType ptr_to = cuik_uncanonical_type(cuik__new_pointer(&tu->types, src));
            return ptr_to;
        }
        case EXPR_BUILTIN_SYMBOL: {
            // placeholder type, the call will handle things
            return cuik_uncanonical_type(&cuik__builtin_void);
        }
        case EXPR_SYMBOL: {
            Stmt* restrict sym = e->sym.stmt;
            if (sym->flags & STMT_FLAGS_IS_RESOLVING) {
                diag_err(&tu->tokens, sym->loc, "cycle in symbol", sym->decl.name);
                return cuik_uncanonical_type(&cuik__builtin_void);
            }

            if (sym->op == STMT_LABEL) {
                if (!sym->label.placed) {
                    diag_err(&tu->tokens, sym->loc, "label '%s' is never defined.", sym->label.name);
                }

                return cuik_uncanonical_type(&cuik__builtin_void);
            } else {
                Cuik_Type* type = cuik_canonical_type(sym->decl.type);

                if (type->kind == KIND_ARRAY) {
                    if (type->size == 0 && (sym->op == STMT_GLOBAL_DECL || sym->op == STMT_DECL)) {
                        sym->flags |= STMT_FLAGS_IS_RESOLVING;

                        // try to resolve the type since it's incomplete
                        sema_stmt(tu, sym);

                        sym->flags &= ~STMT_FLAGS_IS_RESOLVING;
                        type = cuik_canonical_type(sym->decl.type);
                        assert(type->size != 0 && "Uhh... we fucked up");
                    }

                    // this is the only *current* example where something sets
                    // it's own cast_type it's an exception to the rules.
                    _->cast_types[e - _->exprs] = cuik_uncanonical_type(cuik__new_pointer(&tu->types, type->array.of));
                }

                return sym->decl.type;
            }
        }
        case EXPR_PARAM: {
            int param_num = e->param_num;

            Param* param_list = cuik_canonical_type(cuik__sema_function_stmt->decl.type)->func.param_list;
            return param_list[param_num].type;
        }

        case EXPR_WSTR: {
            const char* in = (const char*)(e->str.start + 1);
            size_t len = ((const char*)e->str.end - 1) - in;

            // it can't be bigger than the original
            wchar_t* out = tb_arena_alloc(tu->arena, (len + 1) * 2);

            size_t out_i = 0, in_i = 0;
            while (in_i < len) {
                int ch;
                ptrdiff_t distance = parse_char(len - in_i, &in[in_i], &ch);
                if (distance < 0) abort();

                assert(ch < 0x80);
                out[out_i++] = ch;
                in_i += distance;
            }

            assert(out_i <= len);
            out[out_i++] = '\0';

            e->str.start = (unsigned char*)&out[0];
            e->str.end = (unsigned char*)&out[out_i];

            Cuik_QualType wchar_type = cuik_uncanonical_type(&tu->target->signed_ints[CUIK_BUILTIN_SHORT]);
            return cuik_uncanonical_type(cuik__new_array(&tu->types, wchar_type, out_i));
        }
        case EXPR_STR: {
            const char* in = (const char*)(e->str.start + 1);
            size_t len = ((const char*)e->str.end - 1) - in;

            // it can't be bigger than the original
            char* out = tb_arena_alloc(tu->arena, len + 1);

            size_t out_i = 0, in_i = 0;
            while (in_i < len) {
                int ch;
                ptrdiff_t distance = parse_char(len - in_i, &in[in_i], &ch);
                if (distance < 0) abort();

                out[out_i++] = ch;
                in_i += distance;
            }

            assert(out_i <= len);
            out[out_i++] = '\0';

            e->str.start = (unsigned char*)out;
            e->str.end = (unsigned char*)(out + out_i);

            Cuik_QualType char_type = cuik_uncanonical_type(&tu->target->signed_ints[CUIK_BUILTIN_CHAR]);
            return cuik_uncanonical_type(cuik__new_array(&tu->types, char_type, out_i));
        }

        case EXPR_INITIALIZER: {
            Cuik_Type* t = cuik_canonical_type(e->init.type);
            int bounds = compute_initializer_bounds(t);

            if (t->kind == KIND_ARRAY) {
                if (!CUIK_TYPE_IS_COMPLETE(cuik_canonical_type(t->array.of))) {
                    type_layout2(NULL, &tu->tokens, cuik_canonical_type(t->array.of));
                }

                int old_array_count = t->array.count;
                int new_array_count = sema_infer_initializer_array_count(tu, e->init.root);

                // if it's 0, then it's unsized and anything goes
                if (old_array_count != 0) {
                    // verify that everything fits correctly
                    if (old_array_count < new_array_count) {
                        diag_err(&tu->tokens, e->loc, "Array cannot fit into declaration (needs %d, got %d)", old_array_count, new_array_count);
                    }
                } else {
                    t = cuik__new_array(&tu->types, t->array.of, new_array_count);
                    e->init.type = cuik_make_qual_type(t, cuik_get_quals(e->init.type));
                }
            }

            walk_initializer_for_sema(tu, t, e->init.root, 0);
            return e->init.type;
        }

        case EXPR_CALL: {
            Subexpr* target = &GET_EXPR(0);
            Cuik_Type* func_type = cuik_canonical_type(GET_TYPE(0));

            if (target->op == EXPR_BUILTIN_SYMBOL) {
                const char* name = (const char*) target->builtin_sym.name;
                ptrdiff_t search = nl_map_get_cstr(tu->target->builtin_func_map, name);
                assert(search >= 0 && "Builtin symbol somehow isn't builtin?");

                int arg_count = e->call.param_count;
                Cuik_Type* ty = sema_builtin(
                    tu, _, tu->target->builtin_func_map[search].v, arg_count + 1, args
                );

                return cuik_uncanonical_type(ty);
            } else if (target->op == EXPR_CONSTRUCTOR) {
                Cuik_Type* vector_base = func_type->kind == KIND_VECTOR ? func_type->vector.base  : func_type;
                size_t vector_width    = func_type->kind == KIND_VECTOR ? func_type->vector.count : 1;

                int arg_count = e->call.param_count;

                // you can construct the vector from an arbitrary set of smaller vectors
                size_t components_done = 0;
                for (size_t i = 0; i < arg_count; i++) {
                    Cuik_Type* arg_type = cuik_canonical_type(GET_TYPE(i + 1));

                    Cuik_Type* arg_vector_base = arg_type->kind == KIND_VECTOR ? arg_type->vector.base  : arg_type;
                    size_t arg_vector_width    = arg_type->kind == KIND_VECTOR ? arg_type->vector.count : 1;

                    implicit_conversion(tu, cuik_uncanonical_type(arg_vector_base), cuik_uncanonical_type(vector_base), &GET_EXPR(i + 1));

                    components_done += arg_vector_width;
                    if (components_done > vector_width) {
                        diag_err(&tu->tokens, GET_EXPR(i + 1).loc, "Too many arguments (expected %d components, got %d)", vector_width, components_done);
                    }

                    SET_CAST(i + 1, GET_TYPE(i + 1));
                }

                return cuik_uncanonical_type(func_type);
            }

            // implicit dereference
            if (func_type->kind == KIND_PTR) {
                func_type = cuik_canonical_type(func_type->ptr_to);
            }

            if (func_type->kind != KIND_FUNC) {
                diag_err(&tu->tokens, GET_EXPR(0).loc, "function call target must be a function-type, got %!T", func_type);
                return CUIK_QUAL_TYPE_NULL;
            }

            SET_CAST(0, cuik_uncanonical_type(func_type));

            Param* params = func_type->func.param_list;
            int param_count = func_type->func.param_count;
            bool has_varargs = func_type->func.has_varargs;

            // we need at least enough arguments for the parameters
            int arg_count = e->call.param_count;
            if (arg_count < param_count) {
                diag_err(&tu->tokens, e->loc, "too little arguments (expected%s%d, got %d)", has_varargs ? " at least " : " ", param_count, arg_count);
                return CUIK_QUAL_TYPE_NULL;
            } else if (!has_varargs) {
                if (arg_count > param_count) {
                    diag_err(&tu->tokens, e->loc, "too many arguments (expected %d, got %d)", param_count, arg_count);
                    return CUIK_QUAL_TYPE_NULL;
                }
            }

            // type-check the parameters with a known type
            for (size_t i = 0; i < param_count; i++) {
                Cuik_QualType arg_type = GET_TYPE(i + 1);

                implicit_conversion(tu, arg_type, params[i].type, &GET_EXPR(i + 1));
                SET_CAST(i + 1, params[i].type);
            }

            // type-check the untyped arguments
            for (size_t i = param_count; i < arg_count; i++) {
                Cuik_Type* src = cuik_canonical_type(GET_TYPE(i + 1));

                // all integers ranked lower than int are promoted to int
                if (src->kind >= KIND_BOOL && src->kind < KIND_INT) {
                    src = &tu->target->signed_ints[CUIK_BUILTIN_INT];
                }

                // all floats ranked lower than double are promoted to double
                if (src->kind == KIND_FLOAT) {
                    src = &cuik__builtin_double;
                }

                SET_CAST(i + 1, cuik_uncanonical_type(src));
            }

            return func_type->func.return_type;
        }

        case EXPR_DOT:
        case EXPR_ARROW: {
            Cuik_Type* base = cuik_canonical_type(GET_TYPE(0));

            if (e->op == EXPR_DOT && tu->version == CUIK_VERSION_GLSL) {
                if (cuik_type_can_swizzle(base)) {
                    Cuik_Type* vector_base = base->kind == KIND_VECTOR ? base->vector.base  : base;
                    size_t vector_width    = base->kind == KIND_VECTOR ? base->vector.count : 1;

                    const char* name = (const char*) e->dot_arrow.name;
                    size_t len = strlen(name);

                    // early out if there's too many elements
                    if (len > 4) {
                        diag_err(&tu->tokens, e->loc, "too many elements in swizzle");
                    }

                    Cuik_QualType type = cuik_uncanonical_type(cuik__new_vector2(&tu->types, vector_base, len));

                    // convert member access into swizzle
                    e->op = EXPR_SWIZZLE;
                    e->swizzle.len = len;

                    Cuik_GlslSwizzle swizzle = CUIK_GLSL_SWIZZLE_UNKNOWN;
                    for (size_t i = 0; i < len; i++) {
                        int val = -1;
                        Cuik_GlslSwizzle s = CUIK_GLSL_SWIZZLE_UNKNOWN;

                        switch (name[i]) {
                            case 'r': val = 0, s = CUIK_GLSL_SWIZZLE_RGBA; break;
                            case 'g': val = 1, s = CUIK_GLSL_SWIZZLE_RGBA; break;
                            case 'b': val = 2, s = CUIK_GLSL_SWIZZLE_RGBA; break;
                            case 'a': val = 3, s = CUIK_GLSL_SWIZZLE_RGBA; break;

                            case 'x': val = 0, s = CUIK_GLSL_SWIZZLE_XYZW; break;
                            case 'y': val = 1, s = CUIK_GLSL_SWIZZLE_XYZW; break;
                            case 'z': val = 2, s = CUIK_GLSL_SWIZZLE_XYZW; break;
                            case 'w': val = 3, s = CUIK_GLSL_SWIZZLE_XYZW; break;

                            case 's': val = 0, s = CUIK_GLSL_SWIZZLE_STUV; break;
                            case 't': val = 1, s = CUIK_GLSL_SWIZZLE_STUV; break;
                            case 'u': val = 2, s = CUIK_GLSL_SWIZZLE_STUV; break;
                            case 'v': val = 3, s = CUIK_GLSL_SWIZZLE_STUV; break;
                        }

                        e->swizzle.indices[i] = val;

                        // validating input
                        if (val >= vector_width) {
                            diag_err(&tu->tokens, e->loc, "swizzle element '%c' out of bounds (%d components)", name[i], vector_width);
                            continue;
                        }

                        if (swizzle != CUIK_GLSL_SWIZZLE_UNKNOWN && swizzle != s) {
                            diag_err(&tu->tokens, e->loc, "mismatched swizzle style with '%c' and '%c'", name[i], name[i - 1]);
                            continue;
                        }

                        if (s == CUIK_GLSL_SWIZZLE_UNKNOWN) {
                            diag_err(&tu->tokens, e->loc, "unknown swizzle element '%c' (possible options: rgba, xyzw, stuv)", name[i]);
                            continue;
                        }

                        swizzle = s;
                    }

                    return type;
                }
            }

            // Normal C member access
            bool is_arrow = (e->op == EXPR_ARROW);
            Cuik_Type* record_type = get_record_type(tu, base, e->loc, is_arrow);

            if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
                diag_err(&tu->tokens, e->loc, "Cannot get the member of a non-record type (%!T)", record_type);
                diag_note(&tu->tokens, record_type->loc, "see record here");
                return CUIK_QUAL_TYPE_NULL;
            }

            if (record_type->size == 0) {
                type_layout2(NULL, &tu->tokens, record_type);

                if (record_type->size == 0) {
                    diag_err(&tu->tokens, e->loc, "Cannot access members in incomplete type");
                    diag_note(&tu->tokens, record_type->loc, "see here");
                    return CUIK_QUAL_TYPE_NULL;
                }
            }

            uint32_t offset = 0;
            Member* m = sema_traverse_members(record_type, e->dot_arrow.name, &offset);
            if (m != NULL) {
                SET_CAST(0, GET_TYPE(0));

                // resolved
                e->op = (e->op == EXPR_DOT ? EXPR_DOT_R : EXPR_ARROW_R);
                e->dot_arrow.member = m;
                e->dot_arrow.offset = offset;
                return m->type;
            }

            diag_err(&tu->tokens, e->loc, "Could not find member called '%s' for type '%!T'", e->dot_arrow.name, record_type);
            return CUIK_QUAL_TYPE_NULL;
        }

        case EXPR_DEREF: {
            Cuik_QualType base = GET_TYPE(0);
            SET_CAST(0, base);

            Cuik_Type* base_canon = cuik_canonical_type(base);
            if (base_canon->kind == KIND_PTR) {
                return base_canon->ptr_to;
            } else if (base_canon->kind == KIND_ARRAY) {
                return base_canon->array.of;
            } else {
                diag_err(&tu->tokens, e->loc, "Cannot dereference from non-pointer and non-array type %!T", base);
                return cuik_uncanonical_type(&cuik__builtin_void);
            }
        }

        case EXPR_TERNARY: {
            Cuik_Type* cond_type = cuik_canonical_type(GET_TYPE(0));
            if (!is_scalar_type(tu, cond_type)) {
                diag_err(&tu->tokens, e->loc, "Could not convert type %!T into boolean", cond_type);
            }
            SET_CAST(0, cuik_uncanonical_type(&cuik__builtin_bool));

            Cuik_QualType ty1 = cuik__sema_expr(tu, e->ternary.left);
            Cuik_QualType ty2 = cuik__sema_expr(tu, e->ternary.right);

            // if either side is a zero then it's malleable
            if (!is_constant_zero(get_root_subexpr(e->ternary.left)) &&
                !is_constant_zero(get_root_subexpr(e->ternary.right))) {
                implicit_conversion(tu, ty1, ty2, get_root_subexpr(e->ternary.left));
            }

            Cuik_QualType type = CUIK_QUAL_TYPE_NULL;
            if (cuik_type_is_aggregate(cuik_canonical_type(ty1))) {
                // should probably equality check ty1 and ty2 here
                type = ty1;
            } else {
                type = cuik_uncanonical_type(get_common_type(&tu->types, cuik_canonical_type(ty1), cuik_canonical_type(ty2)));
            }

            set_root_cast(e->ternary.left, type);
            set_root_cast(e->ternary.right, type);
            return type;
        }

        case EXPR_COMMA: {
            SET_CAST(0, GET_TYPE(0));
            SET_CAST(1, GET_TYPE(1));

            return GET_TYPE(1);
        }

        case EXPR_VA_ARG: {
            Cuik_Type* va_list_type = cuik_canonical_type(GET_TYPE(0));
            SET_CAST(0, GET_TYPE(0));

            if (!type_equal(va_list_type, tu->va_list)) {
                diag_err(&tu->tokens, e->loc, "va_arg must take in a va_list in the first argument (got %!T)", va_list_type);
            }

            Cuik_QualType type = e->va_arg_.type;
            int size = cuik_canonical_type(type)->size;
            Cuik_Type* target_signed_ints = tu->target->signed_ints;
            if (size < target_signed_ints[CUIK_BUILTIN_INT].size) {
                diag_warn(&tu->tokens, e->loc, "va_arg used on a value smaller than int");
            }

            return type;
        }
        case EXPR_LOGICAL_NOT: {
            Cuik_QualType boolean = cuik_uncanonical_type(&cuik__builtin_bool);

            SET_CAST(0, boolean);
            return boolean;
        }
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR: {
            Cuik_QualType boolean = cuik_uncanonical_type(&cuik__builtin_bool);

            cuik__sema_expr(tu, e->logical_binop.left);
            cuik__sema_expr(tu, e->logical_binop.right);

            set_root_cast(e->logical_binop.left,  boolean);
            set_root_cast(e->logical_binop.right, boolean);

            return boolean;
        }

        case EXPR_SUBSCRIPT: {
            Cuik_Type* base  = cuik_canonical_type(GET_TYPE(0));
            Cuik_Type* index = cuik_canonical_type(GET_TYPE(1));

            if (index->kind == KIND_PTR || index->kind == KIND_ARRAY) {
                SWAP(Cuik_Type*, base, index);
                SWAP(size_t, args[0], args[1]);
            }

            if (base->kind == KIND_ARRAY) {
                base = cuik__new_pointer(&tu->types, base->array.of);
            }

            if (base->kind != KIND_PTR) {
                diag_err(&tu->tokens, e->loc, "cannot perform subscript [] with base type %!T", base);
                return cuik_uncanonical_type(&cuik__builtin_void);
            }

            SET_CAST(0, GET_TYPE(0));
            SET_CAST(1, cuik_uncanonical_type(&tu->target->ptrdiff_type));
            return base->ptr_to;
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
            Cuik_Type* lhs = cuik_canonical_type(GET_TYPE(0));
            Cuik_Type* rhs = cuik_canonical_type(GET_TYPE(1));

            if ((e->op == EXPR_PLUS || e->op == EXPR_MINUS) && (cuik_type_can_deref(lhs) || cuik_type_can_deref(rhs))) {
                Cuik_QualType ptrdiff_ty = cuik_uncanonical_type(&tu->target->ptrdiff_type);

                bool lhs_is_ptr = lhs->kind == KIND_PTR || lhs->kind == KIND_ARRAY;
                bool rhs_is_ptr = rhs->kind == KIND_PTR || rhs->kind == KIND_ARRAY;

                if (lhs_is_ptr && rhs_is_ptr) {
                    if (e->op == EXPR_MINUS) {
                        // ptr - ptr = ptrdiff_t
                        SET_CAST(0, GET_TYPE(0));
                        SET_CAST(1, GET_TYPE(1));

                        e->op = EXPR_PTRDIFF;
                        return ptrdiff_ty;
                    } else {
                        diag_err(&tu->tokens, e->loc, "Cannot do pointer addition with two pointer operands, one must be an integral type.");
                        return CUIK_QUAL_TYPE_NULL;
                    }
                } else {
                    // Pointer arithmatic
                    if (e->op == EXPR_PLUS && rhs_is_ptr) {
                        SWAP(Cuik_Type*, lhs, rhs);
                        SWAP(size_t, args[0], args[1]);
                        e->ptrop.flipped = true;
                    }

                    SET_CAST(0, GET_TYPE(0));
                    SET_CAST(1, ptrdiff_ty);

                    if (cuik_canonical_type(lhs->ptr_to)->size == 0) {
                        diag_err(&tu->tokens, e->loc, "Cannot do pointer arithmatic on incomplete type");
                    }

                    e->op = (e->op == EXPR_PLUS) ? EXPR_PTRADD : EXPR_PTRSUB;
                    return cuik_uncanonical_type(lhs);
                }
            } else {
                // binary operators on vectors
                if (lhs->kind == KIND_VECTOR || rhs->kind == KIND_VECTOR) {
                    if (rhs->kind == KIND_VECTOR) {
                        SWAP(Cuik_Type*, lhs, rhs);
                    }

                    // we can do binop(vecN, scalar) or binop(vecN, vecN)
                    if (rhs->kind == KIND_VECTOR && lhs->vector.count != rhs->vector.count) {
                        diag_err(&tu->tokens, e->loc, "cannot apply binary operator to %!T and %!T", lhs, rhs);
                    }

                    Cuik_QualType type = cuik_uncanonical_type(lhs);
                    SET_CAST(0, type);
                    SET_CAST(1, type);
                    return type;
                }

                if (!(lhs->kind >= KIND_BOOL && lhs->kind <= KIND_DOUBLE && rhs->kind >= KIND_BOOL && rhs->kind <= KIND_DOUBLE)) {
                    diag_err(&tu->tokens, e->loc, "cannot apply binary operator to %!T and %!T", lhs, rhs);
                    return CUIK_QUAL_TYPE_NULL;
                }

                Cuik_QualType type = cuik_uncanonical_type(get_common_type(&tu->types, lhs, rhs));

                // Do we actually need to check both sides?
                implicit_conversion(tu, cuik_uncanonical_type(lhs), type, &GET_EXPR(0));
                implicit_conversion(tu, cuik_uncanonical_type(rhs), type, &GET_EXPR(1));

                SET_CAST(0, type);
                SET_CAST(1, type);
                return type;
            }
        }

        case EXPR_NOT:
        case EXPR_NEGATE:
        case EXPR_PRE_INC:
        case EXPR_PRE_DEC:
        case EXPR_POST_INC:
        case EXPR_POST_DEC: {
            Cuik_QualType src = GET_TYPE(0);

            SET_CAST(0, src);
            return src;
        }

        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE: {
            Cuik_QualType type = cuik_uncanonical_type(get_common_type(
                    &tu->types,
                    cuik_canonical_type(GET_TYPE(0)),
                    cuik_canonical_type(GET_TYPE(1))
                ));

            SET_CAST(0, type);
            SET_CAST(1, type);

            return cuik_uncanonical_type(&cuik__builtin_bool);
        }

        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_PERCENT_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN: {
            if (!is_assignable_expr(&GET_EXPR(0))) {
                diag_err(&tu->tokens, GET_EXPR(0).loc, "left-hand side is not assignable");
                return CUIK_QUAL_TYPE_NULL;
            }

            Cuik_QualType lhs = GET_TYPE(0), rhs = GET_TYPE(1);
            if (CUIK_QUAL_TYPE_HAS(lhs, CUIK_QUAL_CONST)) {
                diag_err(&tu->tokens, GET_EXPR(0).loc, "cannot assign to const value");
                return CUIK_QUAL_TYPE_NULL;
            }

            SET_CAST(0, lhs);

            if (e->op == EXPR_PLUS_ASSIGN && cuik_type_is_pointer(cuik_canonical_type(lhs))) {
                // pointer arithmatic
                SET_CAST(1, cuik_uncanonical_type(&tu->target->ptrdiff_type));
                return lhs;
            } else {
                SET_CAST(1, lhs);
                return lhs;
            }
        }

        default:
        diag_err(&tu->tokens, e->loc, "cannot type check operation %s (TODO?)", cuik_get_expr_name(e));
        return CUIK_QUAL_TYPE_NULL;
    }
}
#undef GET_EXPR
#undef GET_TYPE
#undef SET_CAST

Cuik_QualType cuik__sema_expr(TranslationUnit* tu, Cuik_Expr* restrict e) {
    if (e->visited) {
        return e->types[e->count - 1];
    }

    // we're gonna need a type and cast_type stream
    Cuik_QualType* t = TB_ARENA_ARR_ALLOC(tu->arena, 2 * e->count, Cuik_QualType);
    e->visited    = true;
    e->types      = t;
    e->cast_types = &t[e->count];

    size_t stack[128], top = 0;
    Subexpr* exprs = e->exprs;
    for (size_t i = 0; i < e->count; i++) {
        // once we know this we can organize the top slice of the stack as the inputs
        int arity = cuik_get_expr_arity(&exprs[i]);
        top -= arity;
        size_t* args = &stack[top];

        // NOTE(NeGate): we're using the t pointer as opposed to the "correct"
        // e->types because i don't trust compilers to handle aliasing of e
        // and whatever happens in cuik__sema_subexpr.
        t[i] = cuik__sema_subexpr(tu, e, &exprs[i], arity, args);
        if (CUIK_QUAL_TYPE_IS_NULL(t[i])) {
            // there was a nasty error... exit
            return CUIK_QUAL_TYPE_NULL;
        }

        assert(top < 128 && "Too complex of an expression");
        stack[top++] = i;
    }

    return t[e->count - 1];
}

void sema_stmt(TranslationUnit* tu, Stmt* restrict s) {
    if (s == NULL) return;

    switch (s->op) {
        case STMT_NONE: break;
        case STMT_LABEL: break;

        case STMT_GOTO: {
            set_root_cast(s->goto_.target, cuik__sema_expr(tu, s->goto_.target));
            break;
        }
        case STMT_COMPOUND: {
            Stmt** kids = s->compound.kids;
            size_t count = s->compound.kids_count;

            Stmt* killer = NULL;
            for (size_t i = 0; i < count; i++) {
                Stmt* kid = kids[i];
                sema_stmt(tu, kid);

                if (killer) {
                    if (kid->op == STMT_LABEL ||
                        kid->op == STMT_CASE ||
                        kid->op == STMT_DEFAULT) {
                        killer = 0;
                    } else {
                        // diag_warn(&tu->tokens, kid->loc, "Dead code");
                        // diag_note(&tu->tokens, killer->loc, "After");
                    }
                } else {
                    if (kid->op == STMT_RETURN ||
                        kid->op == STMT_GOTO ||
                        kid->op == STMT_BREAK ||
                        kid->op == STMT_DISCARD ||
                        kid->op == STMT_CONTINUE) {
                        killer = kid;
                    }
                }
            }

            break;
        }
        // global decl is only resolved here in the rare occasion where
        // const_eval is needing to resolve a type early
        case STMT_GLOBAL_DECL:
        case STMT_DECL: {
            Cuik_Type* decl_type = cuik_canonical_type(s->decl.type);
            if (s->decl.initial) {
                Cuik_Qualifiers decl_quals = cuik_get_quals(s->decl.type);

                Cuik_Expr* e = s->decl.initial;
                Subexpr* root = get_root_subexpr(e);
                if (root->op == EXPR_INITIALIZER && CUIK_QUAL_TYPE_IS_NULL(root->init.type)) {
                    // give it something to go off of
                    root->init.type = s->decl.type;
                }

                Cuik_Type* expr_type = cuik_canonical_type(cuik__sema_expr(tu, e));
                if (expr_type == NULL) {
                    break;
                }

                if (root->op == EXPR_INITIALIZER) {
                    // Auto-detect array count from initializer
                    if (decl_type->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
                        if (decl_type->array.count != 0 && decl_type->array.count < expr_type->array.count) {
                            diag_err(&tu->tokens, s->loc, "array initializer does not fit into declaration (expected %d, got %d)", decl_type->array.count, expr_type->array.count);
                        } else {
                            s->decl.type = cuik_make_qual_type(expr_type, decl_quals);
                            decl_type = cuik_canonical_type(s->decl.type);
                        }
                    }
                } else if (root->op == EXPR_STR || root->op == EXPR_WSTR) {
                    // Auto-detect array count from string
                    if (decl_type->kind == KIND_ARRAY && decl_type->array.count == 0) {
                        s->decl.type = cuik_make_qual_type(expr_type, decl_quals);
                        decl_type = cuik_canonical_type(s->decl.type);
                    }
                }

                set_root_cast(e, cuik_uncanonical_type(decl_type));
                if (!type_compatible(tu, expr_type, decl_type, root)) {
                    diag_err(&tu->tokens, s->loc, "could not implicitly convert type %!T into %!T.", expr_type, decl_type);
                }
            }

            if (decl_type->size == 0 || !CUIK_TYPE_IS_COMPLETE(decl_type)) {
                diag_err(&tu->tokens, s->loc, "incomplete type used in declaration");
                diag_note(&tu->tokens, decl_type->loc, "type declared here");
            }
            break;
        }
        case STMT_EXPR: {
            set_root_cast(s->expr.expr, cuik__sema_expr(tu, s->expr.expr));
            break;
        }
        case STMT_RETURN: {
            if (s->return_.expr) {
                Cuik_QualType expr_type = cuik__sema_expr(tu, s->return_.expr);
                if (CUIK_QUAL_TYPE_IS_NULL(expr_type)) {
                    break;
                }

                Cuik_QualType return_type = cuik_canonical_type(cuik__sema_function_stmt->decl.type)->func.return_type;

                implicit_conversion(tu, expr_type, return_type, get_root_subexpr(s->return_.expr));
                set_root_cast(s->return_.expr, return_type);
            }
            break;
        }
        case STMT_IF: {
            Subexpr* cond = get_root_subexpr(s->if_.cond);
            if (cond->op >= EXPR_ASSIGN && cond->op <= EXPR_SHR_ASSIGN && !cond->has_parens) {
                diag_warn(&tu->tokens, cond->loc, "using assignment as condition without parenthesis");
            }

            Cuik_Type* cond_type = cuik_canonical_type(cuik__sema_expr(tu, s->if_.cond));
            if (cond_type == NULL) {
                break;
            }

            if (!is_scalar_type(tu, cond_type)) {
                diag_err(&tu->tokens, cond->loc, "Could not convert type %!T into boolean.", cond_type);
            }
            set_root_cast(s->if_.cond, cuik_uncanonical_type(&cuik__builtin_bool));

            sema_stmt(tu, s->if_.body);
            if (s->if_.next) {
                sema_stmt(tu, s->if_.next);
            }
            break;
        }
        case STMT_WHILE: {
            Subexpr* cond = get_root_subexpr(s->while_.cond);
            if (cond->op >= EXPR_ASSIGN && cond->op <= EXPR_SHR_ASSIGN && !cond->has_parens) {
                diag_warn(&tu->tokens, cond->loc, "using assignment as condition without parenthesis");
            }

            cuik__sema_expr(tu, s->while_.cond);
            set_root_cast(s->while_.cond, cuik_uncanonical_type(&cuik__builtin_bool));

            if (s->while_.body) {
                sema_stmt(tu, s->while_.body);
            }
            break;
        }
        case STMT_DO_WHILE: {
            if (s->do_while.body) {
                sema_stmt(tu, s->do_while.body);
            }

            cuik__sema_expr(tu, s->do_while.cond);
            set_root_cast(s->do_while.cond, cuik_uncanonical_type(&cuik__builtin_bool));
            break;
        }
        case STMT_FOR: {
            if (s->for_.first) {
                sema_stmt(tu, s->for_.first);
            }

            if (s->for_.cond) {
                Subexpr* cond = get_root_subexpr(s->for_.cond);
                if (cond->op >= EXPR_ASSIGN && cond->op <= EXPR_SHR_ASSIGN && !cond->has_parens) {
                    diag_warn(&tu->tokens, cond->loc, "using assignment as condition without parenthesis");
                }

                cuik__sema_expr(tu, s->for_.cond);
                set_root_cast(s->for_.cond, cuik_uncanonical_type(&cuik__builtin_bool));
            }

            if (s->for_.body) {
                sema_stmt(tu, s->for_.body);
            }

            if (s->for_.next) {
                set_root_cast(s->for_.next, cuik__sema_expr(tu, s->for_.next));
            }
            break;
        }
        case STMT_SWITCH: {
            Cuik_QualType type = cuik__sema_expr(tu, s->switch_.condition);
            set_root_cast(s->switch_.condition, type);

            if (CUIK_QUAL_TYPE_IS_NULL(type)) {
                break;
            }

            if (!cuik_type_is_integer_or_bool(cuik_canonical_type(type))) {
                diag_err(&tu->tokens, s->loc, "switch case type must be an integral type, got a %!T", type);
            }

            sema_stmt(tu, s->switch_.body);
            break;
        }
        case STMT_CASE: {
            while (s->case_.body && s->case_.body->op == STMT_CASE) {
                s = s->case_.body;
            }

            sema_stmt(tu, s->case_.body);
            break;
        }
        case STMT_DEFAULT: {
            sema_stmt(tu, s->default_.body);
            break;
        }
        case STMT_DISCARD:
        case STMT_CONTINUE:
        case STMT_BREAK: {
            break;
        }

        default:
        TODO();
    }
}

static void sema_top_level(TranslationUnit* tu, Stmt* restrict s) {
    Cuik_Type* type = cuik_canonical_type(s->decl.type);
    Cuik_Qualifiers quals = cuik_get_quals(s->decl.type);

    char* name = (char*)s->decl.name;
    switch (s->op) {
        case STMT_FUNC_DECL: {
            assert(type->kind == KIND_FUNC);
            if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
                diag_err(&tu->tokens, s->loc, "Function '%s' cannot be both static and extern.", name);
                break;
            }

            if (s->decl.attrs.is_static && !s->decl.attrs.is_inline) {
                if (tu->warnings->unused_funcs && !s->decl.attrs.is_used) {
                    diag_warn(&tu->tokens, s->loc, "Function '%s' is never used.", name);
                }
            }

            if (s->decl.attrs.is_static || s->decl.attrs.is_inline) {
                if (!s->decl.attrs.is_used) break;
            }

            // type check function body
            cuik__sema_function_stmt = s;
            sema_stmt(tu, s->decl.initial_as_stmt);
            cuik__sema_function_stmt = 0;
            break;
        }
        case STMT_DECL:
        case STMT_GLOBAL_DECL: {
            if (name == NULL) break;
            if (!s->decl.attrs.is_used) break;
            if (s->decl.attrs.is_typedef) break;

            if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
                diag_err(&tu->tokens, s->loc, "Global declaration '%s' cannot be both static and extern.", name);
                break;
            }

            bool is_external_sym = (type->kind == KIND_FUNC && s->decl.initial_as_stmt == NULL);
            if (s->decl.attrs.is_extern) is_external_sym = true;

            if (!is_external_sym) {
                if (s->decl.initial) {
                    Subexpr* e = get_root_subexpr(s->decl.initial);
                    if (e->op == EXPR_INITIALIZER && CUIK_QUAL_TYPE_IS_NULL(e->init.type)) {
                        // give it something to go off of
                        //
                        // doesn't have to be complete in terms of array count
                        // just enough to infer the rest in a sec
                        e->init.type = s->decl.type;
                    }

                    Cuik_Type* expr_type = cuik_canonical_type(cuik__sema_expr(tu, s->decl.initial));

                    if (e->op == EXPR_INITIALIZER || e->op == EXPR_STR || e->op == EXPR_WSTR) {
                        if (type->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
                            if (type_equal(cuik_canonical_type(type->array.of), cuik_canonical_type(expr_type->array.of))) {
                                if (type->array.count != 0 && type->array.count < expr_type->array.count) {
                                    diag_err(&tu->tokens, s->loc, "array initializer does not fit into declaration (expected %d, got %d)", type->array.count, expr_type->array.count);
                                } else {
                                    assert(expr_type->array.count);

                                    // preserve qualifiers
                                    s->decl.type = cuik_make_qual_type(expr_type, quals);
                                    type = cuik_canonical_type(s->decl.type);
                                }
                            } else {
                                diag_err(&tu->tokens, s->loc, "array initializer type mismatch (got '%s', expected '%s')", cuik_canonical_type(expr_type->array.of), cuik_canonical_type(type->array.of));
                            }
                        }
                    }

                    if (!type_compatible(tu, expr_type, type, e)) {
                        diag_err(&tu->tokens, s->loc, "declaration type does not match (got '%s', expected '%s')", type, expr_type);
                    }

                    set_root_cast(s->decl.initial, s->decl.type);
                }

                if (type->size == 0 || !CUIK_TYPE_IS_COMPLETE(type)) {
                    diag_err(&tu->tokens, s->loc, "incomplete type used in declaration");
                    diag_note(&tu->tokens, type->loc, "type declared here");
                }
            }
            break;
        }
        default:
        assert(0);
    }
}

static void sema_mark_decl(TranslationUnit* tu, Stmt* restrict s) {
    assert(s->op == STMT_FUNC_DECL || s->op == STMT_DECL || s->op == STMT_GLOBAL_DECL);

    // log_debug("mark %s", s->decl.name);
    s->decl.attrs.is_used = true;

    if (s->op == STMT_FUNC_DECL) {
        s->flags |= STMT_FLAGS_HAS_IR_BACKING;
    }

    Cuik_Expr* restrict e = s->decl.first_symbol;
    while (e != NULL) {
        // mark subexpressions
        for (ptrdiff_t i = e->first_symbol; i >= 0; i = e->exprs[i].sym.next_symbol) {
            Stmt* kid = e->exprs[i].sym.stmt;
            assert(e->exprs[i].op == EXPR_SYMBOL);

            if (!kid->decl.attrs.is_used) {
                sema_mark_decl(tu, kid);
            }
        }

        e = e->next_in_chain;
    }
}

int cuiksema_run(TranslationUnit* restrict tu, Cuik_IThreadpool* restrict thread_pool) {
    size_t count = dyn_array_length(tu->top_level_stmts);

    // simple mark and sweep to remove unused symbols
    CUIK_TIMED_BLOCK("sema: collection") {
        for (size_t i = 0; i < count; i++) {
            Stmt* restrict s = tu->top_level_stmts[i];
            assert(s->op == STMT_FUNC_DECL || s->op == STMT_DECL || s->op == STMT_GLOBAL_DECL);

            const char* name = s->decl.name;
            if (s->op == STMT_FUNC_DECL) {
                if (!s->decl.attrs.is_static && !s->decl.attrs.is_inline) {
                    s->flags |= STMT_FLAGS_IS_EXPORTED;
                }
            } else if (s->op == STMT_GLOBAL_DECL || s->op == STMT_DECL) {
                if (!s->decl.attrs.is_extern && !s->decl.attrs.is_typedef && name != NULL && cuik_canonical_type(s->decl.type)->kind != KIND_FUNC) {
                    s->flags |= STMT_FLAGS_HAS_IR_BACKING;
                    s->flags |= STMT_FLAGS_IS_EXPORTED;
                }
            }

            if (s->decl.attrs.is_root) {
                sema_mark_decl(tu, s);
            }
        }
    }

    // go through all top level statements and type check
    // NOTE(NeGate): we can multithread this... it's not helpful tho
    CUIK_TIMED_BLOCK("sema: type check") {
        for (size_t i = 0; i < count; i++) {
            sema_top_level(tu, tu->top_level_stmts[i]);
        }
    }

    return cuikdg_error_count(&tu->tokens);
}
