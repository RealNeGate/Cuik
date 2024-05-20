// Using the new TB builder because it's better :p
typedef struct {
    IRValType kind : 16;
    int mem_var    : 16;
    struct {
        uint16_t offset;
        uint16_t width;
    } bits;
    TB_Node* n;
    Cuik_QualType type;
    Cuik_QualType cast_type;
} Val;

static void assign_to_lval(TB_GraphBuilder* g, Cuik_Type* type, const Val* dst, TB_Node* src, bool is_volatile) {
    if (dst->kind == LVALUE_BITS && dst->bits.width != (type->size * 8)) {
        // NOTE(NeGate): the semantics around volatile bitfields are janky at best
        TB_DataType dt = ctype_to_tbtype(type);
        TB_Node* old_value = tb_builder_load(g, 0, true, dt, dst->n, type->align);

        // mask out the space for our bitfield member
        uint64_t clear_mask = ~((UINT64_MAX >> (64ull - dst->bits.width)) << dst->bits.offset);
        old_value = tb_builder_binop_int(g, TB_AND, old_value, tb_builder_uint(g, dt, ~clear_mask), 0);

        // mask source value and position it correctly
        uint64_t insert_mask = (UINT64_MAX >> (64ull - dst->bits.width));
        src = tb_builder_binop_int(g, TB_AND, src, tb_builder_uint(g, dt, insert_mask), 0);

        if (dst->bits.offset) {
            // nuw & nsw are used since we statically know that the bits.offset won't overflow without some sort of UB
            src = tb_builder_binop_int(g, TB_SHL, src, tb_builder_uint(g, dt, dst->bits.offset), TB_ARITHMATIC_NSW | TB_ARITHMATIC_NUW);
        }

        // merge
        src = tb_builder_binop_int(g, TB_OR, old_value, src, 0);
    } else {
        assert(dst->kind == LVALUE);
    }

    tb_builder_store(g, 0, dst->n, src, type->align);
}

static TB_Node* as_rval(TranslationUnit* tu, TB_GraphBuilder* g, const Val* v) {
    Cuik_Type* dst = cuik_canonical_type(v->cast_type);
    Cuik_Type* src = cuik_canonical_type(v->type);
    bool is_volatile = CUIK_QUAL_TYPE_HAS(v->type, CUIK_QUAL_VOLATILE);

    TB_Node* n = v->n;
    switch (v->kind) {
        case RVALUE: break;
        case LVALUE: {
            // Implicit array to pointer
            if (src->kind == KIND_ARRAY || src->kind == KIND_FUNC) {
                // just pass the address don't load
                src = dst;
            } else {
                TB_DataType dt = ctype_to_tbtype(src);
                n = tb_builder_load(g, 0, true, dt, n, src->align);
            }
            break;
        }
        case LVALUE_BITS: {
            uint64_t mask = (UINT64_MAX >> (64ull - v->bits.width));
            TB_DataType dt = ctype_to_tbtype(src);

            n = tb_builder_load(g, 0, true, dt, n, src->align);

            if (v->bits.offset) {
                TB_Node* con = tb_builder_uint(g, dt, v->bits.offset);
                n = tb_builder_binop_int(g, TB_SHR, n, con, 0);
            }

            if (v->bits.width != (src->size * 8)) {
                TB_Node* con = tb_builder_uint(g, dt, mask);
                n = tb_builder_binop_int(g, TB_AND, n, con, 0);
            }
            break;
        }
        default: TODO();
    }

    if (dst->kind == KIND_VOID) {
        return NULL;
    } else if (src != dst) {
        // Cast into correct type
        if (src->kind == KIND_ARRAY && dst->kind == KIND_BOOL) {
            // arrays are never null
            n = tb_builder_bool(g, true);
        } else if (src->kind != KIND_BOOL && dst->kind == KIND_BOOL) {
            TB_DataType dt = n->dt;
            TB_Node* comparand;
            if (dt.type == TB_TAG_F32) {
                comparand = tb_builder_float32(g, 0.0f);
            } else if (dt.type == TB_TAG_F64) {
                comparand = tb_builder_float64(g, 0.0);
            } else {
                comparand = tb_builder_uint(g, dt, 0);
            }

            n = tb_builder_cmp(g, TB_CMP_NE, false, n, comparand);
        } else if (src->kind == KIND_BOOL && cuik_type_is_integer(dst)) {
            n = tb_builder_cast(g, ctype_to_tbtype(dst), TB_ZERO_EXT, n);
        } else if (cuik_type_is_integer(src) && cuik_type_is_integer(dst)) {
            if (dst->size > src->size) {
                // up-casts
                if (src->is_unsigned) {
                    n = tb_builder_cast(g, ctype_to_tbtype(dst), TB_ZERO_EXT, n);
                } else {
                    n = tb_builder_cast(g, ctype_to_tbtype(dst), TB_SIGN_EXT, n);
                }
            } else if (dst->size < src->size) {
                // down-casts
                n = tb_builder_cast(g, ctype_to_tbtype(dst), TB_TRUNCATE, n);
            }
        } else if (cuik_type_is_integer(src) && (dst->kind == KIND_PTR || dst->kind == KIND_FUNC)) {
            // integer -> ptr
            n = tb_builder_cast(g, TB_TYPE_PTR, TB_BITCAST, n);
        } else if ((src->kind == KIND_PTR || src->kind == KIND_FUNC) && cuik_type_is_integer(dst)) {
            // ptr -> integer
            n = tb_builder_cast(g, ctype_to_tbtype(dst), TB_BITCAST, n);
        } else if (src->kind == KIND_PTR && dst->kind == KIND_PTR) {
            /* TB has opaque pointers, nothing needs to be done. */
        } else if (src->kind == KIND_FLOAT && dst->kind == KIND_DOUBLE) {
            TB_DataType dt = n->dt;

            if (dt.type != TB_TAG_F64) {
                n = tb_builder_cast(g, TB_TYPE_F64, TB_FLOAT_EXT, n);
            }
        } else if (src->kind == KIND_DOUBLE && dst->kind == KIND_FLOAT) {
            TB_DataType dt = n->dt;
            if (dt.type != TB_TAG_F32) {
                n = tb_builder_cast(g, TB_TYPE_F32, TB_FLOAT_TRUNC, n);
            }
        } else if (cuik_type_is_float(src) && cuik_type_is_integer(dst)) {
            n = tb_builder_cast(g, ctype_to_tbtype(dst), dst->is_unsigned ? TB_FLOAT2UINT : TB_FLOAT2INT, n);
        } else if (cuik_type_is_integer(src) && cuik_type_is_float(dst)) {
            n = tb_builder_cast(g, ctype_to_tbtype(dst), dst->is_unsigned ? TB_UINT2FLOAT : TB_INT2FLOAT, n);
        }

        assert(n);
    }

    return n;
}

static Val cg_subexpr(TranslationUnit* tu, TB_GraphBuilder* g, Subexpr* e, Cuik_QualType qt, int arg_count, Val* args) {
    switch (e->op) {
        case EXPR_INT: {
            Cuik_Type* t = cuik_canonical_type(qt);
            TB_DataType dt = ctype_to_tbtype(t);

            if (t->kind == KIND_FLOAT) {
                return (Val){ RVALUE, .n = tb_builder_float32(g, e->int_lit.lit) };
            } else if (t->kind == KIND_DOUBLE) {
                return (Val){ RVALUE, .n = tb_builder_float64(g, e->int_lit.lit) };
            } else {
                // TODO(NeGate): maybe this should use tb_inst_sint?
                return (Val){ RVALUE, .n = tb_builder_uint(g, dt, e->int_lit.lit) };
            }
        }

        case EXPR_SYMBOL: {
            Stmt* stmt = e->sym.stmt;
            assert(stmt->op == STMT_DECL || stmt->op == STMT_LABEL || stmt->op == STMT_GLOBAL_DECL || stmt->op == STMT_FUNC_DECL);

            Cuik_Type* type = cuik_canonical_type(stmt->decl.type);
            if (stmt->op == STMT_LABEL) {
                /* if (stmt->backing.n == NULL) {
                    stmt->backing.n = tb_inst_region(func);
                }

                return (Val){ LVALUE_LABEL, .n = stmt->backing.n }; */
                TODO();
                return (Val){ 0 };
            } else if (stmt->op == STMT_FUNC_DECL || type->kind == KIND_FUNC || stmt->op == STMT_GLOBAL_DECL || (stmt->op == STMT_DECL && stmt->decl.attrs.is_static)) {
                if (stmt->backing.s == NULL) {
                    // check if it's defined by another TU
                    // functions are external by default
                    const char* name = (const char*) stmt->decl.name;
                    if (tu->parent != NULL) {
                        stmt->backing.s = get_external(tu->parent, name);
                    } else {
                        stmt->backing.e = tb_extern_create(tu->ir_mod, -1, name, TB_EXTERNAL_SO_LOCAL);
                    }
                }

                assert(stmt->backing.s != NULL);
                return (Val){ LVALUE, .n = tb_builder_symbol(g, stmt->backing.s) };
            } else {
                return (Val){ LVALUE, .n = stmt->backing.n };
            }
        }

        case EXPR_PARAM: {
            int param_num = e->param_num;
            Cuik_Type* arg_type = cuik_canonical_type(function_type->func.param_list[param_num].type);
            assert(arg_type != NULL);
            return (Val){ LVALUE, .mem_var = 1, .n = tb_builder_get_var(g, 2 + param_num) };
        }

        case EXPR_SUBSCRIPT: {
            TB_Node* base  = as_rval(tu, g, &args[0]);
            TB_Node* index = as_rval(tu, g, &args[1]);
            int64_t stride = cuik_canonical_type(qt)->size;
            return (Val){ LVALUE, .n = tb_builder_ptr_array(g, base, index, stride ? stride : 1) };
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
            Cuik_Type* type = cuik_canonical_type(args[0].cast_type);
            TB_Node* n = NULL;

            TB_Node* lhs = as_rval(tu, g, &args[0]);
            TB_Node* rhs = as_rval(tu, g, &args[1]);

            if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
                TB_NodeTypeEnum n_type = TB_NULL;
                switch (e->op) {
                    case EXPR_PLUS:  n_type = TB_FADD; break;
                    case EXPR_MINUS: n_type = TB_FSUB; break;
                    case EXPR_TIMES: n_type = TB_FMUL; break;
                    case EXPR_SLASH: n_type = TB_FDIV; break;
                    default: TODO();
                }
                n = tb_builder_binop_float(g, n_type, lhs, rhs);
            } else {
                TB_ArithmeticBehavior ab = 0;
                TB_NodeTypeEnum n_type = TB_NULL;
                switch (e->op) {
                    case EXPR_PLUS:    n_type = TB_ADD, ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW; break;
                    case EXPR_MINUS:   n_type = TB_SUB, ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW; break;
                    case EXPR_TIMES:   n_type = TB_MUL, ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW; break;
                    case EXPR_SLASH:   n_type = type->is_unsigned ? TB_UDIV : TB_SDIV; break;
                    case EXPR_PERCENT: n_type = type->is_unsigned ? TB_UMOD : TB_SMOD; break;
                    case EXPR_AND:     n_type = TB_AND; break;
                    case EXPR_OR:      n_type = TB_OR;  break;
                    case EXPR_XOR:     n_type = TB_XOR; break;
                    case EXPR_SHL:     n_type = TB_SHL; break;
                    case EXPR_SHR:     n_type = type->is_unsigned ? TB_SHR : TB_SAR; break;
                    default: TODO();
                }
                n = tb_builder_binop_int(g, n_type, lhs, rhs, ab);

                if (type->kind == KIND_BOOL) {
                    // convert into proper bool
                    TB_Node* con = tb_builder_uint(g, TB_TYPE_BOOL, 0);
                    n = tb_builder_cmp(g, TB_CMP_NE, false, n, con);
                }
            }

            return (Val){ RVALUE, .n = n };
        }

        case EXPR_CMPEQ:
        case EXPR_CMPNE: {
            return (Val){ RVALUE, .n = tb_builder_cmp(g, e->op == EXPR_CMPEQ ? TB_CMP_EQ : TB_CMP_NE, false, args[0].n, args[1].n) };
        }
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE: {
            bool flip = false;
            if (e->op == EXPR_CMPGT || e->op == EXPR_CMPGE) {
                flip = true;
            }

            TB_Node* lhs = as_rval(tu, g, &args[0]);
            TB_Node* rhs = as_rval(tu, g, &args[1]);

            TB_NodeTypeEnum n_type = TB_NULL;
            Cuik_Type* type = cuik_canonical_type(args[0].cast_type);
            if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
                switch (e->op) {
                    case EXPR_CMPGT:
                    case EXPR_CMPLT:
                    n_type = TB_CMP_FLT;
                    break;

                    case EXPR_CMPGE:
                    case EXPR_CMPLE:
                    n_type = TB_CMP_FLE;
                    break;

                    default: TODO();
                }
            } else {
                bool is_signed = type->is_unsigned;
                if (type->kind == KIND_PTR) { is_signed = false; }

                switch (e->op) {
                    case EXPR_CMPGT:
                    case EXPR_CMPLT:
                    n_type = is_signed ? TB_CMP_SLT : TB_CMP_ULT;
                    break;

                    case EXPR_CMPGE:
                    case EXPR_CMPLE:
                    n_type = is_signed ? TB_CMP_SLE : TB_CMP_ULE;
                    break;

                    default: TODO();
                }
            }
            return (Val){ RVALUE, .n = tb_builder_cmp(g, n_type, flip, lhs, rhs) };
        }

        case EXPR_PRE_INC:
        case EXPR_PRE_DEC:
        case EXPR_POST_INC:
        case EXPR_POST_DEC: {
            bool is_inc = (e->op == EXPR_PRE_INC || e->op == EXPR_POST_INC);
            bool is_pre = (e->op == EXPR_PRE_INC || e->op == EXPR_PRE_DEC);

            Cuik_Type* type  = cuik_canonical_type(qt);
            bool is_volatile = CUIK_QUAL_TYPE_HAS(qt, CUIK_QUAL_VOLATILE);
            TB_DataType dt   = ctype_to_tbtype(type);

            TB_Node* con = NULL;
            if (type->kind == KIND_PTR) {
                int64_t elem_size = cuik_canonical_type(type->ptr_to)->size;
                con = tb_builder_uint(g, TB_TYPE_I64, is_inc ? elem_size : -elem_size);
            } else {
                con = tb_builder_uint(g, dt, is_inc ? 1 : -1);
            }

            TB_Node* loaded = NULL;
            if (CUIK_QUAL_TYPE_HAS(qt, CUIK_QUAL_ATOMIC)) {
                // TB's atomic ops return the previous value so when we
                // deal with post-op exprs we'll redo the expression after
                // getting back the previous val.
                assert(args[0].kind == LVALUE && "unsupported increment/decrement value");

                loaded = tb_builder_atomic_rmw(g, 0, type->kind == KIND_PTR ? TB_ATOMIC_PTROFF : TB_ATOMIC_ADD, TB_MEM_ORDER_SEQ_CST, args[0].n, con);
                if (is_pre) {
                    return (Val){ RVALUE, .n = loaded };
                }
            } else {
                loaded = as_rval(tu, g, &args[0]);
            }

            TB_Node* operation;
            if (type->kind == KIND_PTR) {
                operation = tb_builder_ptr_array(g, loaded, con, 1);
            } else {
                TB_ArithmeticBehavior ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW;
                operation = tb_builder_binop_int(g, TB_ADD, loaded, con, ab);
            }

            // writeback (the atomic form does this all in one go... as atomics do)
            if (!CUIK_QUAL_TYPE_HAS(qt, CUIK_QUAL_ATOMIC)) {
                assign_to_lval(g, type, &args[0], operation, is_volatile);
            }

            return (Val){ RVALUE, .n = is_pre ? loaded : operation };
        }

        case EXPR_ASSIGN:
        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_PERCENT_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN: {
            Cuik_Type* type  = cuik_canonical_type(qt);
            bool is_volatile = CUIK_QUAL_TYPE_HAS(qt, CUIK_QUAL_VOLATILE);

            if (CUIK_QUAL_TYPE_HAS(qt, CUIK_QUAL_ATOMIC)) {
                // Atomics impls fall into 3 camps:
                // * Native support (just have some instructions for it).
                // * Unsupported op but supported size (we can emulate with a CAS).
                // * Unsupported size (we defer to a library call with a dumb lock).
                __debugbreak();
            } else if (e->op == EXPR_ASSIGN) {
                TB_Node* rhs = as_rval(tu, g, &args[1]);
                assign_to_lval(g, type, &args[0], rhs, is_volatile);
                return (Val){ RVALUE, .n = rhs };
            } else {
                TB_Node* lhs = as_rval(tu, g, &args[0]);

                // Try pointer arithmatic
                if ((e->op == EXPR_PLUS_ASSIGN || e->op == EXPR_MINUS_ASSIGN) && type->kind == KIND_PTR) {
                    int64_t stride = cuik_canonical_type(type->ptr_to)->size;
                    if (stride == 0) { stride = 1; }
                    if (e->op == EXPR_MINUS_ASSIGN) { stride = -stride; }

                    TB_Node* rhs = as_rval(tu, g, &args[1]);
                    TB_Node* arith = tb_builder_ptr_array(g, lhs, rhs, stride);

                    assert(args[0].kind == LVALUE);
                    tb_builder_store(g, 1, args[0].n, arith, type->align);
                    return (Val){ RVALUE, .n = arith };
                }

                TB_DataType dt = ctype_to_tbtype(type);
                if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                    TODO();
                } else if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
                    __debugbreak();
                } else {
                    TB_ArithmeticBehavior ab = 0;
                    TB_NodeTypeEnum n_type = TB_NULL;
                    switch (e->op) {
                        case EXPR_PLUS_ASSIGN:    n_type = TB_ADD, ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW; break;
                        case EXPR_MINUS_ASSIGN:   n_type = TB_SUB, ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW; break;
                        case EXPR_TIMES_ASSIGN:   n_type = TB_MUL, ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW; break;
                        case EXPR_SLASH_ASSIGN:   n_type = type->is_unsigned ? TB_UDIV : TB_SDIV; break;
                        case EXPR_PERCENT_ASSIGN: n_type = type->is_unsigned ? TB_UMOD : TB_SMOD; break;
                        case EXPR_AND_ASSIGN:     n_type = TB_AND; break;
                        case EXPR_OR_ASSIGN:      n_type = TB_OR;  break;
                        case EXPR_XOR_ASSIGN:     n_type = TB_XOR; break;
                        case EXPR_SHL_ASSIGN:     n_type = TB_SHL; break;
                        case EXPR_SHR_ASSIGN:     n_type = type->is_unsigned ? TB_SHR : TB_SAR; break;
                        default: TODO();
                    }

                    TB_Node* rhs = as_rval(tu, g, &args[1]);
                    TB_Node* op  = tb_builder_binop_int(g, n_type, lhs, rhs, ab);

                    if (type->kind == KIND_BOOL) {
                        // convert into proper bool
                        TB_Node* con = tb_builder_uint(g, TB_TYPE_BOOL, 0);
                        op = tb_builder_cmp(g, TB_CMP_NE, false, op, con);
                    }

                    assign_to_lval(g, type, &args[0], op, is_volatile);
                }
            }

            __debugbreak();
            return (Val){ RVALUE, .n = 0 };
        }

        default: TODO();
    }
}

static Val cg_expr(TranslationUnit* tu, TB_GraphBuilder* g, Cuik_Expr* restrict e) {
    Val stack[1024];

    size_t i = 0, top = 0;
    Subexpr* exprs = e->exprs;
    for (; i < e->count; i++) {
        Subexpr* s = &exprs[i];

        // once we know this we can organize the top slice of the stack as the inputs
        int arity = cuik_get_expr_arity(s);
        top -= arity;
        Val* args = &stack[top];

        stack[top] = cg_subexpr(tu, g, s, e->types[i], arity, args);
        stack[top].type = e->types[i];
        stack[top].cast_type = e->cast_types[i];
        top += 1;
    }

    assert(top == 1);
    return stack[0];
}

static TB_Node* cg_rval(TranslationUnit* tu, TB_GraphBuilder* g, Cuik_Expr* restrict e) {
    Val v = cg_expr(tu, g, e);
    return as_rval(tu, g, &v);
}

static void cg_stmt(TranslationUnit* tu, TB_GraphBuilder* g, Stmt* restrict s) {
    if (s == NULL) { return; }

    switch (s->op) {
        case STMT_DECL: {
            Attribs attrs = s->decl.attrs;

            Cuik_Type* type = cuik_canonical_type(s->decl.type);
            Cuik_TypeKind kind = type->kind;
            int size = type->size, align = type->align;

            size_t len = atoms_len(s->decl.name);
            if (attrs.is_static) {
                // Static initialization
                char* name = tls_push(1024);
                int name_len = snprintf(name, 1024, "%s.%s", function_name, s->decl.name);
                if (name_len < 0 || name_len >= 1024) {
                    assert(0 && "temporary global name too long!");
                }

                TB_DebugType* dbg_type = NULL;
                if (tu->has_tb_debug_info) {
                    dbg_type = cuik__as_tb_debug_type(tu->ir_mod, cuik_canonical_type(s->decl.type));
                }

                TB_Global* g = tb_global_create(tu->ir_mod, len, s->decl.name, dbg_type, TB_LINKAGE_PRIVATE);
                tls_restore(name);

                TB_ModuleSectionHandle section = get_variable_storage(tu->ir_mod, &attrs, s->decl.type.raw & CUIK_QUAL_CONST);

                int max_tb_objects = 0;
                if (s->decl.initial != NULL) {
                    Subexpr* initial = get_root_subexpr(s->decl.initial);

                    if (initial->op == EXPR_ADDR) {
                        max_tb_objects = 2;
                    } else if (initial->op == EXPR_INITIALIZER) {
                        max_tb_objects = count_max_tb_init_objects(initial->init.root);
                    } else {
                        max_tb_objects = 1;
                    }
                }

                tb_global_set_storage(tu->ir_mod, section, g, type->size, type->align, max_tb_objects);
                gen_global_initializer(tu, g, type, s->decl.initial, 0);

                if (attrs.is_tls) {
                    tb_module_set_tls_index(tu->ir_mod, sizeof("_tls_index")-1, "_tls_index");
                }

                s->backing.g = g;
                break;
            }

            if (kind == KIND_FUNC) {
                break;
            }

            TB_Node* addr = tb_builder_local(g, size, align);
            /* if (tu->has_tb_debug_info && s->decl.name != NULL) {
                tb_function_attrib_variable(func, addr, current_scope, len, s->decl.name, cuik__as_tb_debug_type(tu->ir_mod, type));
            } */

            if (s->decl.initial) {
                Subexpr* e = get_root_subexpr(s->decl.initial);
                if (e->op == EXPR_INITIALIZER) {
                    __debugbreak();
                    // gen_local_initializer(tu, func, addr, type, e->init.root);
                } else {
                    if (kind == KIND_ARRAY && (e->op == EXPR_STR || e->op == EXPR_WSTR)) {
                        Val v = cg_expr(tu, g, s->decl.initial);
                        TB_Node* con = tb_builder_uint(g, TB_TYPE_I64, size);
                        tb_builder_memcpy(g, 0, addr, v.n, con, align);
                    } else if (kind == KIND_STRUCT || kind == KIND_UNION) {
                        Val v = cg_expr(tu, g, s->decl.initial);
                        TB_Node* con = tb_builder_uint(g, TB_TYPE_I64, size);
                        tb_builder_memcpy(g, 0, addr, v.n, con, align);
                    } else {
                        TB_Node* v = cg_rval(tu, g, s->decl.initial);
                        tb_builder_store(g, 0, addr, v, align);
                    }
                }
            } else {
                /* uninitialized */
            }

            s->backing.n = addr;
            break;
        }

        case STMT_EXPR: {
            cg_expr(tu, g, s->expr.expr);
            break;
        }

        case STMT_COMPOUND: {
            Stmt** kids  = s->compound.kids;
            size_t count = s->compound.kids_count;

            for (size_t i = 0; i < count; i++) {
                cg_stmt(tu, g, kids[i]);
            }
            break;
        }

        case STMT_IF: {
            TB_Node* paths[2];
            TB_Node* cond = cg_rval(tu, g, s->if_.cond);

            TB_Node* merge = tb_builder_label_make(g);
            tb_builder_if(g, cond, paths);
            { // then
                tb_builder_label_set(g, paths[1]);
                cg_stmt(tu, g, s->if_.body);
                tb_builder_br(g, merge);
                tb_builder_label_kill(g, paths[1]);
            }
            { // else
                tb_builder_label_set(g, paths[0]);
                cg_stmt(tu, g, s->if_.next);
                tb_builder_br(g, merge);
                tb_builder_label_kill(g, paths[0]);
            }

            tb_builder_label_set(g, merge);

            // for whatever reason, neither path on the if joined back so this is just a dead label.
            if (merge->inputs[1]->input_count == 0) {
                tb_builder_label_kill(g, merge);
            }
            break;
        }

        case STMT_WHILE: {
            TB_Node* exit   = tb_builder_label_make(g);
            TB_Node* header = tb_builder_loop(g);

            s->backing.loop[0] = header;
            s->backing.loop[1] = exit;

            {
                TB_Node* paths[2];
                TB_Node* cond = cg_rval(tu, g, s->while_.cond);
                tb_builder_if(g, cond, paths);

                tb_builder_label_set(g, paths[1]);
                tb_builder_br(g, exit);
                tb_builder_label_kill(g, paths[1]);

                // loop body
                tb_builder_label_set(g, paths[0]);
                cg_stmt(tu, g, s->while_.body);
                tb_builder_br(g, header);
                tb_builder_label_kill(g, paths[0]);
            }
            tb_builder_label_complete(g, header);
            tb_builder_label_kill(g, header);

            tb_builder_label_set(g, exit);
            break;
        }

        case STMT_DO_WHILE: {
            TB_Node* exit   = tb_builder_label_make(g);
            TB_Node* header = tb_builder_loop(g);

            s->backing.loop[0] = header;
            s->backing.loop[1] = exit;

            {
                cg_stmt(tu, g, s->do_while.body);

                TB_Node* cond = cg_rval(tu, g, s->while_.cond);
                TB_Node* paths[2];
                tb_builder_if(g, cond, paths);

                tb_builder_label_set(g, paths[1]);
                tb_builder_br(g, exit);
                tb_builder_label_kill(g, paths[1]);

                tb_builder_label_set(g, paths[0]);
                tb_builder_br(g, header);
                tb_builder_label_kill(g, paths[0]);
            }
            tb_builder_label_complete(g, header);
            tb_builder_label_kill(g, header);

            tb_builder_label_set(g, exit);
            break;
        }

        case STMT_FOR: {
            if (s->for_.first) {
                cg_stmt(tu, g, s->for_.first);
            }

            TB_Node* exit   = tb_builder_label_make(g);
            TB_Node* header = tb_builder_loop(g);

            s->backing.loop[0] = header;
            s->backing.loop[1] = exit;

            {
                TB_Node* paths[2];
                TB_Node* cond = s->for_.cond ? cg_rval(tu, g, s->for_.cond) : tb_builder_uint(g, TB_TYPE_BOOL, 1);
                tb_builder_if(g, cond, paths);

                tb_builder_label_set(g, paths[1]);
                tb_builder_br(g, exit);
                tb_builder_label_kill(g, paths[1]);

                // loop body
                tb_builder_label_set(g, paths[0]);
                cg_stmt(tu, g, s->for_.body);
                if (s->for_.next) {
                    cg_expr(tu, g, s->for_.next);
                }
                tb_builder_br(g, header);
                tb_builder_label_kill(g, paths[0]);
            }
            tb_builder_label_complete(g, header);
            tb_builder_label_kill(g, header);

            tb_builder_label_set(g, exit);
            break;
        }

        case STMT_RETURN: {
            if (s->return_.expr == NULL) {
                tb_builder_ret(g, 0, 0, NULL);
                break;
            }

            Val v = cg_expr(tu, g, s->return_.expr);
            Cuik_Type* type = cuik_canonical_type(get_root_cast(s->return_.expr));
            if (func_return_rule == TB_PASSING_INDIRECT) {
                // returning aggregates just copies into the first parameter
                // which is agreed to be a caller owned buffer.
                int size = type->size, align = type->align;
                TB_Node* first_arg = tb_builder_get_var(g, 3);

                tb_builder_memcpy(g, 0, first_arg, v.n, tb_builder_uint(g, TB_TYPE_I64, size), align);
            } else {
                bool pushed = false;

                TB_Node* ret = NULL;
                if (v.kind == LVALUE) {
                    // implicit array to pointer
                    if (type->kind == KIND_ARRAY) {
                        pushed = true;
                        ret    = v.n;
                    } else if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                        assert(type->size <= 8);
                        TB_DataType dt = { { TB_TAG_INT, type->size * 8 } };

                        ret = tb_builder_load(g, 0, true, dt, v.n, type->align);
                        pushed = true;
                    }
                }

                // if it wasn't set before, resolve it now
                if (ret == NULL) {
                    ret = as_rval(tu, g, &v);
                }

                tb_builder_ret(g, 0, 1, &v.n);
            }
            break;
        }

        default: TODO();
    }
}

TB_Symbol* cuikcg_top_level(TranslationUnit* restrict tu, TB_Module* m, TB_Arena* ir, TB_Arena* tmp, Stmt* restrict s) {
    assert(s->flags & STMT_FLAGS_HAS_IR_BACKING);
    if (s->op == STMT_FUNC_DECL) {
        Cuik_Type* type = cuik_canonical_type(s->decl.type);
        assert(type->kind == KIND_FUNC);

        cached_file = NULL;
        cached_filepath = NULL;

        // Clear temporary storage
        tls_init();
        assert(type);

        TB_Function* func = s->backing.f;

        // we'll be using the debug info to construct our ABI compliant prototype
        TB_DebugType* dbg_type = cuik__as_tb_debug_type(m, type);
        TB_DebugType** dbg_params = tb_debug_func_params(dbg_type);

        TB_ModuleSectionHandle section = tb_module_get_text(m);
        if (s->decl.attrs.is_inline) {
            // make a comdat section
            section = tb_module_create_section(m, -1, ".text", TB_MODULE_SECTION_EXEC, TB_COMDAT_MATCH_ANY);
        }

        if (cuik_canonical_type(type->func.return_type)->kind != KIND_VOID) {
            TB_DebugType* dbg_ret = tb_debug_func_returns(dbg_type)[0];
            func_return_rule = tb_get_passing_rule_from_dbg(tu->ir_mod, dbg_ret, true);
        } else {
            func_return_rule = TB_PASSING_DIRECT;
        }

        // compile body
        {
            function_type = type;
            function_name = s->decl.name;

            TB_GraphBuilder* g = tb_builder_enter(func, ir, tmp, section, dbg_type, NULL);
            cg_stmt(tu, g, s->decl.initial_as_stmt);

            // we can reach the end, place the implicit return
            if (tb_builder_label_get(g) != NULL) {
                if (strcmp(s->decl.name, "main") == 0) {
                    TB_Node* exit_status = tb_builder_uint(g, TB_TYPE_I32, 0);
                    tb_builder_ret(g, 0, 1, &exit_status);
                } else {
                    tb_builder_ret(g, 0, 0, NULL);
                }
            }

            tb_builder_exit(g);

            function_name = NULL;
            function_type = 0;
        }

        #if 0
        if (tu->has_tb_debug_info) {
            emit_location(tu, func, s->decl.initial_as_stmt->loc.start);

            // mark where the return site is
            SourceLoc loc = s->decl.initial_as_stmt->loc.end;
            ResolvedSourceLoc rloc = cuikpp_find_location(&tu->tokens, loc);
            if (rloc.file->filename[0] != '<') {
                TB_SourceFile* f = tb_get_source_file(tu->ir_mod, -1, rloc.file->filename);
                tb_inst_set_exit_location(func, f, rloc.line, 0); // rloc.column);
            }
        }

        // compile body
        {
            function_type = type;
            function_name = s->decl.name;

            irgen_stmt(tu, g, s->decl.initial_as_stmt);

            function_name = NULL;
            function_type = 0;
        }

        // append return if none exists
        if (tb_inst_get_control(func) != NULL) {
            if (strcmp(s->decl.name, "main") == 0) {
                TB_Node* exit_status = tb_inst_uint(func, TB_TYPE_I32, 0);
                tb_inst_ret(func, 1, &exit_status);
            } else {
                tb_inst_ret(func, 0, NULL);
            }
        }
        #endif

        return (TB_Symbol*) func;
    } else if ((s->flags & STMT_FLAGS_HAS_IR_BACKING) && s->backing.s) {
        Cuik_Type* type = cuik_canonical_type(s->decl.type);
        Subexpr* initial = get_root_subexpr(s->decl.initial);

        if (s->decl.attrs.is_tls) {
            tb_module_set_tls_index(tu->ir_mod, sizeof("_tls_index")-1, "_tls_index");
        }

        TB_ModuleSectionHandle section = get_variable_storage(tu->ir_mod, &s->decl.attrs, s->decl.type.raw & CUIK_QUAL_CONST);
        int max_tb_objects;
        if (initial == NULL) {
            tb_global_set_storage(tu->ir_mod, section, (TB_Global*) s->backing.s, type->size, type->align, 0);
            return s->backing.s;
        } else if (initial->op == EXPR_ADDR) {
            max_tb_objects = 2;
        } else if (initial->op == EXPR_INITIALIZER) {
            max_tb_objects = count_max_tb_init_objects(initial->init.root);
        } else {
            max_tb_objects = 1;
        }

        tb_global_set_storage(tu->ir_mod, section, (TB_Global*) s->backing.s, type->size, type->align, max_tb_objects);
        gen_global_initializer(tu, (TB_Global*) s->backing.s, type, s->decl.initial, 0);
        return s->backing.s;
    }

    return NULL;
}
