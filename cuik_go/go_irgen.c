
typedef struct {
    enum {
        CUIKGO_RVALUE,

        // L-values
        CUIKGO_LVALUE,
        CUIKGO_LVALUE_SYM,
    } tag;
    CuikGo_Type* type;
    TB_Node* n;
} CuikGo_Val;

#define TB_TYPE_GCPTR TB_TYPE_PTRN(1)
#define TB_TYPE_GCREF TB_TYPE_PTRN(2)

static TB_Symbol* checkpoint_fn;
static TB_Symbol* lvb_trap_fn;
static TB_FunctionPrototype* checkpoint_proto;

static TB_Symbol* gc_alloc_fn;
static TB_FunctionPrototype* gc_alloc_proto;

// Checkpoints update root set references along with the NMT
static void insert_checkpoint(CuikGo_Parser* ctx, TB_GraphBuilder* g) {
    TB_Node* ptr = tb_builder_jit_thread_ptr(g);
    ptr = tb_builder_ptr_member(g, ptr, tb_jit_thread_userdata());

    TB_Node* merge = tb_builder_label_make(g);
    TB_Node* n = tb_builder_load(g, 0, true, TB_TYPE_I8, ptr, 1, false);

    TB_Node* paths[2];
    TB_Node* if_n = tb_builder_if(g, n, paths);
    TB_NODE_SET_EXTRA(if_n, TB_NodeIf, .prob = 0.0001f);
    {
        tb_builder_label_set(g, paths[0]);
        tb_builder_call(g, checkpoint_proto, 0, tb_builder_symbol(g, checkpoint_fn), 0, NULL);
        tb_builder_br(g, merge);
    }
    tb_builder_label_set(g, paths[1]);
    tb_builder_br(g, merge);

    tb_builder_label_set(g, merge);
}

static TB_Node* to_rval(CuikGo_Parser* ctx, TB_GraphBuilder* g, CuikGo_Val* v) {
    if (v->tag == CUIKGO_LVALUE) {
        TB_DataType dt = TB_TYPE_VOID;
        switch (v->type->tag) {
            case CUIKGO_INT:     dt = TB_TYPE_I64; break;
            case CUIKGO_FLOAT64: dt = TB_TYPE_F64; break;
            // this is a structure, we can only directly refer to it via pointer
            case CUIKGO_MAP:     dt = TB_TYPE_PTR; break;
            default: TB_ASSERT(0);
        }
        return tb_builder_load(g, 0, true, dt, v->n, v->type->align, false);
    } else if (v->tag == CUIKGO_RVALUE) {
        return v->n;
    } else {
        TB_ASSERT(0);
    }
}

void cuikgo_ir_gen(CuikGo_Parser* ctx, TB_GraphBuilder* g, CuikGo_Word* start, CuikGo_Word* end);
CuikGo_Word* cuikgo_ir_gen_word(CuikGo_Parser* ctx, TB_GraphBuilder* g, CuikGo_Word* w, int arity, CuikGo_Val* args) {
    switch (w->tag) {
        case WORD_CONSUME: break;

        case WORD_DECL:
        case WORD_DECL_REF: {
            CuikGo_Type* type = w->type;
            w->ir.n = tb_builder_local(g, type->size, type->align);
            if (w->tag == WORD_DECL_REF) {
                args[0] = (CuikGo_Val){ CUIKGO_LVALUE, .type = w->type, .n = w->ir.n };
            }

            if (arity == 1) {
                TB_Node* src = to_rval(ctx, g, &args[0]);
                tb_builder_store(g, 0, true, w->ir.n, src, 8, false);
            }
            break;
        }

        case WORD_REF: {
            args[0] = (CuikGo_Val){ CUIKGO_LVALUE, .type = w->type, .n = w->ref->ir.n };
            break;
        }

        case WORD_INT: {
            args[0] = (CuikGo_Val){ CUIKGO_RVALUE, .type = w->type, .n = tb_builder_sint(g, TB_TYPE_I64, w->iconst) };
            break;
        }

        case WORD_NEW: {
            TB_ASSERT(w->type->tag == CUIKGO_PTR);
            size_t size = w->type->val_type->size;

            TB_Node* call_args[2];
            call_args[0] = tb_builder_sint(g, TB_TYPE_I64, size);
            call_args[1] = tb_builder_sint(g, TB_TYPE_I32, 0);

            TB_Node** ref = tb_builder_call(g, gc_alloc_proto, 0, tb_builder_symbol(g, gc_alloc_fn), 2, call_args);
            args[0] = (CuikGo_Val){ CUIKGO_RVALUE, .type = w->type, .n = ref[0] };
            break;
        }

        case WORD_CMPGT: {
            TB_Node* lhs = to_rval(ctx, g, &args[0]);
            TB_Node* rhs = to_rval(ctx, g, &args[1]);
            args[0] = (CuikGo_Val){ CUIKGO_RVALUE, .type = w->type, .n = tb_builder_cmp(g, TB_CMP_ULT, rhs, lhs) };
            break;
        }

        case WORD_ASSIGN: {
            TB_Node* rhs = to_rval(ctx, g, &args[1]);
            TB_ASSERT(args[0].tag == CUIKGO_LVALUE);

            tb_builder_store(g, 0, true, args[0].n, rhs, 8, false);
            args[0] = (CuikGo_Val){ CUIKGO_RVALUE, .type = w->type, .n = rhs };
            break;
        }

        case WORD_FIELD: {
            TB_ASSERT(args[0].tag == CUIKGO_LVALUE);
            TB_Node* base = args[0].n;
            CuikGo_Type* container = args[0].type;
            if (container->tag == CUIKGO_PTR) {
                base = tb_builder_load(g, 0, false, TB_TYPE_GCPTR, args[0].n, 8, false);
                container = container->val_type;
            }

            int64_t offset = -1;
            TB_ASSERT(container->tag == CUIKGO_STRUCT);
            dyn_array_for(i, container->fields) {
                if (container->fields[i].name == w->name) {
                    offset = container->fields[i].offset;
                    break;
                }
            }
            TB_ASSERT(offset >= 0);

            TB_Node* ptr = tb_builder_ptr_member(g, base, offset);
            args[0] = (CuikGo_Val){ CUIKGO_LVALUE, .type = w->type, .n = ptr };
            break;
        }

        case WORD_INDEX: {
            TB_ASSERT(args[0].tag == CUIKGO_LVALUE);
            TB_Node* index = to_rval(ctx, g, &args[1]);

            // array indexing
            TB_ASSERT(args[0].type->tag == CUIKGO_MAP);
            CuikGo_Type* val_type = args[0].type->val_type;
            if (args[0].type->key_type != NULL) {
                TB_ASSERT(0);
            } else {
                TB_Node* base = tb_builder_load(g, 0, false, TB_TYPE_GCPTR, args[0].n, 8, false);

                TB_Node* add = tb_builder_ptr_array(g, base, index, val_type->size);
                args[0] = (CuikGo_Val){ CUIKGO_LVALUE, .type = w->type, .n = add };
                break;
            }
        }

        case WORD_FOR_RANGE: {
            // (key, value, map/array)
            TB_ASSERT(args[0].tag == CUIKGO_LVALUE && args[1].tag == CUIKGO_LVALUE);
            TB_Node* key = args[0].n;
            TB_Node* val = args[1].n;

            TB_Node* iter = args[2].n;
            TB_ASSERT(args[1].tag == CUIKGO_LVALUE);

            TB_Node* base  = tb_builder_load(g, 0, false, TB_TYPE_GCPTR, iter, 8, false);
            TB_Node* limit = tb_builder_load(g, 0, false, TB_TYPE_I64, tb_builder_ptr_member(g, iter, 8), 8, false);
            CuikGo_Type* val_type = &FLOAT64_TYPE;
            TB_Node* exit = tb_builder_label_make(g);

            int index_var = tb_builder_decl(g, tb_builder_label_get(g));
            tb_builder_set_var(g, index_var, tb_builder_sint(g, TB_TYPE_I64, 0));

            TB_Node* header = tb_builder_loop(g);
            TB_Node* loop   = tb_builder_label_clone(g, header);
            {
                TB_Node* paths[2];
                // while (has_next()) {
                TB_Node* index  = tb_builder_get_var(g, index_var);
                TB_Node* cond   = tb_builder_cmp(g, TB_CMP_ULT, index, limit);
                tb_builder_if(g, cond, paths);


                // exit path
                tb_builder_label_set(g, paths[1]);
                tb_builder_br(g, exit);
                tb_builder_label_kill(g, paths[1]);


                // in body
                tb_builder_label_set(g, paths[0]);
                insert_checkpoint(ctx, g);

                // fetch val
                index = tb_builder_get_var(g, index_var);
                TB_Node* val_ld = tb_builder_load(g, 0, true, TB_TYPE_F64, tb_builder_ptr_array(g, base, index, val_type->size), 8, false);
                tb_builder_store(g, 0, true, key, index,  val_type->align, false);
                tb_builder_store(g, 0, true, val, val_ld, val_type->align, false);
                cuikgo_ir_gen(ctx, g, next_word(w), w->ref);
                // advance
                index = tb_builder_get_var(g, index_var);
                tb_builder_set_var(g, index_var, tb_builder_binop_int(g, TB_ADD, index, tb_builder_uint(g, TB_TYPE_I64, 1), 0));
                tb_builder_br(g, loop);
                tb_builder_label_kill(g, paths[0]);
            }
            tb_builder_label_kill(g, loop);
            tb_builder_label_kill(g, header);

            tb_builder_label_set(g, exit);
            return w->ref;
        }
        case WORD_IF: {
            TB_Node* paths[2];
            TB_Node* cond = to_rval(ctx, g, &args[0]);

            TB_Node* merge = tb_builder_label_make(g);
            tb_builder_if(g, cond, paths);
            { // then
                tb_builder_label_set(g, paths[0]);
                cuikgo_ir_gen(ctx, g, next_word(w), w->ref);
                tb_builder_br(g, merge);
                tb_builder_label_kill(g, paths[0]);
            }
            TB_ASSERT(w->ref->tag == WORD_ELSE);
            CuikGo_Word* merge_pos = w->ref->ref;
            { // else
                tb_builder_label_set(g, paths[1]);
                if (next_word(w->ref) != merge_pos) {
                    cuikgo_ir_gen(ctx, g, next_word(w->ref), merge_pos);
                }
                tb_builder_br(g, merge);
                tb_builder_label_kill(g, paths[1]);
            }

            tb_builder_label_set(g, merge);

            // for whatever reason, neither path on the if joined back so this is just a dead label.
            if (merge->inputs[1]->input_count == 0) {
                tb_builder_label_kill(g, merge);
            }
            return merge_pos;
        }

        default: TB_ASSERT(0);
    }
    return next_word(w);
}

void cuikgo_ir_gen(CuikGo_Parser* ctx, TB_GraphBuilder* g, CuikGo_Word* start, CuikGo_Word* end) {
    CuikGo_Val stack[64];
    size_t top = 0;

    CuikGo_Word* w = start;
    while (w != end) {
        // once we know this we can organize the top slice of the stack as the inputs
        top -= w->arity;
        CuikGo_Val* args = &stack[top];
        CuikGo_Word* next = cuikgo_ir_gen_word(ctx, g, w, w->arity, args);

        // push to stack
        if (w->tag < WORD_STMTS_PAST_THIS) {
            top += 1;
        }
        w = next;
    }
}

