
#include <new_hash_map.h>
#include <hash_map.h>
#include <dyn_array.h>
#include <buffer.h>

typedef struct {
    size_t gvn;
    size_t label;
    bool used: 1;
} CFmtFrame;

typedef struct {
    size_t gvn;
    size_t label;
    bool used: 1;
} CFmtFallNext;

typedef struct {
    size_t low;
    size_t high;
} CFmtBlockRange;

typedef struct {
    const char *name;
    TB_Function* f;
    TB_Module *module;
    size_t a;
    size_t num_labels;
    NL_HashSet declared_types;
    DynArray(CFmtFrame *) visited_blocks;
    NL_Table block_ranges;
    TB_CFG cfg;
    NL_HashSet declared_vars;
    nl_buffer_t *globals;
    nl_buffer_t *pre;
    nl_buffer_t *buf;
    ptrdiff_t loop_goes_to;
    int depth;
} CFmtState;

static void c_fmt_ref_to_node(CFmtState* ctx, TB_Node* n);
static void c_fmt_typed_ref_to_node(CFmtState* ctx, TB_DataType dt, TB_Node* n);

static void c_fmt_spaces(CFmtState *ctx) {
    for (unsigned char i = 0; i < ctx->depth; i++) {
        nl_buffer_format(ctx->buf, "  ");
    }
}

static const char *c_fmt_type_name(TB_DataType dt) {
    switch (dt.type) {
        case TB_TAG_INT: {
            if (dt.data == 0) return  "void";
            if (dt.data == 1) return  "char";
            if (dt.data <= 8) return  "uint8_t";
            if (dt.data <= 16) return  "uint16_t";
            if (dt.data <= 32) return  "uint32_t";
            if (dt.data <= 64) return  "uint64_t";
            else __builtin_trap();
            break;
        }
        case TB_TAG_PTR: {
            if (dt.data == 0) return  "void*";
            else tb_todo();
            break;
        }
        case TB_TAG_F32: return "float";
        case TB_TAG_F64: return "double";
        default: tb_todo();
    }
    return NULL;
}

static const char *c_fmt_type_name_signed(TB_DataType dt) {
    switch (dt.type) {
        case TB_TAG_INT: {
            if (dt.data == 0) return  "void";
            if (dt.data == 1) return  "char";
            if (dt.data <= 8) return  "int8_t";
            if (dt.data <= 16) return  "int16_t";
            if (dt.data <= 32) return  "int32_t";
            if (dt.data <= 64) return  "int64_t";
            else __builtin_trap();
            break;
        }
        case TB_TAG_PTR: {
            if (dt.data == 0) return  "void*";
            else tb_todo();
            break;
        }
        case TB_TAG_F32: return "float";
        case TB_TAG_F64: return "double";
        default: tb_todo();
    }
    return NULL;
}

static void c_fmt_output(CFmtState* ctx, TB_Node* n) {
    if (n->users != NULL) {
        if (!nl_hashset_put(&ctx->declared_vars, n)) {
            nl_buffer_format(ctx->pre, "  %s v%u;\n", c_fmt_type_name(n->dt), n->gvn);
        }
        c_fmt_spaces(ctx);
        nl_buffer_format(ctx->buf, "v%u = ", n->gvn);
    }
}

static bool c_fmt_will_inline(TB_Node *n) {
    switch (n->type) {
        case TB_ROOT: case TB_ICONST:
        case TB_REGION: case TB_NATURAL_LOOP: case TB_AFFINE_LOOP:
        case TB_F32CONST: case TB_F64CONST:
        case TB_SYMBOL: case TB_ZERO_EXT: case TB_SIGN_EXT:
        return true;

        case TB_PROJ: if (n->dt.type == TB_TAG_CONTROL) { return true; }
        break;
    }

    size_t len = n->user_count;
    FOR_USERS(head, n) {
        // counts as two uses in the C code
        if (USERN(head)->type == TB_ROL) len += 1;
    }

    if (len <= 1) switch (n->type) {
        case TB_SELECT:
        case TB_SHL: case TB_SHR: case TB_SAR:
        case TB_AND: case TB_XOR: case TB_OR:
        case TB_ADD: case TB_FADD:
        case TB_SUB: case TB_FSUB:
        case TB_MUL: case TB_FMUL:
        case TB_UDIV: case TB_SDIV: case TB_FDIV:
        case TB_CMP_EQ: case TB_CMP_NE:
        case TB_CMP_SLT: case TB_CMP_ULT: case TB_CMP_FLT:
        case TB_CMP_SLE: case TB_CMP_ULE: case TB_CMP_FLE:
        return true;
    }
    return false;
}

static void c_fmt_inline_node(CFmtState* ctx, TB_Node *n) {
    const char* binop = NULL;
    bool is_signed = false;
    switch (n->type) {
        // is_signed fucks up my table's vibes :(
        case TB_SHL:  binop = "<<"; break;
        case TB_SHR:  binop = ">>"; break;
        case TB_AND:  binop = "&";  break;
        case TB_OR:   binop = "|";  break;
        case TB_XOR:  binop = "^";  break;
        case TB_ADD:  binop = "+";  break;
        case TB_SUB:  binop = "-";  break;
        case TB_MUL:  binop = "*";  break;
        case TB_UDIV: binop = "/";  break;
        case TB_SDIV: binop = "/"; is_signed = true; break;
        case TB_FADD: binop = "+";  break;
        case TB_FSUB: binop = "-";  break;
        case TB_FMUL: binop = "*";  break;
        case TB_FDIV: binop = "/";  break;
    }

    if (binop) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        const char* tn = is_signed ? c_fmt_type_name_signed(n->dt) : c_fmt_type_name(n->dt);

        nl_buffer_format(ctx->buf, "(");
        nl_buffer_format(ctx->buf, "(%s) ", tn);
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " %s ", binop);
        nl_buffer_format(ctx->buf, "(%s) ", tn);
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_SELECT) {
        TB_Node *cond = n->inputs[n->input_count-3];
        TB_Node *then = n->inputs[n->input_count-2];
        TB_Node *els = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        nl_buffer_format(ctx->buf, "(char) ");
        c_fmt_ref_to_node(ctx, cond);
        nl_buffer_format(ctx->buf, " ? ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
        c_fmt_ref_to_node(ctx, then);
        nl_buffer_format(ctx->buf, " : ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
        c_fmt_ref_to_node(ctx, els);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_EQ) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " == ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_NE) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " != ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_SLT) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " < ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_SLE) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " <= ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_ULT) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " < ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_ULE) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " <= ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_FLT) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " < ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_CMP_FLE) {
        TB_Node *lhs = n->inputs[n->input_count-2];
        TB_Node *rhs = n->inputs[n->input_count-1];
        nl_buffer_format(ctx->buf, "(");
        c_fmt_ref_to_node(ctx, lhs);
        nl_buffer_format(ctx->buf, " <= ");
        nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(lhs->dt));
        c_fmt_ref_to_node(ctx, rhs);
        nl_buffer_format(ctx->buf, ")");
    } else if (n->type == TB_F32CONST) {
        TB_NodeFloat32* f = TB_NODE_GET_EXTRA(n);
        nl_buffer_format(ctx->buf, "%f", f->value);
    } else if (n->type == TB_F64CONST) {
        TB_NodeFloat64* f = TB_NODE_GET_EXTRA(n);
        nl_buffer_format(ctx->buf, "%f", f->value);
    } else if (n->type == TB_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
        if (sym->name[0]) {
            if (sym->tag == TB_SYMBOL_GLOBAL) {
                nl_buffer_format(ctx->buf, "tb2c_sym%zu_%s", sym->symbol_id, sym->name);
            } else {
                nl_buffer_format(ctx->buf, "%s", sym->name);
            }
        } else if (ctx->module->is_jit) {
            void *addr = sym->address;
            nl_buffer_format(ctx->buf, "(void*)%p", TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym->address);
        } else {
            switch((int) sym->tag) {
                case TB_SYMBOL_EXTERNAL: {
                    nl_buffer_format(ctx->buf, "(void*)0");
                    break;
                }
                case TB_SYMBOL_GLOBAL: {
                    TB_Global *g = (void *) sym;
                    uint8_t *data = tb_platform_heap_alloc(g->size);

                    memset(&data[g->pos], 0, g->size);
                    FOR_N(k, 0, g->obj_count) {
                        if (g->objects[k].type == TB_INIT_OBJ_REGION) {
                            assert(g->objects[k].offset + g->objects[k].region.size <= g->size);
                            memcpy(&data[g->pos + g->objects[k].offset], g->objects[k].region.ptr, g->objects[k].region.size);
                        }
                    }

                    nl_buffer_format(ctx->buf, "\"");
                    FOR_N(k, 0, g->size) {
                        if (data[k] == 0 && k + 1 == g->size) {
                            break;
                        }
                        if (32 <= data[k] && data[k] <= 126) {
                            nl_buffer_format(ctx->buf, "%c", data[k]);
                        } else if (data[k] == '\t') {
                            nl_buffer_format(ctx->buf, "\\t");
                        } else if (data[k] == '\r') {
                            nl_buffer_format(ctx->buf, "\\r");
                        } else if (data[k] == '\n') {
                            nl_buffer_format(ctx->buf, "\\n");
                        } else if (data[k] == ' ') {
                            nl_buffer_format(ctx->buf, " ");
                        } else {
                            nl_buffer_format(ctx->buf, "\\x%02x", data[k]);
                        }
                    }
                    nl_buffer_format(ctx->buf, "\"");

                    tb_platform_heap_free(data);
                    break;
                }
                case TB_SYMBOL_FUNCTION: {
                    nl_buffer_format(ctx->buf, "(void *) 0");
                    break;
                }
            }
        }
    } else if (n->type == TB_ZERO_EXT) {
        c_fmt_ref_to_node(ctx, n->inputs[1]);
    } else if (n->type == TB_SIGN_EXT) {
        c_fmt_ref_to_node(ctx, n->inputs[1]);
    } else if (n->type == TB_ICONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

        nl_buffer_format(ctx->buf, "(%s)", c_fmt_type_name(n->dt));

        if (num->value < 0xFFFF) {
            nl_buffer_format(ctx->buf, "%"PRIi64"llu", num->value);
        } else {
            nl_buffer_format(ctx->buf, "%#0"PRIx64"llu", num->value);
        }
    } else {
        assert(false && "wrong node type");
    }
}

static void c_fmt_ref_to_node(CFmtState* ctx, TB_Node* n) {
    if (c_fmt_will_inline(n)) {
        c_fmt_inline_node(ctx, n);
    } else {
        nl_buffer_format(ctx->buf, "v%u", n->gvn);
        return;
    }
}

static void c_fmt_typed_ref_to_node(CFmtState* ctx, TB_DataType dt, TB_Node *n) {
    nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(dt));
    c_fmt_ref_to_node(ctx, n);
}

CFmtBlockRange *c_fmt_get_block_range(CFmtState* ctx, TB_Worklist* ws, TB_Node* n) {
    if (nl_table_get(&ctx->block_ranges, n) == NULL) {
        TB_BasicBlock* bb = ctx->f->scheduled[n->gvn];

        size_t foreach_start = dyn_array_length(ws->items);
        tb_greedy_scheduler(ctx->f, &ctx->cfg, ws, NULL, bb);
        size_t foreach_end = dyn_array_length(ws->items);

        CFmtBlockRange *range = tb_platform_heap_alloc(sizeof(CFmtBlockRange));

        range->low = foreach_start;
        range->high = foreach_end;

        nl_table_put(&ctx->block_ranges, n, range);
    }
    return nl_table_get(&ctx->block_ranges, n);
}

// deals with printing BB params
static void c_fmt_branch_edge(CFmtState* ctx, TB_Node* n, bool fallthru) {
    TB_Node* target = fallthru ? cfg_next_control(n) : cfg_next_bb_after_cproj(n);

    if (cfg_is_region(target)) {
        int phi_i = -1;
        FOR_USERS(u, n) {
            if (cfg_is_region(USERN(u))) {
                phi_i = 1 + USERI(u);
                break;
            }
        }

        c_fmt_spaces(ctx);
        nl_buffer_format(ctx->buf, "{ // phi moves\n");
        ctx->depth += 1;
        size_t has_phi = 0;
        FOR_USERS(u, target) {
            if (USERN(u)->type == TB_PHI) {
                if (USERN(u)->inputs[phi_i] != NULL) {
                    if (USERN(u)->inputs[phi_i]->dt.type != TB_TAG_CONTROL && USERN(u)->inputs[phi_i]->dt.type != TB_TAG_MEMORY) {
                        has_phi += 1;
                    }
                }
            }
        }
        ctx->a += has_phi;
        size_t pos = 0;
        FOR_USERS(u, target) {
            if (USERN(u)->type == TB_PHI) {
                if (USERN(u)->inputs[phi_i] != NULL) {
                    if (USERN(u)->inputs[phi_i]->dt.type != TB_TAG_CONTROL && USERN(u)->inputs[phi_i]->dt.type != TB_TAG_MEMORY) {
                        assert(phi_i >= 0);
                        if (pos != 0) {
                            c_fmt_spaces(ctx);
                            nl_buffer_format(ctx->buf, "%s a%zu = ", c_fmt_type_name(USERN(u)->dt), ctx->a + pos);
                            c_fmt_ref_to_node(ctx, USERN(u)->inputs[phi_i]);
                            nl_buffer_format(ctx->buf, ";\n");
                        }
                        pos += 1;
                    }
                }
            }
        }
        pos = 0;
        FOR_USERS(u, target) {
            if (USERN(u)->type == TB_PHI) {
                if (USERN(u)->inputs[phi_i] != NULL) {
                    if (USERN(u)->inputs[phi_i]->dt.type != TB_TAG_CONTROL && USERN(u)->inputs[phi_i]->dt.type != TB_TAG_MEMORY) {
                        assert(phi_i >= 0);
                        if (pos == 0) {
                            c_fmt_output(ctx, USERN(u));
                            c_fmt_ref_to_node(ctx, USERN(u)->inputs[phi_i]);
                            nl_buffer_format(ctx->buf, ";\n");
                        } else {
                            c_fmt_output(ctx, USERN(u));
                            nl_buffer_format(ctx->buf, "a%zu;\n", ctx->a + pos);
                        }
                        pos += 1;
                    }
                }
            }
        }
        ctx->depth -= 1;
        c_fmt_spaces(ctx);
        nl_buffer_format(ctx->buf, "}\n");
    }

    c_fmt_spaces(ctx);
    nl_buffer_format(ctx->buf, "goto bb%u;\n", target->gvn);
}

static void c_fmt_bb(CFmtState* ctx, TB_Worklist* ws, TB_Node* bb_start) {
    size_t declared_vars_length = dyn_array_length(&ctx->declared_vars);
    nl_buffer_format(ctx->buf, "bb%u:\n", bb_start->gvn);

    TB_BasicBlock* bb = ctx->f->scheduled[bb_start->gvn];
    CFmtBlockRange *range = c_fmt_get_block_range(ctx, ws, bb_start);

    TB_Node* prev_effect = NULL;
    FOR_N(i, range->low, range->high) {
        TB_Node* n = ws->items[i];

        // skip these
        if (c_fmt_will_inline(n) && n->type != TB_ROOT) {
            continue;
        }

        if (n->type == TB_MERGEMEM || n->type == TB_SPLITMEM || n->type == TB_NULL || n->type == TB_PHI || n->type == TB_PROJ) {
            continue;
        }

        switch (n->type) {
            case TB_DEAD: {
                c_fmt_spaces(ctx);
                nl_buffer_format(ctx->buf, "// dead\n");
                break;
            }

            case TB_DEBUGBREAK: {
                c_fmt_spaces(ctx);
                nl_buffer_format(ctx->buf, "// debugbreak\n");
                break;
            }
            case TB_UNREACHABLE: {
                c_fmt_spaces(ctx);
                nl_buffer_format(ctx->buf, "// unreachable\n");
                break;
            }

            case TB_BRANCH: {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
                TB_ArenaSavepoint sp = tb_arena_save(ctx->f->tmp_arena);
                TB_Node** restrict succ = tb_arena_alloc(ctx->f->arena, br->succ_count * sizeof(TB_Node**));

                size_t succ_count = 0;

                // fill successors
                FOR_USERS(u, n) {
                    if (USERN(u)->type == TB_PROJ) {
                        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                        succ[index] = USERN(u);
                        succ_count += 1;
                    }
                }

                TB_NodeBranchProj* if_br = cfg_if_branch(n);
                if (if_br) {
                    if (if_br->key == 0) {
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "if (");
                        FOR_N(i, 1, n->input_count) {
                            if (i != 1) nl_buffer_format(ctx->buf, ", ");
                            c_fmt_ref_to_node(ctx, n->inputs[i]);
                        }
                        nl_buffer_format(ctx->buf, ") {\n");
                        ctx->depth += 1;
                        c_fmt_branch_edge(ctx, succ[0], false);
                        ctx->depth -= 1;
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "} else {\n");
                        ctx->depth += 1;
                        c_fmt_branch_edge(ctx, succ[1], false);
                        ctx->depth -= 1;
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "}\n");
                    } else {
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "if ((uint64_t) ");
                        FOR_N(i, 1, n->input_count) {
                            if (i != 1) nl_buffer_format(ctx->buf, ", ");
                            c_fmt_ref_to_node(ctx, n->inputs[i]);
                        }
                        nl_buffer_format(ctx->buf, " == (uint64_t) %"PRIi64, if_br->key);
                        nl_buffer_format(ctx->buf, ") {\n");
                        ctx->depth += 1;
                        c_fmt_branch_edge(ctx, succ[1], false);
                        ctx->depth -= 1;
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "} else {\n");
                        ctx->depth += 1;
                        c_fmt_branch_edge(ctx, succ[0], false);
                        ctx->depth -= 1;
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "}\n");
                    }
                } else {
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "switch ((uint64_t) ");
                    FOR_N(i, 1, n->input_count) {
                        if (i != 1) nl_buffer_format(ctx->buf, ", ");
                        c_fmt_ref_to_node(ctx, n->inputs[i]);
                    }
                    nl_buffer_format(ctx->buf, ") {\n");
                    FOR_N(i, 1, succ_count) {
                        TB_Node* proj = USERN(proj_with_index(n, i));
                        TB_NodeBranchProj* p = TB_NODE_GET_EXTRA(proj);

                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "case %"PRIi64"llu:\n", p->key);
                        ctx->depth += 1;
                        c_fmt_branch_edge(ctx, succ[i], false);
                        ctx->depth -= 1;
                    }
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "default:\n");
                    ctx->depth += 1;
                    c_fmt_branch_edge(ctx, succ[0], false);
                    ctx->depth -= 1;
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "}\n");
                }
                tb_arena_restore(ctx->f->tmp_arena, sp);
                break;
            }

            case TB_TRAP: {
                c_fmt_spaces(ctx);
                nl_buffer_format(ctx->buf, "throw new Error(\"trap\");\n");
                break;
            }

            case TB_TAILCALL: {
                TB_Node *func = n->inputs[2];

                TB_FunctionPrototype *proto = ctx->f->prototype;
                size_t nrets = proto->return_count;
                TB_PrototypeParam *rets = &proto->params[proto->param_count];

                if (!nl_hashset_put(&ctx->declared_types, n)) {
                    if (nrets == 0) {
                        nl_buffer_format(ctx->globals, "typedef void(*tb2c_%s_v%u_t)(", ctx->name, n->gvn);
                    } else if (nrets == 1) {
                        nl_buffer_format(ctx->globals, "typedef %s(*tb2c_%s_v%u_t)(", c_fmt_type_name(rets[0].dt), ctx->name, n->gvn);
                    } else {
                        nl_buffer_format(ctx->globals, "typedef struct {\n");
                        size_t index = 0;
                        FOR_N(i, 0, nrets) {
                            nl_buffer_format(ctx->globals, "  %s v%zu;\n", c_fmt_type_name(rets[i].dt), index);
                            index += 1;
                        }
                        nl_buffer_format(ctx->globals, "} tb2c_%s_v%u_ret_t;\n", ctx->name, n->gvn);
                        nl_buffer_format(ctx->globals, "typedef tb2c_%s_v%u_ret_t(*tb2c_%s_v%u_t)(", ctx->name, n->gvn, ctx->name, n->gvn);
                    }
                    bool first = true;
                    FOR_N(i, 3, n->input_count) {
                        if (n->inputs[i]->dt.type != TB_TAG_CONTROL && n->inputs[i]->dt.type != TB_TAG_MEMORY) {
                            if (!first) {
                                nl_buffer_format(ctx->globals, ", ");
                            }
                            nl_buffer_format(ctx->globals, "%s", c_fmt_type_name(n->inputs[i]->dt));
                            first = false;
                        }
                    }
                    if (first) {
                        nl_buffer_format(ctx->globals, "void");
                    }
                    nl_buffer_format(ctx->globals, ");\n");
                }
                if (nrets == 0 || nrets == 1) {
                    c_fmt_spaces(ctx);
                    if (nrets == 1) {
                        nl_buffer_format(ctx->buf, "return (tb2c_%s_ret_t) ", ctx->name);
                    }
                    nl_buffer_format(ctx->buf, "((tb2c_%s_v%u_t) ", ctx->name, n->gvn);
                    c_fmt_ref_to_node(ctx, func);
                    nl_buffer_format(ctx->buf, ")");
                    nl_buffer_format(ctx->buf, "(");
                    {
                        bool first = true;
                        FOR_N(i, 3, n->input_count) {
                            if (n->inputs[i]->dt.type != TB_TAG_CONTROL && n->inputs[i]->dt.type != TB_TAG_MEMORY) {
                                if (!first) {
                                    nl_buffer_format(ctx->buf, ", ");
                                }
                                c_fmt_ref_to_node(ctx, n->inputs[i]);
                                first = false;
                            }
                        }
                    }
                    nl_buffer_format(ctx->buf, ");\n");
                    if (nrets == 0) {
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "return;\n");
                    }
                } else {
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "tb2c_%s_v%u_ret_t v%u_ret = ", ctx->name, n->gvn, n->gvn);
                    nl_buffer_format(ctx->buf, "((tb2c_%s_v%u_t) ", ctx->name, n->gvn);
                    c_fmt_ref_to_node(ctx, func);
                    nl_buffer_format(ctx->buf, ")(");
                    {
                        bool first = true;
                        FOR_N(i, 3, n->input_count) {
                            if (n->inputs[i]->dt.type != TB_TAG_CONTROL && n->inputs[i]->dt.type != TB_TAG_MEMORY) {
                                if (!first) {
                                    nl_buffer_format(ctx->buf, ", ");
                                }
                                c_fmt_ref_to_node(ctx, n->inputs[i]);
                                first = false;
                            }
                        }
                    }
                    nl_buffer_format(ctx->buf, ");\n");
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "{\n");
                    ctx->depth += 1;
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "tb2c_%s_ret_t ret;\n", ctx->name);
                    size_t index = 0;
                    FOR_N(i, 0, nrets) {
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "ret.v%zu = v%u_ret.v%zu;\n", index, n->gvn, index);
                        index += 1;
                    }
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "return (tb2c_%s_ret_t) ret;\n", ctx->name);
                    ctx->depth -= 1;
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "}\n");
                }
                break;
            }

            case TB_RETURN: {
                size_t count = 0;
                FOR_N(i, 3, n->input_count) {
                    count += 1;
                }
                if (count == 0) {
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "return;\n");
                } else if (count == 1) {
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "return ");
                    FOR_N(i, 3, n->input_count) {
                        c_fmt_ref_to_node(ctx, n->inputs[i]);
                    }
                    nl_buffer_format(ctx->buf, ";\n");
                } else {
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "{\n");
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "  tb2c_%s_ret_t ret;\n", ctx->name);

                    size_t index = 0;
                    FOR_N(i, 3, n->input_count) {
                        c_fmt_spaces(ctx);
                        nl_buffer_format(ctx->buf, "  ret.v%zu = ", index);
                        c_fmt_ref_to_node(ctx, n->inputs[i]);
                        nl_buffer_format(ctx->buf, ";\n");
                        index += 1;
                    }
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "  return ret;\n");
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "}\n");
                }
                break;
            }

            case TB_NATURAL_LOOP:
            case TB_AFFINE_LOOP:
            case TB_CALLGRAPH: {
                break;
            }

            case TB_STORE: {
                TB_Node *dest = n->inputs[n->input_count-2];
                TB_Node *src = n->inputs[n->input_count-1];
                c_fmt_spaces(ctx);
                nl_buffer_format(ctx->buf, "*(%s*) ", c_fmt_type_name(src->dt));
                c_fmt_ref_to_node(ctx, dest);
                nl_buffer_format(ctx->buf, " = ");
                c_fmt_ref_to_node(ctx, src);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_NEG: {
                TB_Node *src = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "-");
                c_fmt_ref_to_node(ctx, src);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_LOAD: {
                TB_Node *src = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "*(%s*) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, src);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_LOCAL: {
                if (!nl_hashset_put(&ctx->declared_vars, n)) {
                    TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
                    nl_buffer_format(ctx->pre, "  uint8_t v%u[0x%x];\n", n->gvn, l->size);
                }
                break;
            }

            case TB_BITCAST: {
                TB_Node *src = n->inputs[n->input_count-1];
                if (TB_IS_FLOAT_TYPE(src->dt) && TB_IS_FLOAT_TYPE(n->dt)) {
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "{\n");
                    ctx->depth += 1;
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "union {%s src; %s dest;} tmp;\n", c_fmt_type_name(src->dt), c_fmt_type_name(n->dt));
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "tmp.src = ");
                    c_fmt_ref_to_node(ctx, src);
                    nl_buffer_format(ctx->buf, ";\n");
                    c_fmt_output(ctx, n);
                    nl_buffer_format(ctx->buf, "tmp.dest;\n");
                    ctx->depth -= 1;
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "}\n");
                } else {
                    c_fmt_output(ctx, n);
                    nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                    c_fmt_ref_to_node(ctx, src);
                    nl_buffer_format(ctx->buf, ";\n");
                }
                break;
            }

            case TB_OR: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " | ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_XOR: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " ^ ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_ROL: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "((%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " << ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ") | ((%s)", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " >> (%d - ", n->dt.data);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, "));\n");
                break;
            }
            case TB_SHR: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " >> ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_SAR: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " >> ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_SHL: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " << ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_AND: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " & ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_FADD:
            case TB_ADD: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " + ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_FSUB:
            case TB_SUB: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " - ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_FMUL:
            case TB_MUL: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " * ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_FDIV: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " / ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_SDIV: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(n->dt));
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " / ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(n->dt));
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_UDIV: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " / ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_SMOD: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(n->dt));
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " %% ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(n->dt));
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }
            case TB_UMOD: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " %% ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_CMP_EQ: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " == ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_CMP_NE: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " != ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_POISON: {
                break;
            }

            case TB_CMP_FLT: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " < ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_CMP_FLE: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " <= ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }


            case TB_CMP_SLT: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " < ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_CMP_SLE: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " <= ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name_signed(lhs->dt));
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }


            case TB_CMP_ULT: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_typed_ref_to_node(ctx, lhs->dt, lhs);
                nl_buffer_format(ctx->buf, " < ");
                c_fmt_typed_ref_to_node(ctx, rhs->dt, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_CMP_ULE: {
                TB_Node *lhs = n->inputs[n->input_count-2];
                TB_Node *rhs = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_ref_to_node(ctx, lhs);
                nl_buffer_format(ctx->buf, " <= ");
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(lhs->dt));
                c_fmt_ref_to_node(ctx, rhs);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_PTR_OFFSET: {
                TB_Node *off = n->inputs[n->input_count-2];
                TB_Node *ptr = n->inputs[n->input_count-1];

                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(void*) ((char *) ");
                c_fmt_ref_to_node(ctx, ptr);
                nl_buffer_format(ctx->buf, " + ");
                c_fmt_ref_to_node(ctx, off);
                nl_buffer_format(ctx->buf, ");\n");
                break;
            }


            case TB_SELECT: {
                TB_Node *cond = n->inputs[n->input_count-3];
                TB_Node *then = n->inputs[n->input_count-2];
                TB_Node *els = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                c_fmt_ref_to_node(ctx, cond);
                nl_buffer_format(ctx->buf, " ? ");
                c_fmt_ref_to_node(ctx, then);
                nl_buffer_format(ctx->buf, " : ");
                c_fmt_ref_to_node(ctx, els);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_FLOAT2UINT:
            case TB_FLOAT2INT:
            case TB_UINT2FLOAT:
            case TB_INT2FLOAT:
            case TB_TRUNCATE: {
                TB_Node *input = n->inputs[n->input_count-1];
                c_fmt_output(ctx, n);
                nl_buffer_format(ctx->buf, "(%s) ", c_fmt_type_name(n->dt));
                c_fmt_ref_to_node(ctx, input);
                nl_buffer_format(ctx->buf, ";\n");
                break;
            }

            case TB_CALL: {
                TB_Node *func = n->inputs[2];

                TB_Node* projs[4] = { 0 };
                FOR_USERS(use, n) {
                    if (USERN(use)->type == TB_PROJ) {
                        int index = TB_NODE_GET_EXTRA_T(USERN(use), TB_NodeProj)->index;
                        projs[index] = USERN(use);
                    }
                }

                if (projs[2] == NULL) {
                    nl_buffer_format(ctx->globals, "typedef void(*tb2c_%s_v%u_t)(", ctx->name, n->gvn);
                } else if (projs[3] == NULL) {
                    nl_buffer_format(ctx->globals, "typedef %s(*tb2c_%s_v%u_t)(", c_fmt_type_name(projs[2]->dt), ctx->name, n->gvn);
                } else {
                    nl_buffer_format(ctx->globals, "typedef struct {\n");
                    FOR_N(i, 2, 4) {
                        if (projs[i] == NULL) break;
                        nl_buffer_format(ctx->globals, "  %s v%u;\n", c_fmt_type_name(projs[i]->dt), projs[i]->gvn);
                    }
                    nl_buffer_format(ctx->globals, "} tb2c_%s_v%u_ret_t;\n", ctx->name, n->gvn);
                    nl_buffer_format(ctx->globals, "typedef tb2c_%s_v%u_ret_t(*tb2c_%s_v%u_t)(", ctx->name, n->gvn, ctx->name, n->gvn);
                }
                {
                    bool first = true;
                    FOR_N(i, 3, n->input_count) {
                        if (n->inputs[i]->dt.type != TB_TAG_CONTROL && n->inputs[i]->dt.type != TB_TAG_MEMORY) {
                            if (!first) {
                                nl_buffer_format(ctx->globals, ", ");
                            }
                            nl_buffer_format(ctx->globals, "%s", c_fmt_type_name(n->inputs[i]->dt));
                            first = false;
                        }
                    }
                    if (first) {
                        nl_buffer_format(ctx->globals, "void");
                    }
                }
                nl_buffer_format(ctx->globals, ");\n");
                if (projs[2] == NULL || projs[3] == NULL) {
                    if (projs[2] != NULL) {
                        c_fmt_output(ctx, projs[2]);
                    } else {
                        c_fmt_spaces(ctx);
                    }
                    nl_buffer_format(ctx->buf, "((tb2c_%s_v%u_t) ", ctx->name, n->gvn);
                    c_fmt_ref_to_node(ctx, func);
                    nl_buffer_format(ctx->buf, ")");
                    nl_buffer_format(ctx->buf, "(");
                    {
                        bool first = true;
                        FOR_N(i, 3, n->input_count) {
                            if (n->inputs[i]->dt.type != TB_TAG_CONTROL && n->inputs[i]->dt.type != TB_TAG_MEMORY) {
                                if (!first) {
                                    nl_buffer_format(ctx->buf, ", ");
                                }
                                c_fmt_ref_to_node(ctx, n->inputs[i]);
                                first = false;
                            }
                        }
                    }
                    nl_buffer_format(ctx->buf, ");\n");
                } else {
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "{\n");
                    ctx->depth += 1;
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "tb2c_%s_v%u_ret_t ret = ", ctx->name, n->gvn);
                    nl_buffer_format(ctx->buf, "((tb2c_%s_v%u_t) ", ctx->name, n->gvn);
                    c_fmt_ref_to_node(ctx, func);
                    nl_buffer_format(ctx->buf, ")(");
                    {
                        bool first = true;
                        FOR_N(i, 3, n->input_count) {
                            if (n->inputs[i]->dt.type != TB_TAG_CONTROL && n->inputs[i]->dt.type != TB_TAG_MEMORY) {
                                if (!first) {
                                    nl_buffer_format(ctx->buf, ", ");
                                }
                                c_fmt_ref_to_node(ctx, n->inputs[i]);
                                first = false;
                            }
                        }
                    }
                    nl_buffer_format(ctx->buf, ");\n");
                    if (projs[2] != NULL) {
                        FOR_N(i, 2, 4) {
                            if (projs[i] == NULL) break;
                            c_fmt_output(ctx, projs[i]);
                            nl_buffer_format(ctx->buf, "ret.v%u;\n", projs[i]->gvn);
                        }
                    }
                    ctx->depth -= 1;
                    c_fmt_spaces(ctx);
                    nl_buffer_format(ctx->buf, "}\n");
                }
                break;
            }

            case TB_MEMCPY: {
                TB_Node *dest = n->inputs[n->input_count-3];
                TB_Node *src = n->inputs[n->input_count-2];
                TB_Node *len = n->inputs[n->input_count-1];
                c_fmt_spaces(ctx);
                nl_buffer_format(ctx->buf, "memcpy(");
                c_fmt_ref_to_node(ctx, dest);
                nl_buffer_format(ctx->buf, ", ");
                c_fmt_ref_to_node(ctx, src);
                nl_buffer_format(ctx->buf, ", ");
                c_fmt_ref_to_node(ctx, len);
                nl_buffer_format(ctx->buf, ");\n");
                break;
            }

            case TB_MEMSET: {
                TB_Node *a = n->inputs[n->input_count-3];
                TB_Node *b = n->inputs[n->input_count-2];
                TB_Node *c = n->inputs[n->input_count-1];
                c_fmt_spaces(ctx);
                nl_buffer_format(ctx->buf, "memset(");
                c_fmt_ref_to_node(ctx, a);
                nl_buffer_format(ctx->buf, ", ");
                c_fmt_ref_to_node(ctx, b);
                nl_buffer_format(ctx->buf, ", ");
                c_fmt_ref_to_node(ctx, c);
                nl_buffer_format(ctx->buf, ");\n");
                break;
            }

            default: {
                fprintf(stderr, "internal unimplemented node type: %s\n", tb_node_get_name(n));
                fflush(stderr);
                __builtin_trap();
                break;
            }
        }
    }

    // dyn_array_set_length(ws->items, ctx->cfg.block_count);

    if (!cfg_is_terminator(bb->end)) {
        c_fmt_branch_edge(ctx, bb->end, true);
    }

    // nl_buffer_t *buf = ctx->buf;
    // ctx->buf = old_buf;
    // nl_buffer_format(ctx->buf, "%s", nl_buffer_get(buf));
}

TB_API char *tb_c_prelude(TB_Module *mod) {
    size_t n_private_syms = 0;

    void *arena = tb_arena_create(TB_ARENA_MEDIUM_CHUNK_SIZE);

    nl_buffer_t *buf = nl_buffer_new();

    nl_buffer_format(buf, "typedef signed char int8_t;\n");
    nl_buffer_format(buf, "typedef unsigned char uint8_t;\n");
    nl_buffer_format(buf, "typedef signed short int16_t;\n");
    nl_buffer_format(buf, "typedef unsigned short uint16_t;\n");
    nl_buffer_format(buf, "typedef signed int int32_t;\n");
    nl_buffer_format(buf, "typedef unsigned int uint32_t;\n");
    if (sizeof(long) == 8) {
        nl_buffer_format(buf, "typedef signed long int64_t;\n");
        nl_buffer_format(buf, "typedef unsigned long uint64_t;\n");
    } else {
        nl_buffer_format(buf, "typedef signed long long int64_t;\n");
        nl_buffer_format(buf, "typedef unsigned long long uint64_t;\n");
    }
    if (sizeof(size_t) == sizeof(uint64_t)) {
        nl_buffer_format(buf, "typedef uint64_t size_t;\n");
    }
    if (sizeof(size_t) == sizeof(uint32_t)) {
        nl_buffer_format(buf, "typedef uint32_t size_t;\n");
    }
    nl_buffer_format(buf, "void *memcpy(void *dest, const void *src, size_t n);\n");
    nl_buffer_format(buf, "void *memset(void *str, int c, size_t n);\n");
    nl_buffer_format(buf, "\n");
    NL_HashSet* syms = &tb_thread_info(mod)->symbols;
    nl_hashset_for(sym_vp, syms) {
        TB_Symbol *sym = *sym_vp;
        if (sym->name == NULL ||sym->name[0] == '\0') {
            continue;
        }
        switch ((int) sym->tag) {
            case TB_SYMBOL_EXTERNAL: {
                if (mod->is_jit) {
                    nl_buffer_format(buf, "static void *%s = (void *) 0x%zx;\n", sym->name, (size_t) sym->address);
                } else {
                    nl_buffer_format(buf, "extern void %s(void);\n", sym->name);
                }
                break;
            }
            case TB_SYMBOL_GLOBAL: {
                TB_Global *g = (void *) sym;
                if (g->linkage == TB_LINKAGE_PRIVATE) {
                    nl_buffer_format(buf, "static ");
                    sym->symbol_id = ++n_private_syms;
                } else {
                    sym->symbol_id = 0;
                }
                if (g->obj_count == 0) {
                    nl_buffer_format(buf, "uint8_t tb2c_sym%zu_%s[%zu];\n", (size_t) sym->symbol_id, sym->name, (size_t) g->size);
                } else {
                    uint8_t *data_buf = tb_platform_heap_alloc(sizeof(uint8_t) * g->size);
                    memset(data_buf, 0, sizeof(uint8_t) * g->size);
                    FOR_N(k, 0, g->obj_count) {
                        if (g->objects[k].type == TB_INIT_OBJ_REGION) {
                            assert(g->objects[k].offset + g->objects[k].region.size <= g->size);
                            memcpy(&data_buf[g->pos + g->objects[k].offset], g->objects[k].region.ptr, g->objects[k].region.size);
                        }
                    }
                    nl_buffer_format(buf, "uint8_t tb2c_sym%zu_%s[%zu] = {\n", (size_t) sym->symbol_id, sym->name, (size_t) g->size);
                    for (size_t i = 0; i < g->size; i++) {
                        nl_buffer_format(buf, "  0x%02x,\n", data_buf[i]);
                    }
                    nl_buffer_format(buf, "};\n");
                }
                // nl_buffer_format(buf, "// global: %s\n", sym->name);
                break;
            }
            case TB_SYMBOL_FUNCTION: {
                TB_Function *f = (void *) sym;
                TB_FunctionPrototype* p = f->prototype;
                if (p->return_count == 0) {
                    nl_buffer_format(buf, "typedef void tb2c_%s_ret_t;\n", sym->name);
                } else if (p->return_count == 1) {
                    if (p->params[p->param_count].dt.type == TB_TAG_INT && p->params[p->param_count].dt.data == 32 && !strcmp(sym->name, "main")) {
                        nl_buffer_format(buf, "#define tb2c_%s_ret_t int\n", sym->name);
                        nl_buffer_format(buf, "#define main(...) main(int v4, char **v5)\n");
                    } else {
                        nl_buffer_format(buf, "typedef %s tb2c_%s_ret_t;\n", c_fmt_type_name(p->params[p->param_count].dt), sym->name);
                    }
                } else {
                    nl_buffer_format(buf, "typedef struct {\n");

                    size_t index = 0;
                    FOR_N(i, 0, p->return_count) {
                        nl_buffer_format(buf, "  %s v%zu;\n", c_fmt_type_name(p->params[p->param_count + i].dt), index);
                        index += 1;
                    }
                    nl_buffer_format(buf, "} tb2c_%s_ret_t;\n", sym->name);
                }
                if (f->linkage == TB_LINKAGE_PRIVATE) {
                    nl_buffer_format(buf, "static ");
                }
                nl_buffer_format(buf, "tb2c_%s_ret_t %s(", sym->name, sym->name);
                TB_Node** params = f->params;
                size_t count = 0;
                FOR_N(i, 3, 3 + f->param_count) {
                    if (params[i] != NULL && params[i]->dt.type != TB_TAG_MEMORY && params[i]->dt.type != TB_TAG_CONTROL && params[i]->dt.type != TB_TAG_TUPLE) {
                        if (count != 0) {
                            nl_buffer_format(buf, ", ");
                        }
                        nl_buffer_format(buf, "%s v%u", c_fmt_type_name(params[i]->dt), params[i]->gvn);
                        count += 1;
                    }
                }
                if (count == 0) {
                    nl_buffer_format(buf, "void");
                }
                nl_buffer_format(buf, ");\n");
                break;
            }
        }
    }

    char *ret = nl_buffer_get(buf);

    // tb_arena_destroy(arena);

    return ret;
}

TB_API char* tb_print_c(TB_Function* f, TB_Worklist* ws, TB_Arena* tmp) {
    const char *name = f->super.name;
    cuikperf_region_start("print_c", NULL);

    f->tmp_arena = tmp;
    f->worklist  = ws;

    CFmtState ctx = { name, f, f->super.module };
    ctx.globals = nl_buffer_new();
    ctx.buf = nl_buffer_new();
    ctx.pre = nl_buffer_new();

    // ctx.global_funcs = nl_hashset_alloc(ctx.module->compiled_function_count);
    ctx.declared_types = nl_hashset_alloc(4);
    ctx.visited_blocks = dyn_array_create(size_t, 8);
    ctx.declared_vars = nl_hashset_alloc(16);
    ctx.block_ranges = nl_table_alloc(ctx.cfg.block_count);

    // nl_hashset_clear(&ctx.visited_blocks);
    ctx.cfg = tb_compute_rpo(f, ws);

    // schedule nodes
    tb_global_schedule(f, ws, ctx.cfg, false, false, NULL);

    // TB_Node* end_bb = NULL;
    FOR_N(i, 0, ctx.cfg.block_count) {
        ctx.depth += 1;
        c_fmt_bb(&ctx, ws, ws->items[i]);
        ctx.depth -= 1;
    }
    tb_free_cfg(&ctx.cfg);

    f->worklist = NULL;
    f->scheduled = NULL;
    cuikperf_region_end();

    nl_buffer_t *buf = nl_buffer_new();

    nl_buffer_format(buf, "%s\n", nl_buffer_get(ctx.globals));
    nl_buffer_format(buf, "tb2c_%s_ret_t %s(", name, name);
    TB_Node** params = f->params;
    size_t count = 0;
    FOR_N(i, 3, 3 + f->param_count) {
        if (params[i] != NULL && params[i]->dt.type != TB_TAG_MEMORY && params[i]->dt.type != TB_TAG_CONTROL && params[i]->dt.type != TB_TAG_TUPLE) {
            if (count != 0) {
                nl_buffer_format(buf, ", ");
            }
            nl_buffer_format(buf, "%s v%u", c_fmt_type_name(params[i]->dt), params[i]->gvn);
            count += 1;
        }
    }
    if (count == 0) {
        nl_buffer_format(buf, "void");
    }
    nl_buffer_format(buf, ") {\n");
    nl_buffer_format(buf, "%s", nl_buffer_get(ctx.pre));
    nl_buffer_format(buf, "%s", nl_buffer_get(ctx.buf));
    nl_buffer_format(buf, "}\n");

    dyn_array_destroy(ctx.visited_blocks);

    char *ret = nl_buffer_get(buf);

    // tb_arena_destroy(ctx.arena);

    return ret;
}
