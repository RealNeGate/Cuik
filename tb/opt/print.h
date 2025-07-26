
typedef struct {
    TB_Function* f;
    TB_CFG cfg;
} PrinterCtx;

static int cool_ansi_color(uint32_t x) {
    int y = x % 14;
    if (y > 7) { return 90 + (y - 7); }
    else { return 32 + y; }
}

const char* tb_node_get_name(TB_NodeTypeEnum n_type) {
    switch (n_type) {
        case TB_NULL: return "NULL";
        case TB_UNREACHABLE: return "unreachable";

        case TB_BRANCH_PROJ: return "br_proj";
        case TB_MACH_JIT_THREAD_PTR: return "jit_thread_ptr";

        case TB_ROOT:   return "root";
        case TB_RETURN: return "return";
        case TB_PROJ:   return "proj";
        case TB_REGION: return "region";
        case TB_NATURAL_LOOP: return "loop";
        case TB_AFFINE_LOOP: return "loop.affine";
        case TB_CALLGRAPH: return "callgraph";
        case TB_SYMBOL_TABLE: return "symbol_table";
        case TB_FRAME_PTR: return "frame_ptr";

        case TB_LOCAL: return "local";
        case TB_ENTRY_FORK: return "entry_fork";

        case TB_VA_START: return "vastart";
        case TB_DEBUGBREAK: return "dbgbrk";
        case TB_BLACKHOLE: return "blackhole";

        case TB_POISON: return "poison";
        case TB_DEAD: return "dead";
        case TB_TRAP: return "trap";
        case TB_ICONST: return "int";
        case TB_F32CONST: return "float32";
        case TB_F64CONST: return "float64";

        case TB_PHI: return "phi";
        case TB_SELECT: return "select";

        case TB_PTR_OFFSET: return "ptr_offset";

        case TB_CYCLE_COUNTER: return "cyclecnt";
        case TB_PREFETCH: return "prefetch";
        case TB_INLINE_ASM: return "inlineasm";
        case TB_DEBUG_LOCATION: return "dbgloc";
        case TB_DEBUG_SCOPES: return "dbgscopes";
        case TB_SAFEPOINT: return "safepoint";

        case TB_MEMSET: return "memset";
        case TB_MEMCPY: return "memcpy";
        case TB_SPLITMEM: return "split";
        case TB_MERGEMEM: return "merge";

        case TB_BSWAP: return "bswap";
        case TB_POPCNT: return "popcnt";
        case TB_ZERO_EXT: return "zxt";
        case TB_SIGN_EXT: return "sxt";
        case TB_FLOAT_TRUNC: return "fptrunc";
        case TB_FLOAT_EXT: return "fpxt";
        case TB_TRUNCATE: return "trunc";
        case TB_BITCAST: return "bitcast";
        case TB_UINT2FLOAT: return "uint2float";
        case TB_INT2FLOAT: return "int2float";
        case TB_FLOAT2UINT: return "float2uint";
        case TB_FLOAT2INT: return "float2int";
        case TB_SYMBOL: return "symbol";

        case TB_CMP_NE: return "cmp.ne";
        case TB_CMP_EQ: return "cmp.eq";
        case TB_CMP_ULT: return "cmp.ult";
        case TB_CMP_ULE: return "cmp.ule";
        case TB_CMP_SLT: return "cmp.slt";
        case TB_CMP_SLE: return "cmp.sle";
        case TB_CMP_FLT: return "cmp.lt";
        case TB_CMP_FLE: return "cmp.le";

        case TB_ATOMIC_LOAD: return "atomic.load";
        case TB_ATOMIC_XCHG: return "atomic.xchg";
        case TB_ATOMIC_ADD: return "atomic.add";
        case TB_ATOMIC_AND: return "atomic.and";
        case TB_ATOMIC_XOR: return "atomic.xor";
        case TB_ATOMIC_OR: return "atomic.or";
        case TB_ATOMIC_PTROFF: return "atomic.ptroff";
        case TB_ATOMIC_CAS: return "atomic.cas";

        case TB_CLZ: return "clz";
        case TB_CTZ: return "ctz";
        case TB_AND: return "and";
        case TB_OR: return "or";
        case TB_XOR: return "xor";
        case TB_ADD: return "add";
        case TB_SUB: return "sub";
        case TB_MUL: return "mul";
        case TB_UDIV: return "udiv";
        case TB_SDIV: return "sdiv";
        case TB_UMOD: return "umod";
        case TB_SMOD: return "smod";
        case TB_SHL: return "shl";
        case TB_SHR: return "shr";
        case TB_ROL: return "rol";
        case TB_ROR: return "ror";
        case TB_SAR: return "sar";

        case TB_FADD: return "fadd";
        case TB_FSUB: return "fsub";
        case TB_FMUL: return "fmul";
        case TB_FDIV: return "fdiv";
        case TB_FMAX: return "fmax";
        case TB_FMIN: return "fmin";
        case TB_FNEG: return "fneg";

        case TB_UMULPAIR: return "umulpair";
        case TB_SMULPAIR: return "smulpair";
        case TB_LOAD:     return "load";
        case TB_STORE:    return "store";
        case TB_DEAD_STORE: return "dead_store";
        case TB_VSHUFFLE: return "vshuffle";
        case TB_VBROADCAST: return "vbroadcast";
        case TB_HARD_BARRIER: return "hard_barrier";

        case TB_CALL:     return "call";
        case TB_SYSCALL:  return "syscall";
        case TB_BRANCH:   return "branch";
        case TB_AFFINE_LATCH: return "affine_latch";
        case TB_NEVER_BRANCH: return "never_branch";
        case TB_TAILCALL: return "tailcall";

        case TB_MACH_TEMP: return "mach_temp";
        case TB_MACH_JUMP: return "mach_jump";
        case TB_MACH_COPY: return "mach_copy";
        case TB_MACH_PROJ: return "mach_proj";
        case TB_MACH_SYMBOL: return "mach_symbol";
        case TB_MACH_FRAME_PTR: return "mach_frameptr";

        case TB_X86INTRIN_LDMXCSR: return "x86_ldmxcsr";
        case TB_X86INTRIN_STMXCSR: return "x86_stmxcsr";
        case TB_X86INTRIN_SQRT:  return "x86_sqrt";
        case TB_X86INTRIN_RSQRT: return "x86_rsqrt";

        default: {
            int family = n_type / 0x100;
            TB_ASSERT(family >= 1 && family < TB_ARCH_MAX);
            return tb_codegen_families[family].node_name(n_type);
        }
    }
}

static const char* plain_type_name(TB_DataType dt) {
    switch (dt.type) {
        case TB_TAG_VOID:    return "void";
        case TB_TAG_BOOL:    return "bool";
        case TB_TAG_I8:      return "i8";
        case TB_TAG_I16:     return "i16";
        case TB_TAG_I32:     return "i32";
        case TB_TAG_I64:     return "i64";
        case TB_TAG_PTR:     return dt.elem_or_addrspace == 0 ? "ptr" : NULL;
        case TB_TAG_F32:     return "f32";
        case TB_TAG_F64:     return "f64";
        case TB_TAG_TUPLE:   return "tuple";
        case TB_TAG_CONTROL: return "ctrl";
        case TB_TAG_MEMORY:  return  "mem";

        case TB_TAG_V64:
        case TB_TAG_V128:
        case TB_TAG_V256:
        case TB_TAG_V512:
        return NULL; // not plain types

        default: return NULL;
    }
}

static int print_type(OutStream* s, TB_DataType dt) {
    const char* plain = plain_type_name(dt);
    if (plain) {
        return s_writef(s, plain);
    } else if (TB_IS_VECTOR_TYPE(dt)) {
        const char* elem_plain = plain_type_name((TB_DataType){ .type = dt.elem_or_addrspace });
        return s_writef(s, "v%d[%s]", 1u << ((dt.type - TB_TAG_V64) + 6), elem_plain);
    } else if (dt.type == TB_TAG_PTR && dt.elem_or_addrspace != 0) {
        return s_writef(s, "ptr%d", dt.elem_or_addrspace);
    } else {
        tb_todo();
    }
}

static void print_ref_to_node(PrinterCtx* ctx, TB_Node* n, OutStream* s, bool def) {
    if (n == NULL) {
        s_writef(s, "_");
    } else if (n->type == TB_ROOT) {
        s_writef(s, "%s", ctx->f->super.name);

        if (def) {
            s_writef(s, "(");
            TB_Node** params = ctx->f->params;
            FOR_N(i, 1, 3 + ctx->f->param_count) {
                if (i > 1) s_writef(s, ", ");

                if (params[i] == NULL) {
                    s_writef(s, "_");
                } else {
                    s_color(s, cool_ansi_color(params[i]->gvn));
                    s_writef(s, "%%%u: ", params[i]->gvn);
                    s_color(s, 0);
                    print_type(s, params[i]->dt);
                }
            }

            FOR_USERS(u, n) if (USERN(u)->type == TB_MACH_PROJ) {
                s_writef(s, ", ");
                s_color(s, cool_ansi_color(USERN(u)->gvn));
                s_writef(s, "%%%u: ", USERN(u)->gvn);
                s_color(s, 0);
                print_type(s, USERN(u)->dt);
            }
            s_writef(s, ")");
        }
    } else if (cfg_is_region(n)) {
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
        s_color(s, cool_ansi_color(n->gvn));
        if (r->tag != NULL) {
            s_writef(s, "%%%u.%s", n->gvn, r->tag);
        } else {
            s_writef(s, "%%%u", n->gvn);
        }
        s_color(s, 0);

        if (def) {
            bool first = true;
            s_writef(s, "(");
            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_PHI) {
                    if (first) {
                        first = false;
                    } else {
                        s_writef(s, ", ");
                    }

                    s_color(s, cool_ansi_color(USERN(u)->gvn));
                    s_writef(s, "%%%u: ", USERN(u)->gvn);
                    s_color(s, 0);
                    print_type(s, USERN(u)->dt);
                }
            }
            s_writef(s, ")");
            if (n->type == TB_NATURAL_LOOP) s_writef(s, " !natural");
            if (n->type == TB_AFFINE_LOOP) s_writef(s, " !affine");
        }
    } else if (n->type == TB_F32CONST) {
        TB_NodeFloat32* f = TB_NODE_GET_EXTRA(n);
        s_writef(s, "%f", f->value);
    } else if (n->type == TB_F64CONST) {
        TB_NodeFloat64* f = TB_NODE_GET_EXTRA(n);
        s_writef(s, "%f", f->value);
    } else if (n->type == TB_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
        if (sym->name[0]) {
            s_writef(s, "%s", sym->name);
        } else {
            s_writef(s, "sym%p", sym);
        }
    } else if (cfg_is_cproj(n)) {
        if (n->inputs[0]->type == TB_ROOT) {
            print_ref_to_node(ctx, n->inputs[0], s, def);
        } else {
            s_color(s, cool_ansi_color(n->gvn));
            s_writef(s, "%%%u", n->gvn);
            s_color(s, 0);

            if (def) { s_writef(s, "()"); }
        }
    } else if (n->type == TB_ICONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

        if (num->value < 0xFFFF) {
            int bits = tb_data_type_bit_size(ctx->f->super.module, n->dt.type);
            s_writef(s, "%"PRId64, tb__sxt(num->value, bits, 64));
        } else {
            s_writef(s, "%#0"PRIx64, num->value);
        }
    } else {
        s_color(s, cool_ansi_color(n->gvn));
        s_writef(s, "%%%u", n->gvn);
        s_color(s, 0);
    }
}

static bool is_empty_cproj(PrinterCtx* ctx, TB_Node* n) {
    if (cfg_is_cproj(n)) {
        TB_BasicBlock* bb = ctx->f->scheduled[n->gvn];
        return aarray_length(bb->items) == 1;
    }

    return false;
}

// deals with printing BB params
static void print_branch_edge(PrinterCtx* ctx, TB_Node* n, OutStream* s, bool fallthru) {
    TB_Node* target = fallthru || is_empty_cproj(ctx, n) ? cfg_next_control(n) : n;
    print_ref_to_node(ctx, target, s, false);

    // print phi args
    s_writef(s, "(");
    if (cfg_is_region(target)) {
        int phi_i = -1;
        FOR_USERS(u, n) {
            if (cfg_is_region(USERN(u))) {
                phi_i = 1 + USERI(u);
                break;
            }
        }

        bool first = true;
        FOR_USERS(u, target) {
            if (USERN(u)->type == TB_PHI) {
                if (first) {
                    first = false;
                } else {
                    s_writef(s, ", ");
                }

                TB_ASSERT(phi_i >= 0);
                print_ref_to_node(ctx, USERN(u)->inputs[phi_i], s, false);
            }
        }
    }
    s_writef(s, ")");
}

static int node_latency(TB_Function* f, TB_Node* n, TB_Node* end) { return 1; }
static void print_bb(PrinterCtx* ctx, TB_Worklist* ws, TB_BasicBlock* bb, OutStream* s) {
    print_ref_to_node(ctx, bb->start, s, true);
    s_newline(s);

    TB_Function* f = ctx->f;
    // tb_greedy_scheduler(f, &ctx->cfg, ws, bb);
    tb_list_scheduler(f, &ctx->cfg, ws, bb, node_latency);

    FOR_N(i, 0, dyn_array_length(ws->items)) {
        TB_Node* n = ws->items[i];

        // skip these
        if (n->type == TB_ICONST || n->type == TB_F32CONST ||
            n->type == TB_F64CONST || n->type == TB_SYMBOL ||
            n->type == TB_PROJ || n->type == TB_BRANCH_PROJ || n->type == TB_MACH_PROJ ||
            n->type == TB_REGION || n->type == TB_NATURAL_LOOP || n->type == TB_AFFINE_LOOP ||
            n->type == TB_NULL || n->type == TB_PHI) {
            continue;
        }

        switch (n->type) {
            case TB_DEBUGBREAK:  s_writef(s, "  debugbreak"); break;
            case TB_UNREACHABLE: s_writef(s, "  unreachable"); break;

            case TB_NEVER_BRANCH: {
                TB_Node* taken = USERN(proj_with_index(n, 0));
                TB_Node* never = USERN(proj_with_index(n, 1));

                s_writef(s, "  goto ");
                print_branch_edge(ctx, taken, s, false);
                s_writef(s, " // never branch ");
                print_branch_edge(ctx, never, s, false);
                break;
            }

            case TB_AFFINE_LATCH:
            case TB_BRANCH: {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
                TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
                TB_Node** restrict succ = tb_arena_alloc(&f->tmp_arena, br->succ_count * sizeof(TB_Node*));

                // fill successors
                FOR_USERS(u, n) {
                    if (USERN(u)->type == TB_BRANCH_PROJ) {
                        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                        succ[index] = USERN(u);
                    }
                }

                TB_ASSERT(br->succ_count >= 2);
                if (br->succ_count == 2) {
                    int bits = tb_data_type_bit_size(f->super.module, n->inputs[1]->dt.type);

                    s_writef(s, "  if ");
                    print_ref_to_node(ctx, n->inputs[1], s, false);
                    int64_t key = TB_NODE_GET_EXTRA_T(succ[1], TB_NodeBranchProj)->key;
                    if (key == 0) {
                        s_writef(s, " then ");
                    } else {
                        s_writef(s, " != %"PRId64" then ", key);
                    }
                    print_branch_edge(ctx, succ[0], s, false);
                    s_writef(s, " else ");
                    print_branch_edge(ctx, succ[1], s, false);
                } else {
                    s_writef(s, "  br ");
                    print_ref_to_node(ctx, n->inputs[1], s, false);
                    s_writef(s, "%s=> {", n->input_count > 1 ? " " : "");
                    s_newline(s);

                    FOR_N(i, 0, br->succ_count) {
                        int64_t key = TB_NODE_GET_EXTRA_T(succ[i], TB_NodeBranchProj)->key;

                        if (i != 0) s_writef(s, "    %"PRId64": ", key);
                        else s_writef(s, "    default: ");

                        print_branch_edge(ctx, succ[i], s, false);
                        s_newline(s);
                    }
                    s_writef(s, "  }");
                }
                tb_arena_restore(&f->tmp_arena, sp);
                break;
            }

            case TB_TRAP: {
                s_writef(s, "  trap");
                break;
            }

            case TB_RETURN: {
                s_writef(s, "  return ");
                FOR_N(i, 1, n->input_count) {
                    if (i != 1) s_writef(s, ", ");
                    print_ref_to_node(ctx, n->inputs[i], s, false);
                }
                break;
            }

            default: {
                if (cfg_is_branch(n)) {
                    s_writef(s, "  %s ", tb_node_get_name(n->type));
                } else if (n->dt.type == TB_TAG_TUPLE) {
                    // print with multiple returns
                    TB_Node* projs[32] = { 0 };
                    FOR_USERS(u, n) {
                        if (USERN(u)->type == TB_PROJ) {
                            int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                            projs[index] = USERN(u);
                        }
                    }

                    s_writef(s, "  ");

                    size_t first = projs[0] && projs[0]->dt.type == TB_TAG_CONTROL ? 1 : 0;
                    FOR_N(i, first, 32) {
                        if (projs[i] == NULL) break;
                        if (i > first) s_writef(s, ", ");
                        s_color(s, cool_ansi_color(projs[i]->gvn));
                        s_writef(s, "%%%u", projs[i]->gvn);
                        s_color(s, 0);
                    }
                    s_writef(s, " = %s.(", tb_node_get_name(n->type));
                    FOR_N(i, first, 32) {
                        if (projs[i] == NULL) break;
                        if (i > first) s_writef(s, ", ");
                        print_type(s, projs[i]->dt);
                    }
                    s_writef(s, ")");
                } else {
                    // print as normal instruction
                    s_writef(s, "  ");
                    s_color(s, cool_ansi_color(n->gvn));
                    s_writef(s, "%%%u", n->gvn);
                    s_color(s, 0);
                    s_writef(s, " = %s.", tb_node_get_name(n->type));

                    TB_DataType dt = n->dt;
                    if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
                        dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                    } else if (n->type == TB_STORE) {
                        dt = n->inputs[3]->dt;
                    }
                    print_type(s, dt);
                }
                s_writef(s, " ");

                size_t first = n->type == TB_PROJ ? 0 : 1;
                FOR_N(i, first, n->input_count) {
                    if (i != first) s_writef(s, ", ");
                    print_ref_to_node(ctx, n->inputs[i], s, false);
                }

                // print extra data
                switch (n->type) {
                    case TB_CMP_EQ:
                    case TB_CMP_NE:
                    case TB_CMP_ULT:
                    case TB_CMP_ULE:
                    case TB_CMP_SLT:
                    case TB_CMP_SLE:
                    case TB_CMP_FLT:
                    case TB_CMP_FLE:
                    case TB_SELECT:
                    case TB_BITCAST:
                    break;

                    case TB_PROJ: {
                        s_writef(s, ", %d", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
                        break;
                    }

                    case TB_AND:
                    case TB_OR:
                    case TB_XOR:
                    case TB_ADD:
                    case TB_SUB:
                    case TB_MUL:
                    case TB_SHL:
                    case TB_SHR:
                    case TB_ROL:
                    case TB_ROR:
                    case TB_SAR:
                    case TB_UDIV:
                    case TB_SDIV:
                    case TB_UMOD:
                    case TB_SMOD:
                    {
                        TB_NodeBinopInt* b = TB_NODE_GET_EXTRA(n);
                        if (b->ab & TB_ARITHMATIC_NSW) s_writef(s, " !nsw");
                        if (b->ab & TB_ARITHMATIC_NUW) s_writef(s, " !nuw");
                        break;
                    }

                    case TB_LOAD:
                    case TB_STORE:
                    case TB_MEMSET:
                    case TB_MEMCPY:
                    case TB_DEAD_STORE: {
                        TB_NodeMemAccess* mem = TB_NODE_GET_EXTRA(n);
                        s_writef(s, " !align(%d)", mem->align);
                        break;
                    }

                    case TB_ATOMIC_LOAD:
                    case TB_ATOMIC_XCHG:
                    case TB_ATOMIC_ADD:
                    case TB_ATOMIC_AND:
                    case TB_ATOMIC_XOR:
                    case TB_ATOMIC_OR:
                    case TB_ATOMIC_PTROFF:
                    case TB_ATOMIC_CAS: {
                        static const char* order_names[] = {
                            "relaxed", "consume", "acquire",
                            "release", "acqrel", "seqcst"
                        };

                        TB_NodeAtomic* atomic = TB_NODE_GET_EXTRA(n);
                        s_writef(s, " !order(%s)", order_names[atomic->order]);
                        if (n->type == TB_ATOMIC_CAS) {
                            s_writef(s, " !fail_order(%s)", order_names[atomic->order2]);
                        }
                        break;
                    }

                    case TB_CALL:
                    case TB_TAILCALL:
                    case TB_SYSCALL:
                    case TB_SAFEPOINT:
                    case TB_MERGEMEM:
                    case TB_SPLITMEM:
                    break;

                    case TB_DEBUG_LOCATION: {
                        TB_NodeDbgLoc* loc = TB_NODE_GET_EXTRA(n);
                        s_writef(s, " // %s:%d", loc->file->path, loc->line);
                        break;
                    }

                    case TB_LOCAL: {
                        TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
                        s_writef(s, "!size(%u) !align(%u)", l->size, l->align);
                        if (l->type) {
                            s_writef(s, " !var(%s)", l->name);
                        }
                        break;
                    }

                    case TB_MACH_SYMBOL: {
                        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeMachSymbol)->sym;
                        if (sym->name[0]) {
                            s_writef(s, "%s", sym->name);
                        } else {
                            s_writef(s, "sym%p", sym);
                        }
                        break;
                    }

                    case TB_MACH_TEMP: {
                        /* TB_NodeMachTemp* tmp = TB_NODE_GET_EXTRA(n);
                        s_writef(s, "def=");
                        tb__print_regmask(tmp->def);
                        s_writef(s, " ");*/
                        break;
                    }

                    case TB_MACH_COPY:
                    break;

                    case TB_VSHUFFLE: {
                        TB_NodeVShuffle* shuf = TB_NODE_GET_EXTRA(n);
                        s_writef(s, ", [");
                        FOR_N(i, 0, shuf->width) {
                            if (i) { s_writef(s, ", "); }
                            s_writef(s, "%d", shuf->indices[i]);
                        }
                        s_writef(s, "]");
                        break;
                    }

                    default: {
                        int family = n->type / 0x100;
                        if (family == 0) {
                            TB_ASSERT_MSG(extra_bytes(n) == 0, "TODO");
                        } else {
                            TB_ASSERT(family >= 1 && family < TB_ARCH_MAX);

                            s_writef(s, ", ");
                            tb_codegen_families[family].print_extra(n);
                        }

                        if (cfg_is_branch(n)) {
                            TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
                            TB_Node** restrict succ = tb_arena_alloc(&f->tmp_arena, n->user_count * sizeof(TB_Node**));

                            // fill successors
                            int limit = 0;
                            FOR_USERS(u, n) {
                                if (USERN(u)->type == TB_BRANCH_PROJ) {
                                    int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                                    if (index + 1 > limit) {
                                        limit = index + 1;
                                    }
                                    succ[index] = USERN(u);
                                }
                            }

                            s_writef(s, " => {");
                            s_newline(s);

                            FOR_N(i, 0, limit) {
                                int64_t key = TB_NODE_GET_EXTRA_T(succ[i], TB_NodeBranchProj)->key;

                                if (i != 0) s_writef(s, "    %"PRId64": ", key);
                                else s_writef(s, "    default: ");

                                print_branch_edge(ctx, succ[i], s, false);
                                s_newline(s);
                            }
                            s_writef(s, "  }");
                            tb_arena_restore(&f->tmp_arena, sp);
                        }
                        break;
                    }
                }
                break;
            }
        }

        if (n->dt.type == TB_TAG_TUPLE) {
            s_color(s, 32);
            s_writef(s, " // tuple %%%u", n->gvn);
            s_color(s, 0);
        }

        s_newline(s);
    }
    dyn_array_clear(ws->items);

    if (!cfg_is_terminator(bb->end)) {
        s_writef(s, "  goto ");
        print_branch_edge(ctx, bb->end, s, true);
        s_newline(s);
    }
}

void tb_print_to_stream(TB_Function* f, OutStream* s) {
    if (f->super.tag != TB_SYMBOL_FUNCTION) {
        return;
    }

    size_t old_scheduled_n = f->scheduled_n;
    TB_BasicBlock** old_scheduled = f->scheduled;

    // the temp arena might've been freed, let's restore it
    if (f->tmp_arena.top == NULL) {
        tb_arena_create(&f->tmp_arena, "Tmp");
    }

    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
    f->scheduled = NULL;

    cuikperf_region_start("print", NULL);

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, f->node_count);

    PrinterCtx ctx = { 0 };
    ctx.f   = f;
    ctx.cfg = tb_compute_cfg(f, &ws, &f->tmp_arena, true);

    // schedule nodes
    tb_global_schedule(f, &ws, ctx.cfg, false, NULL);

    TB_BasicBlock* end_bb = NULL;
    aarray_for(i, ctx.cfg.blocks) {
        TB_BasicBlock* bb = &ctx.cfg.blocks[i];
        if (bb->end->type == TB_RETURN) {
            end_bb = bb;
            continue;
        } else if (is_empty_cproj(&ctx, bb->start)) {
            continue;
        }

        print_bb(&ctx, &ws, bb, s);
    }

    if (end_bb != NULL) {
        print_bb(&ctx, &ws, end_bb, s);
    }

    tb_clear_anti_deps(f, &ws);
    worklist_free(&ws);
    tb_free_cfg(&ctx.cfg);
    tb_arena_restore(&f->tmp_arena, sp);
    cuikperf_region_end();

    f->scheduled_n = old_scheduled_n;
    f->scheduled = old_scheduled;
}

void tb_print(TB_Function* f) {
    tb_print_to_stream(f, &OUT_STREAM_DEFAULT);
}

