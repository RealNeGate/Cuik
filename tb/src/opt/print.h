
typedef struct {
    TB_Function* f;
    TB_CFG cfg;
} PrinterCtx;

const char* tb_node_get_name(TB_NodeTypeEnum n_type) {
    switch (n_type) {
        case TB_NULL: return "FREED";
        case TB_UNREACHABLE: return "unreachable";

        case TB_BRANCH_PROJ:   return "br_proj";

        case TB_ROOT:   return "root";
        case TB_RETURN: return "return";
        case TB_PROJ:   return "proj";
        case TB_REGION: return "region";
        case TB_NATURAL_LOOP: return "loop";
        case TB_AFFINE_LOOP: return "loop.affine";
        case TB_CALLGRAPH: return "callgraph";
        case TB_SYMBOL_TABLE: return "symbol_table";

        case TB_LOCAL: return "local";

        case TB_VA_START: return "vastart";
        case TB_DEBUGBREAK: return "dbgbrk";

        case TB_POISON: return "poison";
        case TB_DEAD: return "dead";
        case TB_TRAP: return "trap";
        case TB_ICONST: return "int";
        case TB_F32CONST: return "float32";
        case TB_F64CONST: return "float64";

        case TB_PHI: return "phi";
        case TB_SELECT: return "select";
        case TB_LOOKUP: return "lookup";

        case TB_PTR_OFFSET: return "ptr_offset";

        case TB_CYCLE_COUNTER: return "cyclecnt";
        case TB_PREFETCH: return "prefetch";
        case TB_INLINE_ASM: return "inlineasm";
        case TB_DEBUG_LOCATION: return "dbgloc";
        case TB_SAFEPOINT_POLL: return "safepoint.poll";

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
        case TB_NEG: return "neg";
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
        case TB_ADC: return "adc";

        case TB_FADD: return "fadd";
        case TB_FSUB: return "fsub";
        case TB_FMUL: return "fmul";
        case TB_FDIV: return "fdiv";
        case TB_FMAX: return "fmax";
        case TB_FMIN: return "fmin";
        case TB_FNEG: return "fneg";

        case TB_MULPAIR:  return "mulpair";
        case TB_SDIVMOD:  return "sdivmod";
        case TB_UDIVMOD:  return "udivmod";
        case TB_LOAD:     return "load";
        case TB_STORE:    return "store";
        case TB_READ:     return "read";
        case TB_WRITE:    return "write";

        case TB_CALL:     return "call";
        case TB_SYSCALL:  return "syscall";
        case TB_BRANCH:   return "branch";
        case TB_AFFINE_LATCH: return "affine_latch";
        case TB_NEVER_BRANCH: return "never_branch";
        case TB_TAILCALL: return "tailcall";

        case TB_MACH_MOVE: return "mach_move";
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

static int print_type(TB_DataType dt) {
    switch (dt.type) {
        case TB_TAG_INT:     return dt.data == 0 ? printf("void") : printf("i%d", dt.data);
        case TB_TAG_PTR:     return dt.data == 0 ? printf("ptr") : printf("ptr%d", dt.data);
        case TB_TAG_F32:     return printf("f32");
        case TB_TAG_F64:     return printf("f64");
        case TB_TAG_TUPLE:   return printf("tuple");
        case TB_TAG_CONTROL: return printf("ctrl");
        case TB_TAG_MEMORY:  return printf("mem");
        default: tb_todo();  return 0;
    }
}

static void print_type2(TB_DataType dt) {
    #if 0
    printf("\x1b[96m");
    print_type(dt);
    printf("\x1b[0m");
    #else
    print_type(dt);
    #endif
}

static void print_ref_to_node(PrinterCtx* ctx, TB_Node* n, bool def) {
    if (n == NULL) {
        printf("_");
    } else if (n->type == TB_ROOT) {
        printf("%s", ctx->f->super.name);

        if (def) {
            printf("(");
            TB_Node** params = ctx->f->params;
            FOR_N(i, 1, 3 + ctx->f->param_count) {
                if (i > 1) printf(", ");

                if (params[i] == NULL) {
                    printf("_");
                } else {
                    printf("%%%u: ", params[i]->gvn);
                    print_type2(params[i]->dt);
                }
            }

            FOR_USERS(u, n) if (USERN(u)->type == TB_MACH_PROJ) {
                printf(", %%%u: ", USERN(u)->gvn);
                print_type2(USERN(u)->dt);
            }
            printf(")");
        }
    } else if (cfg_is_region(n)) {
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
        ptrdiff_t i = try_find_traversal_index(&ctx->cfg, n);
        if (i >= 0) {
            if (r->tag != NULL) {
                printf(".bb%zu.%s", i, r->tag);
            } else {
                printf(".bb%zu", i);
            }
        } else {
            printf("*DEAD*");
        }

        if (def) {
            bool first = true;
            printf("(");
            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_PHI) {
                    if (first) {
                        first = false;
                    } else {
                        printf(", ");
                    }

                    printf("%%%u: ", USERN(u)->gvn);
                    print_type2(USERN(u)->dt);
                }
            }
            printf(") // region %%%u", n->gvn);
            if (n->type == TB_NATURAL_LOOP) printf(" !natural");
            if (n->type == TB_AFFINE_LOOP) printf(" !affine");
        }
    } else if (n->type == TB_F32CONST) {
        TB_NodeFloat32* f = TB_NODE_GET_EXTRA(n);
        printf("%f", f->value);
    } else if (n->type == TB_F64CONST) {
        TB_NodeFloat64* f = TB_NODE_GET_EXTRA(n);
        printf("%f", f->value);
    } else if (n->type == TB_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
        if (sym->name[0]) {
            printf("%s", sym->name);
        } else {
            printf("sym%p", sym);
        }
    } else if (cfg_is_cproj(n)) {
        if (n->inputs[0]->type == TB_ROOT) {
            print_ref_to_node(ctx, n->inputs[0], def);
        } else {
            ptrdiff_t i = try_find_traversal_index(&ctx->cfg, n);
            if (i >= 0) {
                printf(".bb%zu", i);
                if (def) {
                    // printf("()");
                    printf("() // cproj %%%u", n->gvn);
                }
            } else {
                printf("*DEAD*");
            }
        }
    } else if (n->type == TB_ICONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

        if (num->value < 0xFFFF) {
            int bits = n->dt.type == TB_TAG_PTR ? 64 : n->dt.data;
            printf("%"PRId64, tb__sxt(num->value, bits, 64));
        } else {
            printf("%#0"PRIx64, num->value);
        }
    } else {
        printf("%%%u", n->gvn);
    }
}

// deals with printing BB params
static void print_branch_edge(PrinterCtx* ctx, TB_Node* n, bool fallthru) {
    TB_Node* target = fallthru ? cfg_next_control(n) : cfg_next_bb_after_cproj(n);
    print_ref_to_node(ctx, target, false);

    // print phi args
    printf("(");
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
                    printf(", ");
                }

                TB_ASSERT(phi_i >= 0);
                print_ref_to_node(ctx, USERN(u)->inputs[phi_i], false);
            }
        }
    }
    printf(")");
}

static void print_bb(PrinterCtx* ctx, TB_Worklist* ws, TB_Node* bb_start) {
    print_ref_to_node(ctx, bb_start, true);
    printf("\n");

    TB_Function* f = ctx->f;
    TB_BasicBlock* bb = f->scheduled[bb_start->gvn];

    #ifndef NDEBUG
    TB_BasicBlock* expected = &nl_map_get_checked(ctx->cfg.node_to_block, bb_start);
    TB_ASSERT(expected == bb);
    #endif

    tb_greedy_scheduler(f, &ctx->cfg, ws, NULL, bb);

    FOR_N(i, ctx->cfg.block_count, dyn_array_length(ws->items)) {
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
            case TB_DEBUGBREAK:  printf("  debugbreak"); break;
            case TB_UNREACHABLE: printf("  unreachable"); break;

            case TB_NEVER_BRANCH: {
                TB_Node* taken = USERN(proj_with_index(n, 0));
                TB_Node* never = USERN(proj_with_index(n, 1));

                printf("  goto ");
                print_branch_edge(ctx, taken, false);
                printf(" // never branch ");
                print_branch_edge(ctx, never, false);
                break;
            }

            case TB_AFFINE_LATCH:
            case TB_BRANCH: {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
                TB_ArenaSavepoint sp = tb_arena_save(f->tmp_arena);
                TB_Node** restrict succ = tb_arena_alloc(f->tmp_arena, br->succ_count * sizeof(TB_Node**));

                // fill successors
                FOR_USERS(u, n) {
                    if (USERN(u)->type == TB_BRANCH_PROJ) {
                        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                        succ[index] = USERN(u);
                    }
                }

                TB_ASSERT(br->succ_count >= 2);
                if (br->succ_count == 2) {
                    int bits = n->inputs[1]->dt.type == TB_TAG_PTR ? 64 : n->inputs[1]->dt.data;

                    printf("  if ");
                    print_ref_to_node(ctx, n->inputs[1], false);
                    int64_t key = TB_NODE_GET_EXTRA_T(succ[1], TB_NodeBranchProj)->key;
                    if (key == 0) {
                        printf(" then ");
                    } else {
                        printf(" != %"PRId64" then ", key);
                    }
                    print_branch_edge(ctx, succ[0], false);
                    printf(" else ");
                    print_branch_edge(ctx, succ[1], false);
                } else {
                    printf("  br ");
                    print_ref_to_node(ctx, n->inputs[1], false);
                    printf("%s=> {\n", n->input_count > 1 ? " " : "");

                    FOR_N(i, 0, br->succ_count) {
                        int64_t key = TB_NODE_GET_EXTRA_T(succ[i], TB_NodeBranchProj)->key;

                        if (i != 0) printf("    %"PRId64": ", key);
                        else printf("    default: ");

                        print_branch_edge(ctx, succ[i], false);
                        printf("\n");
                    }
                    printf("  }");
                }
                printf(" // branch %%%u", n->gvn);
                tb_arena_restore(f->tmp_arena, sp);
                break;
            }

            case TB_TRAP: {
                printf("  trap");
                break;
            }

            case TB_RETURN: {
                printf("  return ");
                FOR_N(i, 1, n->input_count) {
                    if (i != 1) printf(", ");
                    print_ref_to_node(ctx, n->inputs[i], false);
                }
                break;
            }

            default: {
                if (cfg_is_branch(n)) {
                    printf("  %s ", tb_node_get_name(n->type));
                } else if (n->dt.type == TB_TAG_TUPLE) {
                    // print with multiple returns
                    TB_Node* projs[32] = { 0 };
                    FOR_USERS(u, n) {
                        if (USERN(u)->type == TB_PROJ) {
                            int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                            projs[index] = USERN(u);
                        }
                    }

                    printf("  ");

                    size_t first = projs[0] && projs[0]->dt.type == TB_TAG_CONTROL ? 1 : 0;
                    FOR_N(i, first, 32) {
                        if (projs[i] == NULL) break;
                        if (i > first) printf(", ");
                        printf("%%%u", projs[i]->gvn);
                    }
                    printf(" = %s.(", tb_node_get_name(n->type));
                    FOR_N(i, first, 32) {
                        if (projs[i] == NULL) break;
                        if (i > first) printf(", ");
                        print_type2(projs[i]->dt);
                    }
                    printf(")");
                } else {
                    // print as normal instruction
                    printf("  %%%u = %s.", n->gvn, tb_node_get_name(n->type));

                    TB_DataType dt = n->dt;
                    if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
                        dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                    } else if (n->type == TB_STORE) {
                        dt = n->inputs[3]->dt;
                    }
                    print_type2(dt);
                }
                printf(" ");

                size_t first = n->type == TB_PROJ ? 0 : 1;
                FOR_N(i, first, n->input_count) {
                    if (i != first) printf(", ");
                    print_ref_to_node(ctx, n->inputs[i], false);
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
                        printf(", %d", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
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
                        if (b->ab & TB_ARITHMATIC_NSW) printf(" !nsw");
                        if (b->ab & TB_ARITHMATIC_NUW) printf(" !nuw");
                        break;
                    }

                    case TB_LOAD:
                    case TB_STORE:
                    case TB_MEMSET:
                    case TB_MEMCPY: {
                        TB_NodeMemAccess* mem = TB_NODE_GET_EXTRA(n);
                        printf(" !align(%d)", mem->align);
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
                        printf(" !order(%s)", order_names[atomic->order]);
                        if (n->type == TB_ATOMIC_CAS) {
                            printf(" !fail_order(%s)", order_names[atomic->order2]);
                        }
                        break;
                    }

                    case TB_CALL:
                    case TB_TAILCALL:
                    case TB_SYSCALL:
                    case TB_SAFEPOINT_POLL:
                    case TB_MERGEMEM:
                    case TB_SPLITMEM:
                    break;

                    case TB_DEBUG_LOCATION: {
                        TB_NodeDbgLoc* loc = TB_NODE_GET_EXTRA(n);
                        printf(" // %s:%d", loc->file->path, loc->line);
                        break;
                    }

                    case TB_LOCAL: {
                        TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
                        printf("!size(%u) !align(%u)", l->size, l->align);
                        if (l->type) {
                            printf(" !var(%s)", l->name);
                        }
                        break;
                    }

                    case TB_MACH_SYMBOL: {
                        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeMachSymbol)->sym;
                        if (sym->name[0]) {
                            printf("%s", sym->name);
                        } else {
                            printf("sym%p", sym);
                        }
                        break;
                    }

                    case TB_LOOKUP: {
                        TB_NodeLookup* l = TB_NODE_GET_EXTRA(n);

                        printf(" { default: %"PRId64, l->entries[0].val);
                        FOR_N(i, 1, l->entry_count) {
                            printf(", %"PRId64": %"PRId64, l->entries[i].key, l->entries[i].val);
                        }
                        printf("}");
                        break;
                    }

                    case TB_MACH_COPY:
                    case TB_MACH_MOVE:
                    break;

                    default: {
                        int family = n->type / 0x100;
                        if (family == 0) {
                            TB_ASSERT_MSG(extra_bytes(n) == 0, "TODO");
                        } else {
                            TB_ASSERT(family >= 1 && family < TB_ARCH_MAX);
                            tb_codegen_families[family].print_extra(n);
                        }

                        if (cfg_is_branch(n)) {
                            TB_ArenaSavepoint sp = tb_arena_save(f->tmp_arena);
                            TB_Node** restrict succ = tb_arena_alloc(f->tmp_arena, n->user_count * sizeof(TB_Node**));

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

                            printf(" => {\n");
                            FOR_N(i, 0, limit) {
                                int64_t key = TB_NODE_GET_EXTRA_T(succ[i], TB_NodeBranchProj)->key;

                                if (i != 0) printf("    %"PRId64": ", key);
                                else printf("    default: ");

                                print_branch_edge(ctx, succ[i], false);
                                printf("\n");
                            }
                            printf("  }");
                            printf(" // branch %%%u", n->gvn);
                            tb_arena_restore(f->tmp_arena, sp);
                        }
                        break;
                    }
                }
                break;
            }
        }

        printf("\n");
    }

    dyn_array_set_length(ws->items, ctx->cfg.block_count);

    if (!cfg_is_terminator(bb->end)) {
        printf("  goto ");
        print_branch_edge(ctx, bb->end, true);
        printf("\n");
    }
}

void tb_print(TB_Function* f, TB_Arena* tmp) {
    if (tmp == NULL) {
        tmp = f->tmp_arena;
    }

    size_t old_scheduled_n = f->scheduled_n;
    TB_BasicBlock** old_scheduled = f->scheduled;
    TB_ArenaSavepoint sp = tb_arena_save(tmp);
    TB_Arena* old = f->tmp_arena;
    f->tmp_arena = tmp;
    f->scheduled = NULL;

    cuikperf_region_start("print", NULL);

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, f->node_count);

    PrinterCtx ctx = { 0 };
    ctx.f   = f;
    ctx.cfg = tb_compute_rpo(f, &ws);

    // schedule nodes
    tb_global_schedule(f, &ws, ctx.cfg, false, false, NULL);

    TB_Node* end_bb = NULL;
    FOR_N(i, 0, ctx.cfg.block_count) {
        TB_Node* end = nl_map_get_checked(ctx.cfg.node_to_block, ws.items[i]).end;
        if (end->type == TB_RETURN) {
            end_bb = ws.items[i];
            continue;
        }

        print_bb(&ctx, &ws, ws.items[i]);
    }

    if (end_bb != NULL) {
        print_bb(&ctx, &ws, end_bb);
    }
    worklist_free(&ws);
    tb_free_cfg(&ctx.cfg);

    f->scheduled_n = old_scheduled_n;
    f->scheduled = old_scheduled;
    f->tmp_arena = old;

    tb_arena_restore(tmp, sp);
    cuikperf_region_end();
}
