// This is the table of all peephole rewrite rules, there's a few categories:
//
// * Idealize: replaces a node with a simplified form, this is run until exhaustion
//             on a node (although in practice will warn past 5 trips, it's weird to
//             even do more than 2 trips)
//
// * Identity: replace a node with it's direct inputs (one step).
//
// * ValueOf:  constant propagation crap (works for pessimistic and optimistic crap out of the box)
//
// * GVN:      if a node has some identical copy, it will be replaced with it.
//
// They're run in this order for every node and given each is well-formed (one step Church-Rosser)
// the number of rewrites performed should scale linearly with the size of the IR.
typedef TB_Node* (*NodeIdealize)(TB_Function* f, TB_Node* n);
typedef TB_Node* (*NodeIdentity)(TB_Function* f, TB_Node* n);
typedef Lattice* (*NodeValueOf)(TB_Function* f, TB_Node* n);

typedef struct {
    NodeIdealize idealize;
    NodeIdentity identity;
    NodeValueOf  value;
} NodeVtable;

static const uint32_t node_flags[TB_NODE_TYPE_MAX] = {
    [TB_ROOT]           =             NODE_TERMINATOR | NODE_END,
    [TB_RETURN]         = NODE_CTRL | NODE_TERMINATOR | NODE_END,
    [TB_TRAP]           = NODE_CTRL | NODE_TERMINATOR | NODE_END,
    [TB_UNREACHABLE]    = NODE_CTRL | NODE_TERMINATOR | NODE_END,
    [TB_TAILCALL]       = NODE_CTRL | NODE_TERMINATOR | NODE_END,

    [TB_BRANCH]         = NODE_CTRL | NODE_TERMINATOR | NODE_FORK_CTRL | NODE_BRANCH,
    [TB_AFFINE_LATCH]   = NODE_CTRL | NODE_TERMINATOR | NODE_FORK_CTRL | NODE_BRANCH,
    [TB_NEVER_BRANCH]   = NODE_CTRL | NODE_TERMINATOR | NODE_FORK_CTRL,

    [TB_DEBUG_LOCATION] = NODE_CTRL,
    [TB_CALL]           = NODE_CTRL,
    [TB_SYSCALL]        = NODE_CTRL,
    [TB_REGION]         = NODE_CTRL,
    [TB_NATURAL_LOOP]   = NODE_CTRL,
    [TB_AFFINE_LOOP]    = NODE_CTRL,
    [TB_SAFEPOINT_POLL] = NODE_CTRL,
    [TB_DEBUGBREAK]     = NODE_CTRL,
    [TB_READ]           = NODE_CTRL,
    [TB_WRITE]          = NODE_CTRL,
};

static const NodeVtable node_vtables[TB_NODE_TYPE_MAX] = {
    // type                 ideal              identity            value
    [TB_ICONST]         = { NULL,              NULL,               value_int        },
    [TB_F32CONST]       = { NULL,              NULL,               value_f32        },
    [TB_F64CONST]       = { NULL,              NULL,               value_f64        },
    // memory
    [TB_LOAD]           = { ideal_load,        identity_load,      NULL             },
    [TB_STORE]          = { ideal_store,       NULL,               value_mem        },
    [TB_MEMSET]         = { ideal_memset,      NULL,               value_mem        },
    [TB_MEMCPY]         = { ideal_memcpy,      NULL,               value_mem        },
    [TB_SPLITMEM]       = { NULL,              NULL,               value_split_mem  },
    [TB_MERGEMEM]       = { ideal_merge_mem,   NULL,               value_merge_mem  },
    // ptr values
    [TB_LOCAL]          = { NULL,              NULL,               value_ptr_vals   },
    [TB_SYMBOL]         = { NULL,              NULL,               value_ptr_vals   },
    // pointer arithmetic
    [TB_PTR_OFFSET]     = { ideal_ptr_offset,  identity_int_binop, NULL             },
    // arithmetic
    [TB_ADD]            = { ideal_int_binop,   identity_int_binop, value_add_mul    },
    [TB_SUB]            = { ideal_int_binop,   identity_int_binop, value_sub        },
    [TB_MUL]            = { ideal_int_binop,   identity_int_binop, value_add_mul    },
    [TB_UDIV]           = { ideal_int_div,     identity_int_binop, NULL             },
    [TB_SDIV]           = { ideal_int_div,     identity_int_binop, NULL             },
    [TB_UMOD]           = { ideal_int_mod,     identity_int_binop, NULL             },
    [TB_SMOD]           = { ideal_int_mod,     identity_int_binop, NULL             },
    // comparisons
    [TB_CMP_EQ]         = { ideal_int_binop,   identity_int_binop, value_cmp        },
    [TB_CMP_NE]         = { ideal_int_binop,   identity_int_binop, value_cmp        },
    [TB_CMP_SLT]        = { ideal_int_binop,   identity_int_binop, value_cmp        },
    [TB_CMP_SLE]        = { ideal_int_binop,   identity_int_binop, value_cmp        },
    [TB_CMP_ULT]        = { ideal_int_binop,   identity_int_binop, value_cmp        },
    [TB_CMP_ULE]        = { ideal_int_binop,   identity_int_binop, value_cmp        },
    // bitwise ops
    [TB_AND]            = { ideal_int_binop,   identity_int_binop, value_bits       },
    [TB_OR]             = { ideal_int_binop,   identity_int_binop, value_bits       },
    [TB_XOR]            = { ideal_int_binop,   identity_int_binop, value_bits       },
    // shift
    [TB_SHL]            = { ideal_int_binop,   identity_int_binop, value_shift      },
    [TB_SHR]            = { ideal_int_binop,   identity_int_binop, value_shift      },
    [TB_SAR]            = { ideal_int_binop,   identity_int_binop, value_shift      },
    // unary
    [TB_NEG]            = { NULL,              NULL,               value_negate     },
    // casts
    [TB_BITCAST]        = { ideal_bitcast,     NULL,               value_bitcast    },
    [TB_TRUNCATE]       = { ideal_truncate,    NULL,               value_trunc      },
    [TB_ZERO_EXT]       = { ideal_extension,   NULL,               value_zext       },
    [TB_SIGN_EXT]       = { ideal_extension,   NULL,               value_sext       },
    // misc
    [TB_LOOKUP]         = { NULL,              NULL,               value_lookup     },
    [TB_PROJ]           = { NULL,              NULL,               value_proj       },
    [TB_BRANCH_PROJ]    = { NULL,              NULL,               value_proj       },
    [TB_SELECT]         = { ideal_select,      NULL,               value_select     },
    [TB_PHI]            = { ideal_phi,         identity_phi,       value_phi        },
    // control flow
    [TB_RETURN]         = { ideal_return,      NULL,               value_ctrl       },
    [TB_REGION]         = { ideal_region,      identity_region,    value_region,    },
    [TB_NATURAL_LOOP]   = { ideal_region,      identity_region,    value_region,    },
    [TB_AFFINE_LOOP]    = { ideal_region,      identity_region,    value_region,    },
    [TB_BRANCH]         = { ideal_branch,      NULL,               value_branch,    },
    [TB_AFFINE_LATCH]   = { ideal_branch,      NULL,               value_branch,    },
    [TB_SAFEPOINT_POLL] = { NULL,              identity_safepoint, value_ctrl,      },
    [TB_CALL]           = { ideal_libcall,     NULL,               value_call,      },
    [TB_TAILCALL]       = { NULL,              NULL,               value_ctrl,      },
    [TB_SYSCALL]        = { NULL,              NULL,               value_call,      },
    [TB_DEBUGBREAK]     = { NULL,              NULL,               value_ctrl,      },
    [TB_TRAP]           = { NULL,              NULL,               value_ctrl,      },
    [TB_UNREACHABLE]    = { NULL,              NULL,               value_ctrl,      },
    [TB_DEAD]           = { NULL,              NULL,               value_dead,      },
    [TB_ROOT]           = { NULL,              NULL,               value_root,      },
};

bool cfg_is_region(TB_Node* n) {
    return n->type >= TB_REGION && n->type <= TB_AFFINE_LOOP;
}

bool cfg_is_natural_loop(TB_Node* n) {
    return n->type >= TB_NATURAL_LOOP && n->type <= TB_AFFINE_LOOP;
}

uint32_t cfg_flags(TB_Node* n) {
    if (n->type >= TB_NODE_TYPE_MAX) {
        int family = n->type / 0x100;
        assert(family >= 1 && family < TB_ARCH_MAX);
        return tb_codegen_families[family].flags(n);
    } else {
        return node_flags[n->type];
    }
}

bool cfg_is_branch(TB_Node* n)     { return cfg_flags(n) & NODE_BRANCH; }
bool cfg_is_fork(TB_Node* n)       { return cfg_flags(n) & NODE_FORK_CTRL; }
bool cfg_is_terminator(TB_Node* n) { return cfg_flags(n) & NODE_TERMINATOR; }
bool cfg_is_endpoint(TB_Node* n)   { return cfg_flags(n) & NODE_END; }

