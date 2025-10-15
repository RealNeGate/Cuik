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

static const NodeVtable node_vtables[TB_NODE_TYPE_MAX] = {
    // type                 ideal              identity            value
    [TB_ICONST]         = { NULL,              NULL,               value_int        },
    [TB_F32CONST]       = { NULL,              NULL,               value_f32        },
    [TB_F64CONST]       = { NULL,              NULL,               value_f64        },
    // memory
    [TB_LOAD]           = { ideal_load,        identity_load,      NULL             },
    [TB_STORE]          = { ideal_store,       NULL,               value_mem        },
    [TB_MEMSET]         = { NULL,              NULL,               value_mem        },
    [TB_MEMCPY]         = { ideal_memcpy,      NULL,               value_mem        },
    [TB_SPLITMEM]       = { ideal_split_mem,   NULL,               value_mem        },
    [TB_MERGEMEM]       = { ideal_merge_mem,   NULL,               value_mem        },
    // ptr values
    [TB_LOCAL]          = { NULL,              NULL,               value_ptr_vals   },
    [TB_SYMBOL]         = { NULL,              NULL,               value_ptr_vals   },
    // pointer arithmetic
    [TB_PTR_OFFSET]     = { ideal_ptr_offset,  identity_ptr_offset,NULL             },
    // arithmetic
    [TB_ADD]            = { ideal_arith,       identity_int_binop, value_arith      },
    [TB_SUB]            = { ideal_arith,       identity_int_binop, value_arith      },
    [TB_MUL]            = { ideal_arith,       identity_int_binop, value_arith      },
    [TB_UDIV]           = { ideal_int_div,     identity_int_binop, NULL             },
    [TB_SDIV]           = { ideal_int_div,     identity_int_binop, NULL             },
    [TB_UMOD]           = { ideal_int_mod,     identity_int_binop, NULL             },
    [TB_SMOD]           = { ideal_int_mod,     identity_int_binop, NULL             },
    // floats
    [TB_FNEG]           = { NULL,              NULL,               value_fpneg      },
    [TB_FADD]           = { ideal_farith,      identity_flt_binop, value_farith     },
    [TB_FSUB]           = { ideal_farith,      identity_flt_binop, value_farith     },
    [TB_FMUL]           = { ideal_farith,      identity_flt_binop, value_farith     },
    [TB_FDIV]           = { ideal_farith,      identity_flt_binop, value_farith     },
    // comparisons
    [TB_CMP_EQ]         = { ideal_cmp,         identity_int_binop, value_cmp        },
    [TB_CMP_NE]         = { ideal_cmp,         identity_int_binop, value_cmp        },
    [TB_CMP_SLT]        = { ideal_cmp,         identity_int_binop, value_cmp        },
    [TB_CMP_SLE]        = { ideal_cmp,         identity_int_binop, value_cmp        },
    [TB_CMP_ULT]        = { ideal_cmp,         identity_int_binop, value_cmp        },
    [TB_CMP_ULE]        = { ideal_cmp,         identity_int_binop, value_cmp        },
    [TB_CMP_FLT]        = { NULL,              NULL,               value_cmp        },
    [TB_CMP_FLE]        = { NULL,              NULL,               value_cmp        },
    // bitwise ops
    [TB_AND]            = { ideal_bits,        identity_bits,      value_bits       },
    [TB_OR]             = { ideal_bits,        identity_bits,      value_bits       },
    [TB_XOR]            = { ideal_bits,        identity_bits,      value_bits       },
    // shift
    [TB_SHL]            = { ideal_shift,       identity_int_binop, value_shift      },
    [TB_SHR]            = { ideal_shift,       identity_int_binop, value_shift      },
    [TB_SAR]            = { NULL,              identity_int_binop, value_shift      },
    // casts
    [TB_BITCAST]        = { ideal_bitcast,     NULL,               value_bitcast    },
    [TB_TRUNCATE]       = { ideal_truncate,    NULL,               value_trunc      },
    [TB_ZERO_EXT]       = { ideal_extension,   NULL,               value_zext       },
    [TB_SIGN_EXT]       = { ideal_extension,   NULL,               value_sext       },
    [TB_FLOAT_EXT]      = { NULL,              NULL,               value_fpext      },
    [TB_INT2FLOAT]      = { NULL,              NULL,               value_int2float  },
    // misc
    [TB_PROJ]           = { NULL,              NULL,               value_proj       },
    [TB_BRANCH_PROJ]    = { NULL,              NULL,               value_proj       },
    [TB_SELECT]         = { ideal_select,      identity_select,    value_select     },
    [TB_PHI]            = { ideal_phi,         identity_phi,       value_phi        },
    [TB_POISON]         = { NULL,              NULL,               value_poison     },
    // control flow
    [TB_DEBUG_LOCATION] = { ideal_location,    NULL,               NULL             },
    [TB_RETURN]         = { ideal_return,      NULL,               value_ctrl       },
    [TB_REGION]         = { ideal_region,      NULL,               value_region,    },
    [TB_NATURAL_LOOP]   = { ideal_region,      NULL,               value_region,    },
    [TB_AFFINE_LOOP]    = { ideal_region,      NULL,               value_region,    },
    [TB_BRANCH]         = { ideal_branch,      NULL,               value_branch,    },
    [TB_AFFINE_LATCH]   = { ideal_if,          NULL,               value_if,        },
    [TB_IF]             = { ideal_if,          NULL,               value_if,        },
    [TB_CALL]           = { ideal_libcall,     NULL,               value_call,      },
    [TB_TAILCALL]       = { NULL,              NULL,               value_ctrl,      },
    [TB_SYSCALL]        = { NULL,              NULL,               value_call,      },
    [TB_DEBUGBREAK]     = { NULL,              NULL,               value_ctrl,      },
    [TB_TRAP]           = { NULL,              NULL,               value_ctrl,      },
    [TB_UNREACHABLE]    = { NULL,              NULL,               value_ctrl,      },
    [TB_BLACKHOLE]      = { NULL,              NULL,               value_ctrl,      },
    [TB_DEAD]           = { NULL,              NULL,               value_dead,      },
    [TB_DEAD_STORE]     = { NULL,              identity_dead_store,value_mem,       },
    [TB_ROOT]           = { NULL,              NULL,               value_root,      },
};

bool tb_node_mem_read_only(TB_Node* n) {
    return tb_node_is_memory_in(n) && !tb_node_is_memory_out(n);
}

// has potential memory dep on inputs[1]
TB_Node* tb_node_mem_in(TB_Node* n) {
    if (tb_node_is_memory_in(n)) {
        return n->input_count > 1 ? n->inputs[1] : NULL;
    }
    return NULL;
}
