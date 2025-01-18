// Hello, World!
typedef enum X86NodeType {
    x86_int3,
    x86_add,
    x86_or,
    x86_and,
    x86_sub,
    x86_xor,
    x86_cmp,
    x86_mov,
    x86_test,
}

static size_t extra_bytes(TB_Node* n) {
    switch (n->type) {
        case x86_add: return sizeof(X86MemOp);
        case x86_or: return sizeof(X86MemOp);
        case x86_and: return sizeof(X86MemOp);
        case x86_sub: return sizeof(X86MemOp);
        case x86_xor: return sizeof(X86MemOp);
        case x86_cmp: return sizeof(X86MemOp);
        case x86_mov: return sizeof(X86MemOp);
        case x86_test: return sizeof(X86MemOp);
        default: return 0;
    }
}

