// Metaprogram for generating x64 crap
#include <stdio.h>

static FILE* fp;

typedef struct {
    const char* name;
    const char* extra;
} NodeInfo;

static int node_info_count;
static NodeInfo node_infos[256];

void new_type(const char* name, const char* extra) {
    node_infos[node_info_count++] = (NodeInfo){ name, extra };
}

int main(void) {
    fp = fopen("tb/x64/x64_gen.inc", "wb");

    fprintf(fp, "// Hello, World!\n");
    new_type("int3", NULL);
    // standard integer ops
    new_type("add",  "X86MemOp");
    new_type("or",   "X86MemOp");
    new_type("and",  "X86MemOp");
    new_type("sub",  "X86MemOp");
    new_type("xor",  "X86MemOp");
    new_type("cmp",  "X86MemOp");
    new_type("mov",  "X86MemOp");
    new_type("test", "X86MemOp");
    // standard ops with immediate

    fprintf(fp, "typedef enum X86NodeType {\n");
    for (int i = 0; i < node_info_count; i++) {
        fprintf(fp, "    x86_%s,\n", node_infos[i].name);
    }
    fprintf(fp, "}\n\n");

    fprintf(fp, "static size_t extra_bytes(TB_Node* n) {\n");
    fprintf(fp, "    switch (n->type) {\n");
    for (int i = 0; i < node_info_count; i++) {
        if (node_infos[i].extra != NULL) {
            fprintf(fp, "        case x86_%s: return sizeof(%s);\n", node_infos[i].name, node_infos[i].extra);
        }
    }
    fprintf(fp, "        default: return 0;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n\n");

    fclose(fp);
    return 0;
}
