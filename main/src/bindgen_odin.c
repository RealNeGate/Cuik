#define STB_DS_IMPLEMENTATION
#include "bindgen.h"

static void odinprint__type(TranslationUnit* tu, DefinedTypeEntry** defined_types, Cuik_Type* type, int indent) {
    if (type->also_known_as) {
        printf("%s", type->also_known_as);
        return;
    }

    if (type->kind > KIND_DOUBLE) {
        // non-trivial types might require weird ordering or extra parenthesis
        switch (type->kind) {
            case KIND_PTR: {
                int level = 0;
                while (type->kind == KIND_PTR) {
                    type = cuik_canonical_type(type->ptr_to);
                    level += 1;
                }

                while (level--) printf("^");
                odinprint__type(tu, defined_types, type, indent);
                break;
            }

            case KIND_ARRAY: {
                if (type->array_count) {
                    printf("[%d]", type->array_count);
                } else {
                    printf("[]");
                }
                odinprint__type(tu, defined_types, cuik_canonical_type(type->array_of), indent);
                break;
            }

            case KIND_STRUCT:
            case KIND_UNION: {
                printf(type->kind == KIND_STRUCT ? "struct" : "struct #raw_union");

                bool defined_body = (type->record.kid_count > 0) && (hmgeti(*defined_types, type) < 0);
                if (defined_body) {
                    hmput(*defined_types, type, 1);
                    printf(" {\n");

                    for (size_t i = 0; i < type->record.kid_count; i++) {
                        for (size_t j = 0; j < indent+1; j++) printf("    ");

                        if (type->record.kids[i].name == NULL) {
                            printf("using _: ");
                        } else {
                            printf("%s: ", type->record.kids[i].name);
                        }

                        odinprint__type(tu, defined_types, cuik_canonical_type(type->record.kids[i].type), indent + 1);
                        printf(",\n");
                    }

                    for (size_t j = 0; j < indent; j++) printf("    ");
                    printf("}");
                }
                break;
            }

            case KIND_FUNC: {
                printf("proc(");
                for (size_t i = 0; i < type->func.param_count; i++) {
                    if (i) printf(", ");
                    printf("%s: ", type->func.param_list[i].name);
                    odinprint__type(tu, defined_types, cuik_canonical_type(type->func.param_list[i].type), indent);
                }
                printf(") -> ");
                odinprint__type(tu, defined_types, cuik_canonical_type(type->func.return_type), indent);
                break;
            }

            default: assert(0);
        }
    } else {
        // trivial types have trivial printing
        //
        //   TYPENAME NAME
        //
        switch (type->kind) {
            case KIND_VOID:   printf("void");       break;
            case KIND_BOOL:   printf("c.bool");     break;
            case KIND_CHAR:   printf("c.char");     break;
            case KIND_SHORT:  printf("c.short");    break;
            case KIND_INT:    printf("c.int");      break;
            case KIND_LONG:   printf("c.longlong"); break;
            case KIND_FLOAT:  printf("c.float");    break;
            case KIND_DOUBLE: printf("c.double");   break;
            case KIND_ENUM: {
                bool defined_body = (type->enumerator.count > 0) && (hmgeti(*defined_types, type) < 0);

                printf("enum");
                if (defined_body) {
                    hmput(*defined_types, type, 1);
                    printf(" {\n");

                    for (size_t i = 0; i < type->enumerator.count; i++) {
                        for (size_t j = 0; j < indent+1; j++) printf("    ");

                        printf("%s = %d,\n", type->enumerator.entries[i].key, type->enumerator.entries[i].value);
                    }

                    for (size_t j = 0; j < indent; j++) printf("    ");
                    printf("}");
                }
                break;
            }

            default: assert(0);
        }
    }
}

static void odinprint__decl(TranslationUnit* tu, DefinedTypeEntry** defined_types, Stmt* s) {
    bool is_export = false;

    if (s->op == STMT_FUNC_DECL) {
        Cuik_Attribute* a = s->attr_list;
        while (a != NULL) {
            if (strcmp(a->name, "export") == 0) {
                is_export = true;
                break;
            }

            a = a->prev;
        }
    } else if (s->op == STMT_GLOBAL_DECL) {
        is_export = true;
    }

    if (s->decl.attrs.is_typedef || is_export) {
        // if (s->decl.attrs.is_typedef) printf("typedef ");
        // if (s->decl.attrs.is_static) printf("static ");
        // if (s->decl.attrs.is_extern) printf("extern ");
        // if (s->decl.attrs.is_inline) printf("inline ");

        printf("%s :: ", s->decl.name);
        odinprint__type(tu, defined_types, cuik_canonical_type(s->decl.type), 0);

        if (s->op == STMT_GLOBAL_DECL) {
            printf(" ---");
        }
        printf("\n");
    }
}

static void odinprint__file(const char* filepath) {
    printf("\n\n\n// %s\n", filepath);
}

static void odinprint__include(const char* filepath) {
}

static void odinprint__define(Cuik_DefineIter def) {
    printf("// #define %.*s %.*s\n",
        (int)def.key.length, def.key.data,
        (int)def.value.length, def.value.data
    );
}

Bindgen bindgen__odin = {
    odinprint__file,
    odinprint__decl,
    odinprint__include,
    odinprint__define
};
