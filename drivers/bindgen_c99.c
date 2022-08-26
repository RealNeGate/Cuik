#include "bindgen.h"

static void cprint__type(TranslationUnit* tu, DefinedTypeEntry** defined_types, Cuik_Type* type, const char* name, int indent) {
    // a based type in a cringe system can make all the difference in the world
    while (type->based) {
        if (type->also_known_as) {
            printf("%s", type->also_known_as);

            if (name) {
                printf(" %s", name);
            }
            return;
        }

        type = type->based;
    }

    if (type->kind > KIND_DOUBLE) {
        // non-trivial types might require weird ordering or extra parenthesis
        switch (type->kind) {
            case KIND_PTR: {
                int level = 0;
                while (type->kind == KIND_PTR) {
                    type = type->ptr_to;
                    level += 1;
                }

                // type is now the base type and we have the number of stars in levels
                if (type->kind == KIND_FUNC) {
                    // function pointer amirite
                    //
                    //   void foo()
                    //      VVV
                    //     PTRIFY
                    //      VVV
                    //  void (*foo)()
                    cprint__type(tu, defined_types, type->func.return_type, NULL, indent);
                    printf(" (");
                    while (level--) printf("*");

                    if (name) {
                        printf("%s", name);
                    }
                    printf(")(");

                    // print suffix
                    for (size_t i = 0; i < type->func.param_count; i++) {
                        if (i) printf(", ");
                        cprint__type(tu, defined_types, type->func.param_list[i].type, type->func.param_list[i].name, indent);
                    }
                    printf(")");
                } else {
                    cprint__type(tu, defined_types, type, NULL, indent);
                    while (level--) printf("*");

                    if (name) {
                        printf(" %s", name);
                    }
                }
                break;
            }

            case KIND_ARRAY: {
                cprint__type(tu, defined_types, type->array_of, name, indent);

                // suffix
                while (type->kind == KIND_ARRAY) {
                    if (type->array_count) {
                        printf("[%d]", type->array_count);
                    } else {
                        printf("[]");
                    }

                    type = type->array_of;
                }
                break;
            }

            case KIND_STRUCT:
            case KIND_UNION: {
                printf(type->kind == KIND_STRUCT ? "struct" : "union");

                bool defined_body = (type->record.kid_count > 0) && (hmgeti(*defined_types, type) < 0);
                if (type->record.name) {
                    // TODO(NeGate): we only define the body the first time around
                    printf(" %s", type->record.name);
                }

                if (defined_body) {
                    hmput(*defined_types, type, 1);
                    printf(" {\n");

                    for (size_t i = 0; i < type->record.kid_count; i++) {
                        for (size_t j = 0; j < indent+1; j++) printf("    ");

                        cprint__type(tu, defined_types, type->record.kids[i].type, type->record.kids[i].name, indent + 1);
                        printf(";\n");
                    }

                    for (size_t j = 0; j < indent; j++) printf("    ");
                    printf("}");
                }

                if (name) {
                    printf(" %s", name);
                }
                break;
            }

            case KIND_FUNC: {
                cprint__type(tu, defined_types, type->func.return_type, NULL, indent);

                if (name) {
                    printf(" %s", name);
                }

                printf("(");
                for (size_t i = 0; i < type->func.param_count; i++) {
                    if (i) printf(", ");
                    cprint__type(tu, defined_types, type->func.param_list[i].type, type->func.param_list[i].name, indent);
                }
                printf(")");
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
            case KIND_VOID:   printf("void");      break;
            case KIND_BOOL:   printf("_Bool");     break;
            case KIND_CHAR:   printf("char");      break;
            case KIND_SHORT:  printf("short");     break;
            case KIND_INT:    printf("int");       break;
            case KIND_LONG:   printf("long long"); break;
            case KIND_FLOAT:  printf("float");     break;
            case KIND_DOUBLE: printf("double");    break;
            case KIND_ENUM: {
                bool defined_body = (type->enumerator.count > 0) && (hmgeti(*defined_types, type) < 0);

                printf("enum");
                if (type->enumerator.name) {
                    printf(" %s", type->enumerator.name);
                }

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

        if (name) printf(" %s", name);
    }
}

static void cprint__decl(TranslationUnit* tu, DefinedTypeEntry** defined_types, Stmt* s) {
    bool is_export = false;
    Cuik_Attribute* a = s->attr_list;
    while (a != NULL) {
        if (strcmp(a->name, "export") == 0) {
            is_export = true;
            break;
        }

        a = a->prev;
    }

    if (s->decl.attrs.is_typedef || (is_export && s->op == STMT_FUNC_DECL)) {
        if (s->decl.attrs.is_typedef) printf("typedef ");
        if (s->decl.attrs.is_static) printf("static ");
        if (s->decl.attrs.is_extern) printf("extern ");
        if (s->decl.attrs.is_inline) printf("inline ");

        cprint__type(tu, defined_types, s->decl.type, s->decl.name, 0);
        printf(";\n");
    }
}

static void cprint__file(const char* filepath) {
    printf("\n\n\n// %s\n", filepath);
}

static void cprint__include(const char* filepath) {
    printf("#include <%s>\n", filepath);
}

static void cprint__define(Cuik_DefineIter def) {
    printf("#define %.*s %.*s\n",
        (int)def.key.len, def.key.data,
        (int)def.value.len, def.value.data
    );
}

Bindgen bindgen__c99 = {
    cprint__file,
    cprint__decl,
    cprint__include,
    cprint__define
};
