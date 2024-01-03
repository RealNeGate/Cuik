
#include <hash_map.h>

static NL_Strmap(int) already_defined;
static NL_Map(Cuik_Type*, Atom) typedefs;

static void print_odin_type(Cuik_Type* type, int depth, bool top) {
    if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        while (type != type->record.nominal) {
            type = type->record.nominal;
        }
    }

    if (!top) {
        ptrdiff_t search = nl_map_get(typedefs, type);
        if (search >= 0) {
            printf("%s", typedefs[search].v);
            return;
        }
    }

    switch (type->kind) {
        case KIND_VOID: printf("c.void"); break;
        case KIND_BOOL: printf("c.bool"); break;
        case KIND_CHAR: printf("c.char"); break;
        case KIND_SHORT: printf("c.short"); break;
        case KIND_INT: printf("c.int"); break;
        case KIND_LONG: printf("c.long"); break;
        case KIND_LLONG: printf("c.longlong"); break;
        case KIND_FLOAT: printf("c.float"); break;
        case KIND_DOUBLE: printf("c.double"); break;

        case KIND_ENUM: printf("c.int"); break;

        case KIND_STRUCT:
        case KIND_UNION: {
            if (type->record.kid_count == 0) {
                printf("struct {}");
                break;
            }

            printf("struct %s{\n", type->kind == KIND_UNION ? "#raw_union " : "");

            for (size_t i = 0; i < type->record.kid_count; i++) {
                Member* m = &type->record.kids[i];

                // indentation
                for (size_t j = 0; j <= depth; j++) printf("    ");

                if (m->name == NULL) {
                    printf("using _: ");
                } else {
                    printf("%s: ", m->name);
                }
                print_odin_type(cuik_canonical_type(m->type), depth + 1, false);
                printf(",\n");
            }

            // indentation
            for (size_t j = 0; j < depth; j++) printf("    ");
            printf("}");
            break;
        }

        case KIND_ARRAY: {
            Cuik_Type* base = cuik_canonical_type(type->array.of);

            printf("[%d]", type->array.count);
            print_odin_type(base, depth, false);
            break;
        }
        case KIND_PTR: {
            Cuik_Type* base = cuik_canonical_type(type->ptr_to);
            if (base->kind == KIND_CHAR) {
                printf("cstring");
                break;
            }

            printf("^");
            print_odin_type(base, depth, false);
            break;
        }

        case KIND_FUNC: {
            printf("proc \"c\" (");
            for (size_t i = 0; i < type->func.param_count; i++) {
                if (i) printf(", ");

                printf("%s: ", type->func.param_list[i].name);
                print_odin_type(cuik_canonical_type(type->func.param_list[i].type), depth, false);
            }
            printf(") -> ");
            print_odin_type(cuik_canonical_type(type->func.return_type), depth, false);
            break;
        }

        default: printf("SOMETHING"); break;
    }
}

static bool is_in_sources(const Cuik_DriverArgs* args, const char* name) {
    dyn_array_for(i, args->sources) {
        if (strcmp(args->sources[i]->data, name) == 0) {
            return true;
        }
    }

    return false;
}

int run_bindgen(int argc, const char** argv) {
    cuik_init(true);

    Cuik_DriverArgs args = {
        .version = CUIK_VERSION_C23,
        .target = cuik_target_host(),
        .toolchain = cuik_toolchain_host(),
    };
    cuik_parse_driver_args(&args, argc, argv);

    // we just want to type check
    args.syntax_only = true;
    args.preserve_ast = true;

    // compile source files
    size_t obj_count = dyn_array_length(args.sources);
    Cuik_BuildStep** objs = cuik_malloc(obj_count * sizeof(Cuik_BuildStep*));
    dyn_array_for(i, args.sources) {
        objs[i] = cuik_driver_cc(&args, args.sources[i]->data);
    }

    // link (if no codegen is performed this doesn't *really* do much)
    Cuik_BuildStep* linked = cuik_driver_ld(&args, obj_count, objs);
    if (!cuik_step_run(linked, NULL)) {
        fprintf(stderr, "damn...\n");
        return 1;
    }

    CompilationUnit* cu = cuik_driver_ld_get_cu(linked);

    printf("package mylib\n\nimport \"core:c\"\n\n");
    CUIK_FOR_EACH_TU(tu, cu) {
        size_t stmt_count = cuik_num_of_top_level_stmts(tu);
        Stmt** stmts = cuik_get_top_level_stmts(tu);

        TokenStream* tokens = cuik_get_token_stream_from_tu(tu);

        printf("// TU %s\n", cuikpp_get_files(tokens)[0].filename);

        for (size_t i = 0; i < stmt_count; i++) {
            // only keep the declarations in the main file (or other files in this bindgen)
            ResolvedSourceLoc r = cuikpp_find_location(tokens, stmts[i]->loc.start);
            if (!is_in_sources(&args, r.file->filename)) {
                continue;
            }

            Atom name = stmts[i]->decl.name;
            Cuik_Type* type = cuik_canonical_type(stmts[i]->decl.type);

            if (stmts[i]->decl.attrs.is_typedef) {
                ptrdiff_t search = nl_map_get_cstr(already_defined, name);
                if (search < 0) {
                    nl_map_put_cstr(already_defined, name, 1);

                    ptrdiff_t search = nl_map_get(typedefs, type);
                    if (search < 0) {
                        // mark as defined
                        nl_map_put(typedefs, type, name);

                        // typedef
                        printf("%s :: ", name);
                        print_odin_type(type, 0, true);
                        printf("\n");
                    }
                } else {
                    nl_map_put(typedefs, type, name);
                }
            } else if (type->kind == KIND_FUNC) {
                // normal function
                ptrdiff_t search = nl_map_get_cstr(already_defined, name);
                if (search < 0) {
                    nl_map_put_cstr(already_defined, name, 1);

                    printf("%s :: ", name);
                    print_odin_type(type, 0, true);
                    printf("\n");
                }
            } else {
                // printf("TODO: globals!\n");
            }
        }

        nl_map_free(typedefs);
    }

    return 0;
}
