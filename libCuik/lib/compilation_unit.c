#include "compilation_unit.h"
#include "targets/targets.h"

void cuik_create_compilation_unit(CompilationUnit* restrict cu) {
    *cu = (CompilationUnit){0};
    cu->lock = malloc(sizeof(mtx_t));
    mtx_init((mtx_t*) cu->lock, mtx_plain);
}

void cuik_lock_compilation_unit(CompilationUnit* restrict cu) {
    mtx_lock((mtx_t*) cu->lock);
}

void cuik_unlock_compilation_unit(CompilationUnit* restrict cu) {
    mtx_unlock((mtx_t*) cu->lock);
}

void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu) {
    assert(tu->next == NULL && "somehow the TU is already attached to something...");
    cuik_lock_compilation_unit(cu);

    tu->parent = cu;

    if (cu->tail == NULL) cu->head = tu;
    else cu->tail->next = tu;
    cu->tail = tu;
    cu->count += 1;

    cuik_unlock_compilation_unit(cu);
}

void cuik_destroy_compilation_unit(CompilationUnit* restrict cu) {
    // walk all the TUs and free them (if they're not freed already)
    TranslationUnit* tu = cu->head;
    while (tu != NULL) {
        TranslationUnit* next = tu->next;
        cuik_destroy_translation_unit(tu);
        tu = next;
    }

    mtx_destroy((mtx_t*) cu->lock);
    free(cu->lock);
    *cu = (CompilationUnit){0};
}

size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu) {
    return cu->count;
}

#if CUIK_USE_TB
void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu, TB_Module* mod)
#else
void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu, void* mod)
#endif
{
    FOR_EACH_TU(tu, cu) {
        size_t count = dyn_array_length(tu->top_level_stmts);
        tu->ir_mod = mod;
        // printf("%s:\n", cuikpp_get_main_file(&tu->tokens));

        for (size_t i = 0; i < count; i++) {
            Stmt* s = tu->top_level_stmts[i];
            const char* name = s->decl.name;

            if (s->op == STMT_FUNC_DECL) {
                if (!s->decl.attrs.is_static && !s->decl.attrs.is_inline) {
                    // printf("  Func Export! %s\n", s->decl.name);
                    nl_strmap_put_cstr(cu->export_table, s->decl.name, s);
                }

                if (s->decl.attrs.is_static || s->decl.attrs.is_inline) {
                    if (!s->decl.attrs.is_used) continue;
                }

                #if CUIK_USE_TB
                TB_FunctionPrototype* proto = tu->target->create_prototype(tu, cuik_canonical_type(s->decl.type));
                TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;

                // TODO(NeGate): Fix this up because it's possibly wrong, essentially
                // inline linkage means all the definitions must match which isn't
                // necessarily the same as static where they all can share a name but
                // are different and internal.
                TB_Function* func;
                if (s->decl.attrs.is_inline) {
                    linkage = TB_LINKAGE_PRIVATE;

                    char temp[1024];
                    snprintf(temp, 1024, "_K%d_%s", tu->id_gen++, name ? name : "<unnamed>");

                    func = tb_function_create(tu->ir_mod, temp, linkage);
                } else {
                    func = tb_function_create(tu->ir_mod, name, linkage);
                }
                tb_function_set_prototype(func, proto);
                s->backing.f = func;
                #endif
            } else if (s->op == STMT_GLOBAL_DECL || s->op == STMT_DECL) {
                if (!s->decl.attrs.is_static  && !s->decl.attrs.is_extern &&
                    !s->decl.attrs.is_typedef && !s->decl.attrs.is_inline &&
                    s->decl.name && cuik_canonical_type(s->decl.type)->kind != KIND_FUNC) {
                    // printf("  Global Export! %s\n", s->decl.name);
                    nl_strmap_put_cstr(cu->export_table, s->decl.name, s);
                }

                Cuik_Type* type = cuik_canonical_type(s->decl.type);
                bool is_external_sym = (type->kind == KIND_FUNC && s->decl.initial_as_stmt == NULL);

                #ifdef CUIK_USE_TB
                if (s->decl.attrs.is_extern && is_external_sym && mod != NULL) {
                    // if we have a TB module, fill it up with declarations
                    if (s->decl.attrs.is_tls && !atomic_flag_test_and_set(&irgen_defined_tls_index)) {
                        tb_module_set_tls_index(tu->ir_mod, (TB_Symbol*) tb_extern_create(tu->ir_mod, "_tls_index", TB_EXTERNAL_SO_LOCAL));
                    }

                    TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
                    s->backing.g = tb_global_create(tu->ir_mod, name, s->decl.attrs.is_tls ? TB_STORAGE_TLS : TB_STORAGE_DATA, linkage);
                }
                #endif
            }
        }
    }
}
