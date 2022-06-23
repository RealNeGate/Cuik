#include <cuik.h>
#include <cuik_ast.h>
#include <stdio.h>
#include "helper.h"

#define NL_STRING_MAP_IMPL
#define NL_STRING_MAP_INLINE
#include <string_map.h>

typedef struct {
    enum {
        SYM_EXTERN_FUNC,
        SYM_FUNC,

        SYM_TYPEDEF,

        SYM_GLOBAL,
    } type;

    // source location
    const char* filepath;
    int line;
} Symbol;

static NL_Strmap(Symbol) symbol_table = {0};

static void da_visitor(TranslationUnit* tu, Stmt* restrict s, void* user_data) {
    // if (!cuik_is_in_main_file(tu, s->loc)) return;
    if (!s->decl.attrs.is_used) return;

    if (s->op == STMT_FUNC_DECL || s->op == STMT_GLOBAL_DECL || s->op == STMT_DECL) {
        char* name = (char*)s->decl.name;

        if (name != NULL) {
            // find source location
            TokenStream* tokens = cuik_get_token_stream_from_tu(tu);
            const char* filepath = cuik_get_location_file(tokens, s->loc);
            int line = cuik_get_location_line(tokens, s->loc);

            Symbol sym = {
                .filepath = filepath, .line = line
            };

            if (s->decl.attrs.is_typedef) {
                sym.type = SYM_TYPEDEF;
            } else {
                if (s->decl.attrs.is_static || s->decl.attrs.is_inline) sym.type = SYM_FUNC;
                else sym.type = SYM_EXTERN_FUNC;
            }

            printf("%s\n", name);
            nl_strmap_put_cstr(symbol_table, name, sym);
        }
    }
}

static void print_raw_text_as_html(FILE* outfile, size_t length, const char* str) {
    for (size_t i = 0; i < length; i++) {
        if (str[i] == '<') fprintf(outfile, "&lt;");
        else if (str[i] == '>') fprintf(outfile, "&gt;");
        else fputc(str[i], outfile);
    }
}

int main(int argc, char** argv) {
    if (argc <= 1) {
        fprintf(stderr, "no input file!\n");
        return 1;
    }

    cuik_init();

    // find system libraries
    Cuik_SystemLibs* system_libs = find_system_libs();

    // preproc
    Cuik_CPP cpp;
    TokenStream tokens = cuik_preprocess_simple(&cpp, argv[1], system_libs, 2, (const char*[]) {
                                                    "include/",
                                                    "src/"
                                                });

    symbol_table = nl_strmap_alloc(Symbol, 1024);

    // parse
    TranslationUnit* tu = cuik_parse_translation_unit(NULL, &tokens, NULL);

    // print defines
    Cuik_DefineRef it, curr = cuikpp_first_define(&cpp);
    while (it = curr, cuikpp_next_define(&cpp, &curr)) {
        Cuik_Define def = cuikpp_get_define(&cpp, it);

        if (cuik_is_in_main_file(tu, def.loc)) {
            //printf("#define %.*s %.*s\n", (int)def.key.len, def.key.data, (int)def.value.len, def.value.data);
        }
    }
    cuikpp_finalize(&cpp);
    cuik_visit_top_level(tu, NULL, da_visitor);

    // print HTML page
    {
        /*NL_StrmapHeader* hdr = nl_strmap__get_header(symbol_table);
        for (size_t i = 0; i < hdr->size; i++) {
            if (hdr->keys[i].length == 0) {
                printf(" %3zu\t---\n", i);
            } else {
                printf(" %3zu\t%.*s\n", i, (int) hdr->keys[i].length, hdr->keys[i].data);
            }
        }*/

        FILE* outfile = fopen("W:/TestBed/Site/a.html", "wb");
        Cuik_FileEntry* main_file = &cuikpp_get_file_table(&cpp)[0];

        fprintf(outfile,
                "<html>\n"
                "  <head>\n"
                "    <meta charset=\"utf-8\">\n"
                "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
                "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">\n"
                "  </head>\n");
        fprintf(outfile, "  <body><pre>\n");
        const char* line_start = (const char*)main_file->content;
        int line_num = 0;
        for (;;) {
            const char* line_end = strchr(line_start, '\n');
            size_t line_length = line_end ? line_end - line_start : strlen(line_start);
            line_num += 1;

            //fprintf(outfile, "%3d  ", line_num);

            // spot any identifiers
            size_t token_start = SIZE_MAX, last_printed = 0, i = 0;
            while (i < line_length) {
                char ch = line_start[i];

                if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_') {
                    if (token_start == SIZE_MAX) token_start = i;
                } else if (token_start != i && (ch >= '0' && ch <= '9')) {
                    /* numbers are allowed so long as they aren't at the start of the identifier */
                } else {
                    if (token_start != SIZE_MAX) {
                        // end a token, check for matches
                        NL_Slice ident = { i - token_start, (const uint8_t*) &line_start[token_start] };
                        ptrdiff_t val = nl_strmap_get(symbol_table, ident);

                        if (val >= 0) {
                            if (last_printed != token_start) {
                                print_raw_text_as_html(outfile, token_start - last_printed, line_start + last_printed);
                            }

                            Symbol* sym = &symbol_table[val];
                            if (sym->filepath == main_file->filepath && sym->line == line_num) {
                                // found original definition
                                fprintf(outfile, "<a name=\"%.*s\"><span>%.*s</span></a>",
                                        (int)(i - token_start), line_start + token_start,
                                        (int)(i - token_start), line_start + token_start);
                            } else {
                                // found match, make a <a> block
                                fprintf(outfile, "<a href=\"#%.*s\">%.*s</a>",
                                        (int)(i - token_start), line_start + token_start,
                                        (int)(i - token_start), line_start + token_start);
                            }

                            last_printed = i;
                        } else if (cuik_lex_is_keyword(ident.length, (const char*) ident.data)) {
                            if (last_printed != token_start) {
                                print_raw_text_as_html(outfile, token_start - last_printed, line_start + last_printed);
                            }

                            fprintf(outfile, "<span class=\"kw\">%.*s</span>",
                                    (int)(i - token_start), line_start + token_start);

                            last_printed = i;
                        }

                        token_start = SIZE_MAX;
                    }
                }

                i += 1;
            }

            // print any leftovers
            if (last_printed != line_length) {
                print_raw_text_as_html(outfile, line_length - last_printed, line_start + last_printed);
            }

            fprintf(outfile, "\n");

            // last line
            if (line_end == NULL) break;
            line_start = line_end + 1;
        }
        fprintf(outfile, "  </pre></body>\n</html>\n");
    }

    cuik_destroy_translation_unit(tu);
    cuikpp_deinit(&cpp);
    return 0;
}
