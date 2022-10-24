////////////////////////////////
// Preprocessor REPL
////////////////////////////////
// This is a simple subtool of the main_driver for debugging macros.
// For the most part it can just load a file and introspect using a few comamnds:
//
//   find <filename> - locates the full path of a potential include name
//
//   view <macro>    - displays the contents of a macro along with telling you where
//                     it's located.
//
//   loc             - counts the number of preprocessed lines of code
//
static Cuik_CPP* make_preprocessor(const char* filepath, bool should_finalize);
static void free_preprocessor(Cuik_CPP* cpp);

// FIXME(NeGate): this is from str.c, we shouldn't be accessing it like this tho.
// maybe we should expose the libCuik string utilities correctly.
bool string_equals_cstr(const String* a, const char* b);

static bool strprefix(const char* str, const char* pre) {
    return strncmp(pre, str, strlen(pre)) == 0;
}

static int count_pp_lines(TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuikpp_get_tokens(s);
    size_t count = cuikpp_get_token_count(s);

    int line_count = 0;
    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];
        ResolvedSourceLoc r = cuikpp_find_location(s, t->location);

        if (last_file != r.file->filename || last_line != r.line) {
            line_count += 1;

            last_file = r.file->filename;
            last_line = r.line;
        }
    }

    return line_count;
}

static int pp_repl(void) {
    if (dyn_array_length(input_files) > 1) {
        printf("warning: pprepl doesn't support loading multiple files... yet\n");
    }

    fprintf(stderr, "loading %s\n", input_files[0]);

    Cuik_CPP* cpp = make_preprocessor(input_files[0], false);
    TokenStream* s = cuikpp_get_token_stream(cpp);
    char line[255];
    for (;;) {
        printf("> ");

        // read
        while (fgets(line, 255, stdin) == NULL) {}

        // eval
        uint64_t start = cuik_time_in_nanos();
        if (strprefix(line, "quit")) {
            break;
        } else if (strprefix(line, "reload")) {
            free_preprocessor(cpp);
            cpp = make_preprocessor(input_files[0], false);
            s = cuikpp_get_token_stream(cpp);
        } else if (strprefix(line, "loc")) {
            printf("%d (%zu tokens)\n", count_pp_lines(s), cuikpp_get_token_count(s));
        } else if (strprefix(line, "syms")) {
            CUIKPP_FOR_DEFINES(it, cpp) {
                if (it.value.length) {
                    printf("#define %.*s %.*s\n", (int) it.key.length, it.key.data, (int) it.value.length, it.value.data);
                } else {
                    printf("#define %.*s\n", (int) it.key.length, it.key.data);
                }
            }
        } else if (strprefix(line, "view ")) {
            // swap the newline with a nul terminator
            line[strlen(line) - 1] = 0;
            printf("Viewing... %s\n", line + sizeof("view"));

            Cuik_DefineIter it;
            if (cuikpp_find_define_cstr(cpp, &it, line + sizeof("view"))) {
                if (it.value.length) {
                    printf("#define %.*s %.*s\n", (int) it.key.length, it.key.data, (int) it.value.length, it.value.data);
                } else {
                    printf("%.*s is empty\n", (int) it.key.length, it.key.data);
                }
            } else {
                printf("could not find '%s'\n", line + sizeof("view"));
            }
        }
        uint64_t end = cuik_time_in_nanos();
        printf("  Took %f ms\n", (end - start) / 1000000.0);
    }

    free_preprocessor(cpp);
    return EXIT_SUCCESS;
}
