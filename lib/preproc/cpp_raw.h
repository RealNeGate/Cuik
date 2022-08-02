
CUIK_API TokenStream cuik_raw_tokens(const Cuik_IFileSystem* fs, const char filepath[FILENAME_MAX]) {
    if (fs == NULL) {
        fs = &cuik_default_fs;
    }

    Cuik_File file = CUIK_CALL(fs, get_file, false, filepath);
    if (!file.found) {
        panic("preprocessor error: could not read file! %s\n", filepath);
    }

    // convert all the weird whitespace into something normal
    unsigned char* text = (unsigned char*)file.data;
    remove_weird_whitespace(file.length, text);

    Lexer l = {filepath, text, text, 1};

    TokenStream s = { 0 };
    s.filepath = l.filepath;

    //int current_line_num = 0;
    //SourceLine* current_line = NULL;

    uint64_t timer_start = cuik_time_in_nanos();
    do {
        lexer_read(&l);

        // slap a source location
        /*ptrdiff_t columns = l.token_start - l.line_current;
        ptrdiff_t length = l.token_end - l.token_start;
        assert(columns <= UINT16_MAX && length <= UINT16_MAX);

        SourceLocIndex loc_index = arraddnindex(s.locations, 1);
        s.locations[loc_index] = (SourceLoc) {
            .line = current_line,
            .columns = columns,
            .length = length,
        };

        if (current_line_num != l.current_line) {
            // make a new source line... we'll miss our fallen brother, he's not
            // dead but for when he dies...
            current_line = arena_alloc(&thread_arena, sizeof(SourceLine), _Alignof(SourceLine));
            current_line->filepath = l.filepath;
            current_line->line_str = l.line_current;
            current_line->parent = 0;
            current_line->line = l.current_line;

            current_line_num = l.current_line;
        }

        // insert token
        Token t = {
            l.token_type, loc_index, l.token_start, l.token_end
        };

        // classify potential keywords
        if (t.type == TOKEN_IDENTIFIER) {
            t.type = classify_ident(&l.token_start, l.token_end - l.token_start);
        }*/

        //arrput(s.tokens, t);
    } while (l.token_type);

    uint64_t timer_end = cuik_time_in_nanos();
    printf("%s: Took %f ms\n", filepath, (timer_end - timer_start) / 1000000.0);

    //cuik_profile_region(timer_start, "nopp: %s", filepath);
    return s;
}
