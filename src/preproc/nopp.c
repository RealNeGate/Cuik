#include "nopp.h"

TokenStream nopp_invoke(Lexer* restrict l) {
    if (l->line_current == NULL) {
        l->line_current = l->start;
    }

    TokenStream s = { 0 };
    s.filepath = l->filepath;

    lexer_read(l);

    int current_line_num = 0;
    SourceLine* current_line = NULL;

    while (l->token_type) {
        // slap a source location
        ptrdiff_t columns = l->token_start - l->line_current;
        ptrdiff_t length = l->token_end - l->token_start;
        assert(columns <= UINT16_MAX && length <= UINT16_MAX);

        SourceLocIndex loc_index = arraddnindex(s.locations, 1);
        s.locations[loc_index] = (SourceLoc) {
            .line = current_line,
            .columns = columns,
            .length = length,
        };

        if (current_line_num != l->current_line) {
            // make a new source line... we'll miss our fallen brother, he's not
            // dead but for when he dies...
            current_line = arena_alloc(&thread_arena, sizeof(SourceLine), _Alignof(SourceLine));
            current_line->filepath = l->filepath;
            current_line->line_str = l->line_current;
            current_line->parent = 0;
            current_line->line = l->current_line;

            current_line_num = l->current_line;
        }

        // insert token
        Token t = {
            l->token_type, loc_index, l->token_start, l->token_end
        };

        // classify potential keywords
        if (t.type == TOKEN_IDENTIFIER) {
            t.type = classify_ident(l->token_start, l->token_end - l->token_start);
        }

        arrput(s.tokens, t);
        lexer_read(l);
    }

    return s;
}
