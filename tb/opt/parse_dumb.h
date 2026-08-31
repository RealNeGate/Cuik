#if 0
typedef struct {
    const char* start;
    const char* curr;
} TB_DumbParser;

static bool dumb_num(char ch)    { return ch >= '0' && ch <= '9'; }
static bool dumb_ident0(char ch) { return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' || ch == '%'; }
static bool dumb_ident(char ch)  { return dumb_ident0(ch) || dumb_num(ch); }

static void dumb_skip_ws(TB_DumbParser* parser) {
    while (*parser->curr == ' ' || *parser->curr == '\t') {
        parser->curr++;
    }
}

static bool dumb_read_ident(TB_DumbParser* parser, TB_Slice* out_slice) {
    dumb_skip_ws(parser);
    if (!dumb_ident0(*parser->curr)) { return false; }
    const char* start = parser->curr;
    do {
        parser->curr += 1;
    } while (dumb_ident(*parser->curr));
    *out_slice = (TB_Slice){ (const char*) start, parser->curr - start };
    return true;
}

void tb_load_dump(TB_Function* f, const char* src) {
    TB_DumbParser parser = { src, src };

    while (*source) {
        TB_Slice dst;
        if (!dumb_read_ident(, &dst)) {

        }

        dumb_skip_ws();
        if (parser)

            // Find line bounds
            const char* end = source;
        while (*end != 0 && *end != '\n') { end++; }
    }
}
#endif
