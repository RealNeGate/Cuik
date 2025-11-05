static const char keywords[][16] = {
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Embed",
    "_Generic",
    "_Imaginary",
    "_Pragma",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
    "_Typeof",
    "_Vector",
    "__asm__",
    "__attribute__",
    "__cdecl",
    "__stdcall",
    "__declspec",
    "discard",
    "layout",
    "in",
    "out",
    "inout",
    "uint",
    "buffer",
    "uniform",
    "flat",
    "smooth",
    "noperspective",
    "vec2",
    "vec3",
    "vec4",
    "ivec2",
    "ivec3",
    "ivec4",
    "uvec2",
    "uvec3",
    "uvec4",
    "dvec2",
    "dvec3",
    "dvec4",
};

static const uint8_t eq_classes[256] = {
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0x00
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0x10
    0 , 14, 4 , 7 , 17, 14, 10, 4 , 5 , 5 , 14, 8 , 5 , 11, 13, 14, // 0x20
    2 , 1 , 19, 19, 19, 19, 19, 19, 20, 20, 5 , 5 , 15, 6 , 12, 5 , // 0x30
    5 , 18, 3 , 18, 18, 18, 18, 17, 17, 17, 17, 17, 16, 17, 17, 17, // 0x40
    17, 17, 17, 17, 17, 17, 17, 17, 18, 17, 17, 5 , 0 , 5 , 14, 17, // 0x50
    0 , 18, 3 , 18, 18, 18, 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, // 0x60
    17, 17, 17, 17, 17, 17, 17, 17, 18, 17, 17, 5 , 9 , 5 , 5 , 0 , // 0x70
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0x80
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0x90
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0xa0
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0xb0
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0xc0
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0xd0
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0xe0
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , // 0xf0
};

static const uint8_t dfa[22][20] = {
    [ 1] = { 15,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0, 13, 12, 13, 14, 15,  0,  0,  5,  0 },
    [ 2] = { 11,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0, 13, 12, 13, 14, 15,  0,  0,  5,  0 },
    [ 3] = {  5,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0, 14, 12,  0,  0,  0,  0,  0,  5,  0 },
    [ 4] = {  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  3,  0 },
    [ 5] = {  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 6] = {  1,  2,  0,  0,  0,  0,  2,  2,  2,  0,  0,  0,  0,  0,  0,  0,  2,  2,  0,  2 },
    [ 7] = {  4,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 8] = {  7,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 9] = { 19,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2 },
    [10] = {  6,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [11] = {  8,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [12] = { 17,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0 },
    [13] = {  9,  0,  0,  0,  0,  0,  0,  0,  0, 10,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [14] = {  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [15] = { 16,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0 },
    [16] = { 18,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  5,  0 },
    [17] = {  5,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  5,  0 },
    [18] = {  5,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0, 12, 12,  0,  0,  0,  0,  0,  5,  0 },
    [19] = { 15,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0, 13, 12, 13,  0, 15,  0,  0,  5,  0 },
    [20] = { 15,  0,  0,  0,  0,  5,  0,  0,  0,  0,  0,  0, 12,  0,  0, 15,  0,  0,  5,  0 },
    [21] = {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
};

enum { L_ERROR, L_SIGIL, L_QUOTE, L_IDENT, L_NUMBER };
static const uint8_t lexer_final_state[20] = {
    [1] = L_SIGIL,
    [2] = L_SIGIL,
    [3] = L_QUOTE,
    [4] = L_SIGIL,
    [5] = L_IDENT,
    [6] = L_SIGIL,
    [7] = L_SIGIL,
    [8] = L_SIGIL,
    [9] = L_SIGIL,
    [10] = L_SIGIL,
    [11] = L_NUMBER,
    [12] = L_NUMBER,
    [13] = L_NUMBER,
    [14] = L_NUMBER,
    [15] = L_NUMBER,
    [16] = L_SIGIL,
    [17] = L_SIGIL,
    [19] = L_SIGIL,
};

static bool lexer_is_space(int ch) {
    uint64_t mask = (1ull << ' ') | (1ull << '\t') | (1ull << '\v') | (1ull << '\r') | (1ull << '\n');
    return ch < 64 ? (mask >> ch) & 1 : false;
}
Token lexer_read(Lexer* restrict l) {
    unsigned char* current = l->current;
    Token t = { 0 };
    t.has_space = *current == ' ';
    // phase 1: skip non-token space
    //   branchless space skip
    current += (*current == ' ');
    //   non-token DFA
    retry: {
        // skip whitespace
        while (lexer_is_space(*current)) {
            t.hit_line |= *current++ == '\n';
        }
        // check for comments
        if (current[0] == '/' && current[1] == '/') {
            current += 2;
            // skip until whitespace
            while (*current && *current != '\n') {
                current++;
            }
            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
            t.hit_line = true;
            goto retry;
        } else if (current[0] == '/' && current[1] == '*') {
            current += 2;
            // skip until comment end
            while (current[0] && !(current[0] == '*' && current[1] == '/')) {
                t.hit_line |= *current++ == '\n';
            }
            current += 2;
            goto retry;
        } else if (current[0] == '\\' && (current[1] == '\r' || current[1] == '\n')) {
            current += 1;
            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
            goto retry;
        }
    }
    unsigned char* start = current;
    unsigned char first  = *start;
    if (__builtin_expect(first == '\0', 0)) {
        return (Token){ 0 };
    }
    current++;
    // eval first char
    uint8_t eq_class = eq_classes[first];
    uint64_t state   = dfa[eq_class][0];
    if (state) {
        // eval rest
        for (;;) {
            uint8_t ch = *current;
            // read convert to class (compresses the DFA a lot)
            uint8_t eq_class = eq_classes[ch];
            // eval DFA
            uint64_t next = dfa[eq_class][state];
            if (next == 0) break;
            state = next, current += 1;
        }
    }
    uint64_t tag = lexer_final_state[state];
    if (tag == L_IDENT) {
        t.atom = atoms_put(current - start, start);
        t.type = TOKEN_IDENTIFIER;
    } else if (tag == L_NUMBER) {
        t.atom = atoms_put(current - start, start);
        t.type = TOKEN_INTEGER;
    } else if (tag == L_QUOTE) {
        char quote_type = current[-1];
        for (; *current && *current != quote_type; current++) {
            // skip escape codes
            if (*current == '\\') {
                // this will skip twice because of the for loop's next
                //  \  "  . . .
                //  ^     ^
                //  old   new
                current += 1;
            }
        }
        t.type = quote_type;
        if (start[0] == 'L') {
            t.type += 256;
            start += 1;
        }
        t.atom = atoms_put(current - (start + 1), start + 1);
        current += 1;
    } else {
        // these tokens have their contents embedded into the
        // Atom pointer.
        int length = current - start;
        assert(length <= 3);
        uint32_t mask = UINT32_MAX >> ((4 - length) * 8);
        // potentially unaligned access :P
        uint32_t chars;
        memcpy(&chars, start, sizeof(uint32_t));
        // t.atom = (Atom) ((uintptr_t) (chars & mask) | (tag << 56ull));
        t.atom = atoms_put(current - start, start);
        t.type = chars & mask;
    }
    // NOTE(NeGate): the lexer will modify code to allow for certain patterns
    // if we wanna get rid of this we should make virtual code regions
    if (__builtin_expect(current[0] == '\\' && (current[1] == '\r' || current[1] == '\n'), 0)) {
        __debugbreak();
    }
    l->current = current;
    t.location = encode_file_loc(l->file_id, start - l->start);
    return t;
}
