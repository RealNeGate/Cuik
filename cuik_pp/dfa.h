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
    [33] = 9,
    [34] = 21,
    [35] = 3,
    [36] = 20,
    [37] = 13,
    [38] = 16,
    [39] = 21,
    [40 ... 41] = 2,
    [42] = 11,
    [43] = 14,
    [44] = 2,
    [45] = 17,
    [46] = 8,
    [47] = 12,
    [48 ... 57] = 22,
    [58 ... 59] = 2,
    [60] = 18,
    [61] = 3,
    [62] = 19,
    [63 ... 64] = 2,
    [65 ... 90] = 20,
    [91] = 2,
    [93] = 2,
    [94] = 10,
    [95] = 20,
    [97 ... 122] = 20,
    [123] = 2,
    [124] = 15,
    [125 ... 126] = 2,
    [192 ... 255] = 20,
};

static const uint8_t dfa[23][10] = {
    [ 0] = {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 1] = {  0,  0,  1,  2,  0,  1,  1,  4,  8,  9 },
    [ 2] = {  1,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 3] = {  2,  0,  0,  0,  1,  1,  1,  1,  0,  0 },
    [ 4] = {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 5] = {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 6] = {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 7] = {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 8] = {  3,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [ 9] = {  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [10] = {  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [11] = {  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [12] = {  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [13] = {  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [14] = {  5,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [15] = {  5,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [16] = {  5,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [17] = {  6,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [18] = {  7,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [19] = {  7,  0,  0,  0,  0,  0,  1,  0,  0,  0 },
    [20] = {  8,  0,  0,  0,  0,  0,  0,  0,  8,  9 },
    [21] = {  1,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
    [22] = {  9,  0,  0,  0,  0,  0,  0,  0,  8,  9 },
};

static const uint8_t lexer_final_state[10] = {
};

static bool lexer_is_space(int ch) {
    uint64_t mask = (1ull << ' ') | (1ull << '\t') | (1ull << '\v') | (1ull << '\r') | (1ull << '\n') | (1ull << '/');
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
            t.hit_line = *current++ == '\n';
        }
        // check for comments
        if (current[0] == '/' && current[1] == '/') {
            __debugbreak();
        } else if (current[0] == '/' && current[1] == '*') {
            __debugbreak();
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
            if (ch == first) { eq_class = 1; }
            // eval DFA
            uint64_t next = dfa[eq_class][state];
            if (next == 0) break;
            state = next, current += 1;
        }
    }
    uint64_t tag = lexer_final_state[state];
    if (tag == 0) {
        // these tokens are gonna get converted to real atoms
        t.atom = atoms_put(current - start, start);
    } else {
        // these tokens have their contents embedded into the
        // Atom pointer.
        int length = current - start;
        assert(length <= 3);
        uint32_t mask = UINT32_MAX >> ((4 - length) * 8);
        // potentially unaligned access :P
        uint32_t chars;
        memcpy(&chars, start, sizeof(uint32_t));
        t.atom = (Atom) ((uintptr_t) (chars & mask) | (tag << 56ull));
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
