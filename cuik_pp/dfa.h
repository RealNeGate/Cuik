#define PERFECT_HASH_SEED UINT32_C(166379777)
static const uint8_t keywords_table[256] = {
    [147] = 0, // auto
    [100] = 1, // break
    [41] = 2, // case
    [76] = 3, // char
    [168] = 4, // const
    [135] = 5, // continue
    [151] = 6, // default
    [165] = 7, // do
    [109] = 8, // double
    [124] = 9, // else
    [20] = 10, // enum
    [140] = 11, // extern
    [107] = 12, // float
    [85] = 13, // for
    [22] = 14, // goto
    [11] = 15, // if
    [139] = 16, // inline
    [38] = 17, // int
    [99] = 18, // long
    [9] = 19, // register
    [113] = 20, // restrict
    [167] = 21, // return
    [83] = 22, // short
    [144] = 23, // signed
    [106] = 24, // sizeof
    [104] = 25, // static
    [160] = 26, // struct
    [82] = 27, // switch
    [141] = 28, // typedef
    [150] = 29, // union
    [1] = 30, // unsigned
    [111] = 31, // void
    [81] = 32, // volatile
    [138] = 33, // while
    [92] = 34, // _Alignas
    [164] = 35, // _Alignof
    [105] = 36, // _Atomic
    [58] = 37, // _Bool
    [126] = 38, // _Complex
    [62] = 39, // _Embed
    [26] = 40, // _Generic
    [40] = 41, // _Imaginary
    [175] = 42, // _Pragma
    [154] = 43, // _Noreturn
    [59] = 44, // _Static_assert
    [90] = 45, // _Thread_local
    [14] = 46, // _Typeof
    [60] = 47, // _Vector
    [117] = 48, // __asm__
    [73] = 49, // __attribute__
    [45] = 50, // __cdecl
    [146] = 51, // __stdcall
    [18] = 52, // __declspec
    [70] = 53, // discard
    [61] = 54, // layout
    [51] = 55, // in
    [157] = 56, // out
    [132] = 57, // inout
    [72] = 58, // uint
    [108] = 59, // buffer
    [97] = 60, // uniform
    [65] = 61, // flat
    [28] = 62, // smooth
    [129] = 63, // noperspective
    [16] = 64, // vec2
    [69] = 65, // vec3
    [122] = 66, // vec4
    [63] = 67, // ivec2
    [77] = 68, // ivec3
    [91] = 69, // ivec4
    [52] = 70, // uvec2
    [66] = 71, // uvec3
    [80] = 72, // uvec4
    [172] = 73, // dvec2
    [7] = 74, // dvec3
    [21] = 75, // dvec4
};

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
    [34] = 19,
    [35] = 3,
    [36] = 18,
    [38] = 14,
    [39] = 19,
    [40 ... 41] = 2,
    [42] = 11,
    [43] = 12,
    [44] = 2,
    [45] = 15,
    [46] = 8,
    [48 ... 57] = 20,
    [58 ... 59] = 2,
    [60] = 16,
    [61] = 3,
    [62] = 17,
    [63 ... 64] = 2,
    [65 ... 90] = 18,
    [91] = 2,
    [93] = 2,
    [94] = 10,
    [95] = 18,
    [97 ... 122] = 18,
    [123] = 2,
    [124] = 13,
    [125 ... 126] = 2,
    [192 ... 255] = 18,
};

static const uint64_t dfa[21] = {
    [1] = (6ull << 12ull) | (12ull << 18ull) | (6ull << 30ull) | (6ull << 36ull) | (24ull << 42ull) | (48ull << 48ull) | (54ull << 54ull), // EQ2
    [2] = (6ull << 0ull), // EQ3
    [3] = (12ull << 0ull) | (6ull << 24ull) | (6ull << 30ull) | (6ull << 36ull) | (6ull << 42ull), // EQ4
    [8] = (18ull << 0ull), // EQ9
    [9] = (24ull << 0ull), // EQ10
    [10] = (24ull << 0ull), // EQ11
    [11] = (24ull << 0ull), // EQ12
    [12] = (30ull << 0ull), // EQ13
    [13] = (30ull << 0ull), // EQ14
    [14] = (30ull << 0ull), // EQ15
    [15] = (36ull << 0ull), // EQ16
    [16] = (42ull << 0ull), // EQ17
    [17] = (42ull << 0ull) | (6ull << 36ull), // EQ18
    [18] = (48ull << 0ull) | (48ull << 48ull) | (54ull << 54ull), // EQ19
    [19] = (6ull << 0ull), // EQ20
    [20] = (54ull << 0ull) | (48ull << 48ull) | (54ull << 54ull), // EQ21
};