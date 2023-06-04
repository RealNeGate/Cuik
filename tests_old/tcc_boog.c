// This created a weird infinite token stream (the linked list wrapped around)
#define DEF_ASM_OP1(name) TOK_ASM_ ## name PEAR
#define TOK_ASM_foo TOK_INT

DEF_ASM_OP1(foo)

// concatting two identifiers into a macro name and then
// having a token at the end of that expansion will cause
// an infinite token stream
