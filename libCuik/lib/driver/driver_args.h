X(HELP,        "h",        NULL,      false, "print help")
X(VERBOSE,     "V",        NULL,      false, "print verbose messages")
X(LIVE,        "live",     NULL,      false, "runs compiler persistently, repeats compile step whenever the input changes")
X(BAKE,        "bake",     NULL,      false, "use libCuik to write a C compile option")
// preprocessor
X(DEFINE,      "D",        "define",  true,  "defines a macro before compiling")
X(UNDEF,       "U",        "undef",   true,  "undefines a macro before compiling")
X(INCLUDE,     "I",        "include", true,  "add directory to the include searches")
X(PPTEST,      "Pp",       NULL,      false, "test preprocessor")
X(PP,          "P",        NULL,      false, "print preprocessor output to stdout")
X(PPREPL,      "xp",       NULL,      false, "enables the preprocessor REPL environment")
// parser
X(LANG,        "lang",     NULL,      false, "choose the language (c11, c23, glsl)")
X(AST,         "ast",      NULL,      false, "print AST into stdout")
X(SYNTAX,      "xe",       NULL,      false, "type check only")
// optimizer
X(OPTLVL,      "O",        NULL,      true,  "no optimizations")
// backend
X(EMITIR,      "emit-ir",  NULL,      false, "print IR into stdout")
X(OUTPUT,      "o",        NULL,      true,  "set the output filepath")
X(OBJECT,      "c",        NULL,      false, "output object file")
X(ASSEMBLY,    "S",        NULL,      false, "output assembly to stdout (not ready)")
X(DEBUG,       "g",        NULL,      false, "compile with debug information")
// linker
X(NOLIBC,      "nostdlib", NULL,      false, "don't include and link against the default CRT")
X(LIB,         "l",        NULL,      true,  "add library name to the linking")
X(BASED,       "based",    NULL,      false, "use the TB linker (EXPERIMENTAL)")
// misc
X(TARGET,      "target",   NULL,      true,  "change the target system and arch")
X(THREADS,     "j",        "threads", false, "enabled multithreaded compilation")
X(TIME,        "T",        NULL,      false, "profile the compile times")
X(THINK,       "think",    NULL,      false, "aids in thinking about serious problems")
// run
X(RUN,         "r",        NULL,      false, "run the executable")
#undef X
