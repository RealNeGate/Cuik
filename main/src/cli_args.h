X(ARG_HELP,    "h",        NULL,      false, "print help")
X(ARG_VERBOSE, "V",        NULL,      false, "print verbose messages")
X(ARG_LIVE,    "live",     NULL,      false, "runs compiler persistently, repeats compile step whenever the input changes")
// preprocessor
X(ARG_DEFINE,  "D",        "define",  true,  "defines a macro before compiling")
X(ARG_UNDEF,   "U",        "undef",   true,  "undefines a macro before compiling")
X(ARG_INCLUDE, "I",        "include", true,  "add directory to the include searches")
X(ARG_PPTEST,  "Pp",       NULL,      false, "test preprocessor")
X(ARG_PREPROC, "P",        NULL,      false, "print preprocessor output to stdout")
X(ARG_PPREPL,  "xp",       NULL,      false, "enables the preprocessor REPL environment")
// parser
X(ARG_LANG,    "lang",     NULL,      false, "choose the language (c11, c23, glsl)")
X(ARG_AST,     "ast",      NULL,      false, "print AST into stdout")
X(ARG_SYNTAX,  "xe",       NULL,      false, "type check only")
// optimizer
X(ARG_O0,      "O0",       NULL,      false, "no optimizations")
X(ARG_O1,      "O1",       NULL,      false, "non-aggresive optimizations")
// backend
X(ARG_EMITIR,  "emit-ir",  NULL,      false, "print IR into stdout")
X(ARG_OUTPUT,  "o",        NULL,      true,  "set the output filepath")
X(ARG_OBJECT,  "c",        NULL,      false, "output object file")
X(ARG_ASSEMBLY,"S",        NULL,      false, "output assembly to stdout (not ready)")
X(ARG_DEBUG,   "g",        NULL,      false, "compile with debug information")
// linker
X(ARG_NOLIBC,  "nostdlib", NULL,      false, "don't include and link against the default CRT")
X(ARG_LIB,     "l",        NULL,      true,  "add library name to the linking")
X(ARG_BASED,   "based",    NULL,      false, "use the TB linker (EXPERIMENTAL)")
// misc
X(ARG_TARGET,  "target",   NULL,      true,  "change the target system and arch")
X(ARG_THREADS, "j",        "threads", false, "enabled multithreaded compilation")
X(ARG_TIME,    "T",        NULL,      false, "profile the compile times")
X(ARG_THINK,   "think",    NULL,      false, "aids in thinking about serious problems")
// run
X(ARG_RUN,     "r",        NULL,      false, "run the executable")
#undef X
