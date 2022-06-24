// https://www.duskborn.com/posts/simple-c-command-line-parser
#ifndef OPTION
#error "Must define OPTION macro"
#endif

OPTION(HELP,    h, help,        0, "print all the options and exit")
OPTION(OUT,     o, output,      1, "set the output path")
OPTION(INCLUDE, I, include,     1, "add include directory")
OPTION(PREPROC, P, preprocess,  0, "preprocess file and output to stdout")
OPTION(RUN,     r, run,         0, "execute compiled program")
OPTION(LIB,     l, lib,         1, "add library to compilation unit")
OPTION(TIME,    T, time,        0, "profile the compile times")
OPTION(OBJ,     c, obj,         0, "dont link, only emit the object file")
OPTION(OPT,     O, optimize,    0, "optimize the generated IR")
OPTION(ASM,     S, assembly,    0, "emit assembly in stdout")
OPTION(IR,      _, ir,          0, "compile up until the IR generation")

#undef OPTION
