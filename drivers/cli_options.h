// https://www.duskborn.com/posts/simple-c-command-line-parser
#ifndef OPTION
#error "Must define OPTION macro"
#endif

OPTION(h, help,   0, "print all the options and exit")
OPTION(o, output, 1, "set the output path")

#undef OPTION
