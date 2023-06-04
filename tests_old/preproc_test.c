
#ifdef _WIN32

#ifndef _WIN32
int bar() { return 16; }
#else
int bar() { return 24; }
#endif

int foo() { return 16; }
#else
int foo() { return 24; }
#endif
