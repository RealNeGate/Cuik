#define MAIN ma##in

#ifndef FOO
int *something[5];
#endif

#if defined MAIN
int MAIN(int argc, const char *argv[]) {
    return (argc * (char)4)[argv];
}
#endif
