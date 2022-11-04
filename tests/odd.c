#define MAIN ma##in

#ifndef FOO
int *something[5];
#endif

#if defined MAIN
int MAIN(int argc, const char *argv[]) {
    return foo + (argc * (char)4)[argv][0];
}
#endif

int foo = 1 * 5;
