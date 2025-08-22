
/*#define     hash_hash # ## #
#define     mkstr(a) # a
#define     in_between(a) mkstr(a)
#define     join(c, d) in_between(c hash_hash d)
char p[] = join(x, y);
// equivalent to char p[] = "x ## y";

#define APPLE(x, y) (x + y)

int main() {
    return APPLE(2, 4);
}*/

#ifndef APPLE
#define    x          3
#define    f(a)       f(x * (a))
#undef     x
#define    x          2
#define    g          f
#define    z          z[0]
#define    h          g(~
#define    m(a)       a(w)
#define    w          0,1
#define    t(a)       a
#define    p()        int
#define    q(x)       x
#define    r(x,y)     x ## y
#define    str(x)     # x
f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
g(x+(3,4)-w) | h 5) & m
(f)^m(m);
p() i[q()] = { q(1), r(2,3), r(4,), r(,5), r(,) };
char c[2][6] = { str(hello), str() };
#else
#define str(s)      # s
#define xstr(s)     str(s)
#define debug(s, t) printf("x" # s "= %d, x" # t "= %s", \
    x ## s, x ## t)
#define INCFILE(n) vers ## n
#define glue(a, b) a ## b
#define xglue(a, b) glue(a, b)
#define HIGHLOW     "hello"
#define LOW         LOW ", world"
debug(1, 2);
fputs(str(strncmp("abc\0d", "abc", '\4') // this goes away
        == 0) str(: @\n), s);
#include xstr(INCFILE(2).h)
glue(HIGH, LOW);
xglue(HIGH, LOW)
#endif

