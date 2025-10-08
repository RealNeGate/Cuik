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

t(t(g)(0) + t)(1);
