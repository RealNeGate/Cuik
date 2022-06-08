#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

char *w,*c,t[] = "1 2 3 + * .";
int s[1024],i=0,a,b;

#define X(x)   ((x)=s[--i])
#define Y      (X(a),X(b))
#define Z(x)   (s[i++]=(x))
#define W(o)   (Y,Z(a o b))
#define V(o)   C(o)?W(o):
#define P      printf
#define S      strcmp
#define A      atoi
#define B      abort
#define D(x)   isdigit(*x)
#define C(x)   !S(w,#x)
#define T(x)   strtok_r(x," ",&c)

int main() {
    w=T(t);
    do V(+)V(-)V(*)V(/)
        C(.)?X(a),P("%d\n", a):
        C(swap)?Y,Z(a),Z(b):
        D(w)?Z(A(w)):B();
    while ((w=T(NULL)));

    return 0;
}
