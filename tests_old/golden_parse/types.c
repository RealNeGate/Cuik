typedef int X;
struct Y { short X; };

typedef struct foo { int x; } foo;
void test() {
   foo *foo;
   foo->x = 0;
}
