
typedef struct {
    int x, y;
} Point;

Point add(Point a, Point b) {
    return (Point){ a.x + _.x, a.y + b.y };
}

int foo() { return _; }

#define sub(x, y) (x - y)
char* sigma(int* x, int* y) {
    return sub(x, y);
}
