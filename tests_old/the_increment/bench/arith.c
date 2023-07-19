struct RT {
	char A;
	int B[10][20];
	char C;
};
struct ST {
	int X;
	double Y;
	struct RT Z;
};

int *foo(struct ST *s) {
	return &s[1].Z.B[5][13];
}

int* bar(int* a, int b) {
	return &a[b];
}
