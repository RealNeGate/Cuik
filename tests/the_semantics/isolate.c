
typedef struct {
	char data[7];
} Foo7;

void bar(Foo7* f) {
	*f = (Foo7){ { 69 } };
}
