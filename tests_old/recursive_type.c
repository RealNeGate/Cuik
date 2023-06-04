
struct Object {
	struct Object parent;
	int value;
};

int object_size(struct Object* o) {
	return sizeof(*o);
}
