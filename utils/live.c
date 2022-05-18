#include <stdlib.h>
#include <stdio.h>

#define O(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__)
static void print_help(const char* executable_path) {
	O("Usage: %s [<options>] <command> [<args>]", executable_path ? executable_path : "live-cuik");
}
#undef O

int main(int argc, char** argv) {
	if (argc < 2) {
		fprintf(stderr, "error: expected arguments\n");
		print_help(argc == 0 ? NULL : argv[0]);
		return EXIT_FAILURE;
	}



	return 0;
}