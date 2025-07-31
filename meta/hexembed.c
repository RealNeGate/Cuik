/* hexembed.c - copyright Lewis Van Winkle */
/* zlib license */
/* negate modded it, because it was cool */
#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    if (argc < 4) {
        printf("Usage:\n\thexembed <prefix> <output> <inputs>\n");
        return 1;
    }

    const char* prefix = argv[1];
    FILE* out = fopen(argv[2], "wb");
    fprintf(out, "#include <stddef.h>\n\n");
    fprintf(out, "typedef struct InternalFile InternalFile;\n");
    fprintf(out, "struct InternalFile {\n");
    fprintf(out, "    InternalFile* next;\n");
    fprintf(out, "    const char* name;\n");
    fprintf(out, "    size_t size;\n");
    fprintf(out, "    char data[];\n");
    fprintf(out, "};\n");

    for (int i = 3; i < argc; i++) {
        const char *fname = argv[i];
        FILE *fp = fopen(fname, "rb");
        if (!fp) {
            fprintf(stderr, "Error opening file: %s.\n", fname);
            return 1;
        }

        fseek(fp, 0, SEEK_END);
        const int fsize = ftell(fp);
        int padded_size = fsize + 16;

        fseek(fp, 0, SEEK_SET);
        unsigned char *b = malloc(padded_size);
        fread(b, fsize, 1, fp);
        memset(b+fsize, 0, 16);
        fclose(fp);

        int file_index = i - 3;
        fprintf(out, "InternalFile %s%d = {\n", prefix, file_index);
        if (file_index) {
            fprintf(out, "    .next = &%s%d,\n", prefix, file_index - 1);
        }
        fprintf(out, "    .name = \"%s\",\n", strrchr(fname, '/')+1);
        fprintf(out, "    .size = %d,\n", fsize);
        fprintf(out, "    .data = {\n        ");
        for (int j = 0; j < padded_size; ++j) {
            fprintf(out, "0x%02x%s", b[j] == '\t' ? ' ' : b[j], j == padded_size-1 ? "" : ((j+1) % 16 == 0 ? ",\n        " : ","));
        }
        fprintf(out, "\n    }\n};\n\n");
        free(b);
    }
    fprintf(out, "InternalFile* %s_root = &%s%d;\n", prefix, prefix, argc-4);
    fclose(out);
    return 0;
}
