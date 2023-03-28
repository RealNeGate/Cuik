
int run_objdump(int argc, const char** argv) {
    if (argc < 1) {
        fprintf(stderr, "\x1b[31merror\x1b[0m: no input files!\n");
        return EXIT_FAILURE;
    }

    FileMap fm = open_file_map(argv[0]);
    if (fm.data == NULL) {
        fprintf(stderr, "\x1b[31merror\x1b[0m: '%s' not found!\n", argv[0]);
        return EXIT_FAILURE;
    }

    TB_ObjectFile* file = tb_object_parse_coff((TB_Slice){ fm.size, fm.data });
    if (file == NULL) {
        // TODO(NeGate): make better errors
        fprintf(stderr, "\x1b[31merror\x1b[0m: '%s' was not a valid COFF object!\n", argv[0]);
        return EXIT_FAILURE;
    }

    printf("SECTIONS:\n");
    printf("Idx Name          Size     Address          Type\n");
    for (size_t i = 0; i < file->section_count; i++) {
        TB_ObjectSection* s = &file->sections[i];
        (void)s;

        char name[9];
        int len = s->name.length > 8 ? 8 : s->name.length;
        memcpy(name, s->name.data, len);
        name[len] = 0;

        size_t size = s->raw_data.length;
        if (size < s->virtual_size) size = s->virtual_size;

        char flags[7];
        flags[0] = (s->flags & TB_COFF_SECTION_READ)    ? 'r' : '-';
        flags[1] = (s->flags & TB_COFF_SECTION_EXECUTE) ? 'x' : '-';
        flags[2] = (s->flags & TB_COFF_SECTION_WRITE)   ? 'w' : '-';
        flags[3] = (s->flags & TB_COFF_SECTION_CODE)    ? 'c' : '-';
        flags[4] = (s->flags & TB_COFF_SECTION_UNINIT)  ? 'u' : '-';
        flags[5] = (s->flags & TB_COFF_SECTION_INIT)    ? 'i' : '-';
        flags[6] = 0;

        printf("%3zu %-8s      %08zx %016zx %s", i, name, size, s->virtual_address, flags);
        if (s->flags & TB_COFF_SECTION_ALIGN) {
            printf(" (align %d)", 1u << (s->flags >> 20));
        }
        printf("\n");
    }
    printf("\n  note:\n");
    printf("    r - read   w - write   x - execute\n");
    printf("    c - code   u - uninit  i - init\n");

    printf("\nSYMBOLS:\n");
    printf("Index Value    Section  Storage   Name\n");
    for (size_t i = 0; i < file->symbol_count; i++) {
        TB_ObjectSymbol* s = &file->symbols[i];

        printf("%5u %08x ", s->ordinal, s->value);
        if (s->section_num > 0) {
            TB_ObjectSection* sec = &file->sections[s->section_num - 1];

            char name[9];
            int len = sec->name.length > 8 ? 8 : sec->name.length;
            memcpy(name, sec->name.data, len);
            name[len] = 0;

            printf("%-8s ", name);
        } else {
            printf("*none*   ");
        }

        switch (s->type) {
            case TB_OBJECT_SYMBOL_EXTERN:      printf("EXTERN  "); break;
            case TB_OBJECT_SYMBOL_WEAK_EXTERN: printf("WEAK    "); break;
            case TB_OBJECT_SYMBOL_STATIC:      printf("STATIC  "); break;
            default: printf("???     "); break;
        }
        printf("| %.*s\n", (int) s->name.length, s->name.data);
    }

    printf("\n");
    return 0;
}