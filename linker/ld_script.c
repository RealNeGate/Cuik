#include <pool.h>

////////////////////////////////
// Parsing GNU ld scripts
////////////////////////////////
void elf_append_script(TPool* pool, TB_LinkerObject* lib, int slash) {
    log_debug("linking against %.*s (as LD script)", (int) (lib->name.length - slash), lib->name.data + slash);

    TB_Linker* l = lib->linker;
    TB_Slice content = lib->content;

    printf("%.*s\n", (int) content.length, content.data);
}


