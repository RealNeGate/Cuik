#include "linker.h"
#include "../tb/objects/lib_parse.h"

void append_archive(TPool* pool, TB_LinkerObject* lib, int slash) {
    log_debug("linking against %.*s (as archive)", (int) (lib->name.length - slash), lib->name.data + slash);

    TB_Linker* l = lib->linker;
    TB_Slice ar_file = lib->content;

    TB_ArchiveFileParser ar_parser = { 0 };
    if (!tb_archive_parse(ar_file, &ar_parser)) {
        cuikperf_region_end();
        return;
    }

    // populate lazy symbols, distribute work to other threads... because we can :)
    CUIK_TIMED_BLOCK("lazy") {
        uint64_t t = lib->time;
        char* strtab = ar_parser.symbol_strtab;

        size_t i = 0, j = 0;
        while (i < ar_parser.symbol_count) {
            uint16_t offset_index = ar_parser.symbols[i] - 1;

            const char* name = &strtab[j];
            size_t len = ideally_fast_strlen(name);

            TB_ArchiveEntry e = tb_archive_member_get(&ar_parser, offset_index);

            // We don't *really* care about this info beyond nicer errors (use an arena tho)
            TB_LinkerObject* obj_file = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
            *obj_file = (TB_LinkerObject){ e.name, l, e.content, t + offset_index*65536, lib };

            TB_LinkerObject* k = namehs_intern(&l->objects, obj_file);
            if (k != obj_file) {
                tb_arena_free(&linker_perm_arena, k, sizeof(TB_LinkerObject));
                obj_file = k;
            }

            printf("%s : %u : %#x (%.*s)\n", name, offset_index, ar_parser.members[offset_index], (int) e.name.length, e.name.data);
            TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
            *s = (TB_LinkerSymbol){
                .name   = { (const uint8_t*) name, len },
                .tag    = TB_LINKER_SYMBOL_LAZY,
                .lazy   = { obj_file },
            };

            TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
            if (new_s != s) {
                tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
                s = new_s;
            }
            i += 1, j += len + 1;
        }
    }

    __debugbreak();
}

