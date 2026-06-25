#include "linker.h"
#include <tb_elf.h>

void elf_init(TB_Linker* l) {
    l->entrypoint = "_start";
}

FileMap elf_find_lib(TB_Linker* l, const char* file_name, char* path) {
    FileMap fm;
    dyn_array_for(j, l->libpaths) {
        snprintf(path, FILENAME_MAX, "%s/lib%s.so", l->libpaths[j], file_name);
        fm = open_file_map_read(path);
        if (fm.data != NULL) {
            break;
        }
        path[0] = 0;
    }

    if (path[0] == 0) {
        dyn_array_for(j, l->libpaths) {
            snprintf(path, FILENAME_MAX, "%s/lib%s.a", l->libpaths[j], file_name);
            fm = open_file_map_read(path);
            if (fm.data != NULL) {
                break;
            }
            path[0] = 0;
        }
    }

    if (path[0] == 0) {
        printf("tblink: could not find library: %s\n", file_name);
        dyn_array_for(j, l->libpaths) {
            snprintf(path, FILENAME_MAX, "%s/lib%s.so", l->libpaths[j], file_name);
            printf("  searched at %s\n", path);
        }

        dyn_array_for(j, l->libpaths) {
            snprintf(path, FILENAME_MAX, "%s/lib%s.a", l->libpaths[j], file_name);
            printf("  searched at %s\n", path);
        }
        return (FileMap){ 0 };
    }
    return fm;
}

static void elf_append_shared(TPool* pool, TB_LinkerObject* lib) {

}

void elf_append_library(TPool* pool, void** args) {
    TB_LinkerObject* lib = args[0];

    size_t slash = 0;
    FOR_REV_N(i, 0, lib->name.length) {
        if (lib->name.data[i] == '/' || lib->name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }

    cuikperf_region_start2("library", lib->name.length - slash, (const char*) lib->name.data + slash);

    TB_Linker* l = lib->linker;
    TB_Slice ar_file = lib->content;
    if (ar_file.length >= 4 && memcmp(ar_file.data, (uint8_t[4]){ 0x7F, 'E', 'L', 'F' }, 4) == 0) {
        __debugbreak();
    } else if (ar_file.length >= 8 && memcmp(ar_file.data, "!<arch>\n", 8) == 0) {
        append_archive(pool, lib, slash);
    } else {
        elf_append_script(pool, lib, slash);
    }

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }

    cuikperf_region_end();
}

void elf_append_object(TPool* pool, void** args) {
    TB_LinkerObject* obj = args[0];

    size_t slash = 0;
    FOR_REV_N(i, 0, obj->name.length) {
        if (obj->name.data[i] == '/' || obj->name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }

    cuikperf_region_start2("object", obj->name.length - slash, (const char*) obj->name.data + slash);
    log_info("Object: %.*s\n", obj->name.length - slash, (const char*) obj->name.data + slash);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_Linker* l = obj->linker;
    TB_Slice name = obj->name;
    TB_Slice content = obj->content;

    TB_ASSERT(sizeof(TB_Elf64_Ehdr) <= content.length);
    TB_Elf64_Ehdr* header = (TB_Elf64_Ehdr*) &content.data[0];

    TB_ASSERT(header->shoff + header->shnum*header->shentsize <= content.length);
    TB_Elf64_Shdr* symtab = NULL;

    TB_ArenaSavepoint sp = tb_arena_save(&linker_tmp_arena);
    TB_LinkerSectionPiece** sections = tb_arena_alloc(&linker_tmp_arena, header->shnum * sizeof(TB_LinkerSectionPiece*));

    CUIK_TIMED_BLOCK("parse sections") {
        TB_ASSERT(header->shstrndx < header->shnum);
        TB_Elf64_Shdr* sh_strings = (TB_Elf64_Shdr*) &content.data[header->shoff + header->shstrndx*header->shentsize];

        FOR_N(i, 0, header->shnum) {
            TB_Elf64_Shdr* section_header = (TB_Elf64_Shdr*) &content.data[header->shoff + i*header->shentsize];

            const char* name = (const char*) &content.data[sh_strings->offset + section_header->name];
            size_t name_len = strlen(name);
            if (name_len == sizeof(".symtab") - 1 && memcmp(name, ".symtab", name_len) == 0) {
                symtab = section_header;
            }

            TB_LinkerSectionPiece* p = NULL;
            if (section_header->type == TB_SHT_PROGBITS || section_header->type == TB_SHT_NOBITS) {
                uint32_t flags = TB_PF_R;
                if (section_header->flags & TB_SHF_WRITE)     { flags |= TB_PF_W; }
                if (section_header->flags & TB_SHF_EXECINSTR) { flags |= TB_PF_X; }

                TB_LinkerSection* ls = tb_linker_find_or_create_section(l, name_len, name, flags);

                p = tb_linker_append_piece(ls, PIECE_BSS, section_header->size, obj);
                if (section_header->type == TB_SHT_PROGBITS) {
                    p->kind = PIECE_BUFFER;
                    p->buffer = &content.data[section_header->offset];
                    p->buffer_size = section_header->size;
                }
            }
            sections[i] = p;
        }
    }

    if (symtab) {
        CUIK_TIMED_BLOCK("parse symbols") {
            // printf("\nSymbols (sh_link=%d):\n", symtab->link);

            TB_ASSERT(symtab->link < header->shnum);
            TB_Elf64_Shdr* sh_strings = (TB_Elf64_Shdr*) &content.data[header->shoff + symtab->link*header->shentsize];

            size_t symbol_count = symtab->size / sizeof(TB_Elf64_Sym);
            DynArray(TB_LinkerSymbol*) symbol_map = dyn_array_create(TB_LinkerSymbol*, symbol_count);
            dyn_array_set_length(symbol_map, symbol_count);

            TB_Elf64_Sym* syms = (TB_Elf64_Sym*) &content.data[symtab->offset];
            symbol_map[0] = NULL;
            FOR_N(i, 1, symbol_count) {
                const char* name = (const char*) &content.data[sh_strings->offset + syms[i].name];
                size_t name_len = strlen(name);

                uint32_t bind = syms[i].info >> 4;
                uint32_t type = syms[i].info & 15;

                // printf("  %zu: %#016"PRIx64" %d %s %#x %#x (name=%d)\n", i, syms[i].value, syms[i].shndx, name, bind, type, syms[i].name);

                TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name   = { (const uint8_t*) name, name_len },
                    .tag    = TB_LINKER_SYMBOL_UNKNOWN,
                };

                if (syms[i].shndx != 0) {
                    s->tag = TB_LINKER_SYMBOL_NORMAL;
                    s->normal.piece  = sections[syms[i].shndx];
                    s->normal.secrel = syms[i].value;
                }

                // place non-locals into the global symbol table
                if (bind != TB_ELF64_STB_LOCAL) {
                    TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
                    if (new_s != s) {
                        tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
                        s = new_s;
                    }
                }
                symbol_map[i] = s;
            }

            // associate relocations with their sections + symbol map
            FOR_N(i, 0, header->shnum) {
                TB_Elf64_Shdr* section_header = (TB_Elf64_Shdr*) &content.data[header->shoff + i*header->shentsize];

                if (sections[i] != NULL) {
                    sections[i]->symbol_map = symbol_map;
                }

                // TODO(NeGate): support the lame relocations without addend
                if (section_header->type == TB_SHT_RELA) {
                    TB_LinkerSectionPiece* p = sections[section_header->info];
                    if (p != NULL) {
                        p->reloc_count = section_header->size / sizeof(TB_Elf64_Rela);
                        p->relocs = &content.data[section_header->offset];
                    }
                }
            }
        }
    }
    tb_arena_restore(&linker_tmp_arena, sp);

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }
    cuikperf_region_end();
}

static void elf_parse_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, size_t reloc_i, TB_LinkerReloc* out_reloc) {
    TB_Elf64_Rela* rel = &((TB_Elf64_Rela*) p->relocs)[reloc_i];
    out_reloc->src_offset = rel->offset;
    out_reloc->addend     = rel->addend;
    out_reloc->target     = p->symbol_map[TB_ELF64_R_SYM(rel->info)];

    int type;
    switch (TB_ELF64_R_TYPE(rel->info)) {
        case TB_ELF_X86_64_64:       type = TB_OBJECT_RELOC_ADDR64;  break;
        case TB_ELF_X86_64_PC32:     type = TB_OBJECT_RELOC_REL32;   break;
        case TB_ELF_X86_64_PLT32:    type = TB_OBJECT_RELOC_REL32;   break;
        case TB_ELF_X86_64_GOTPCREL: type = TB_OBJECT_RELOC_GOTPCREL; break;

        // TODO(NeGate): incorrect but we'll fix it later.
        //   these are meant to be GOTPCRELs which can convert from the indirect load/store
        //   to a direct load/store.
        case TB_ELF_X86_64_GOTPCRELX:     type = TB_OBJECT_RELOC_GOTPCREL; break;
        case TB_ELF_X86_64_REX_GOTPCRELX: type = TB_OBJECT_RELOC_GOTPCREL; break;

        default: TB_ASSERT_MSG(0, "missing relocation type");
    }
    out_reloc->type = type;
}

#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
static bool elf_export(TB_Linker* l, const char* file_name) {
    cuikperf_region_start("linker", NULL);

    if (l->jobs.pool != NULL) {
        // finish up parsing all the object file tasks
        int32_t old;
        while (old = l->jobs.done, old != l->jobs.count) {
            futex_wait(&l->jobs.done, old);
        }
    }

    CUIK_TIMED_BLOCK("resize barrier") {
        namehs_resize_barrier(&l->symbols);
        namehs_resize_barrier(&l->sections);
        namehs_resize_barrier(&l->imports);
    }

    // this will resolve the sections, GC any pieces which aren't used and
    // resolve symbols.
    CUIK_TIMED_BLOCK("Resolve & GC") {
        tb_linker_mark_live(l);
    }

    // Layout
    if (!tb_linker_layout(l)) {
        cuikperf_region_end();
        return false;
    }

    TB_Emitter strtbl = { 0 };
    tb_out_reserve(&strtbl, 1024);
    tb_out1b(&strtbl, 0); // null string in the table

    size_t strtab_name = tb_outstr_nul_UNSAFE(&strtbl, ".strtab");

    size_t final_section_count = 0;
    dyn_array_for(i, l->segments) {
        TB_LinkerSegment* s = l->segments[i];

        // reserve space for names
        s->name_pos = tb_outs(&strtbl, s->name.length, s->name.data);
        tb_out1b_UNSAFE(&strtbl, 0);

        final_section_count += 1;
    }

    // +2 comes from the two extra sections:
    //   one for the null section header at [0]
    //   one for the strtab that i put right in front of it
    size_t size_of_headers = sizeof(TB_Elf64_Ehdr)
        + ((1+final_section_count) * sizeof(TB_Elf64_Phdr))
        + ((2+final_section_count) * sizeof(TB_Elf64_Shdr));

    // size_of_headers = align_up(size_of_headers, 512);

    size_t output_size = size_of_headers + strtbl.count;
    uint64_t virt_addr = align_up(size_of_headers, 4096);
    CUIK_TIMED_BLOCK("layout sections") {
        dyn_array_for(i, l->segments) {
            TB_LinkerSegment* s = l->segments[i];

            s->offset = output_size;
            output_size += s->size;

            // we need to make sure:
            //   vaddr % page_size == offset % page_size
            size_t in_page_offset = s->offset & 4095;
            s->address = virt_addr + in_page_offset;
            virt_addr += align_up(in_page_offset + s->size, 4096);

            log_debug("Segment %.*s: %#x - %#x", (int) s->name.length, s->name.data, s->offset, s->offset + s->size - 1);
        }
    }

    tb_linker_print_map(l);

    uint64_t entrypoint = 0;
    TB_LinkerSymbol* entry_sym = tb_linker_symbol_find(tb_linker_find_symbol2(l, l->entrypoint));
    if (entry_sym == NULL) {
        printf("tblink: could not find entrypoint! %s\n", l->entrypoint);
        cuikperf_region_end();
        return false;
    }
    entrypoint = tb__get_symbol_rva(entry_sym);

    // Writing work
    FileMap fm = open_file_map_write(file_name, output_size);
    if (fm.data == NULL) {
        printf("tblink: could not open file! %s", file_name);
        cuikperf_region_end();
        return false;
    }

    CUIK_TIMED_BLOCK("write") {
        size_t write_pos = 0;
        uint8_t* output = fm.data;

        TB_Elf64_Ehdr header = {
            .ident = {
                [TB_EI_MAG0]       = 0x7F, // magic number
                [TB_EI_MAG1]       = 'E',
                [TB_EI_MAG2]       = 'L',
                [TB_EI_MAG3]       = 'F',
                [TB_EI_CLASS]      = 2, // 64bit ELF file
                [TB_EI_DATA]       = 1, // little-endian
                [TB_EI_VERSION]    = 1, // 1.0
                [TB_EI_OSABI]      = 0,
                [TB_EI_ABIVERSION] = 0
            },
            .type = TB_ET_DYN, // executable
            .version = 1,
            .entry = entrypoint,

            .flags = 0,

            .ehsize = sizeof(TB_Elf64_Ehdr),

            .phentsize = sizeof(TB_Elf64_Phdr),
            .phoff     = sizeof(TB_Elf64_Ehdr),
            .phnum     = 1 + final_section_count,

            .shoff = sizeof(TB_Elf64_Ehdr) + (sizeof(TB_Elf64_Phdr) * (1 + final_section_count)),
            .shentsize = sizeof(TB_Elf64_Shdr),
            .shnum = 2 + final_section_count,
            .shstrndx  = 1,
        };
        switch (l->target_arch) {
            case TB_ARCH_X86_64:  header.machine = TB_EM_X86_64;  break;
            case TB_ARCH_AARCH64: header.machine = TB_EM_AARCH64; break;
            default: tb_todo();
        }
        WRITE(&header, sizeof(header));

        // write program headers
        TB_Elf64_Phdr phdr = {
            .type   = TB_PT_LOAD,
            .flags  = TB_PF_R,
            .offset = 0,
            .vaddr  = 0,
            .filesz = size_of_headers,
            .memsz  = size_of_headers,
            .align  = 1,
        };
        WRITE(&phdr, sizeof(phdr));
        dyn_array_for(i, l->segments) {
            TB_LinkerSegment* s = l->segments[i];
            TB_Elf64_Phdr sec = {
                .type   = TB_PT_LOAD,
                .flags  = s->flags,
                .offset = s->offset,
                .vaddr  = s->address,
                .filesz = s->size,
                .memsz  = s->size,
                .align  = 1,
            };
            WRITE(&sec, sizeof(sec));
        }

        TB_Elf64_Shdr strtab = {
            .name = strtab_name,
            .type = TB_SHT_STRTAB,
            .flags = 0,
            .addralign = 1,
            .size = strtbl.count,
            .offset = size_of_headers,
        };

        // write section headers
        memset(&output[write_pos], 0, sizeof(TB_Elf64_Shdr)), write_pos += sizeof(TB_Elf64_Shdr);
        WRITE(&strtab, sizeof(strtab));
        dyn_array_for(i, l->segments) {
            TB_LinkerSegment* s = l->segments[i];
            TB_Elf64_Shdr sec = {
                .name = s->name_pos,
                .type = TB_SHT_PROGBITS,
                .flags = TB_SHF_ALLOC | ((s->flags & TB_PF_X) ? TB_SHF_EXECINSTR : 0) | ((s->flags & TB_PF_W) ? TB_SHF_WRITE : 0),
                .addralign = 1,
                .size = s->size,
                .addr = s->address,
                .offset = s->offset,
            };
            WRITE(&sec, sizeof(sec));
        }

        WRITE(strtbl.data, strtbl.count);

        l->output = output;
        l->output_cap = output_size;
        tb_linker_export_pieces(l);
    }

    close_file_map(&fm);
    cuikperf_region_end();
    return true;
}
#undef WRITE

TB_LinkerVtbl tb__linker_elf = {
    .init           = elf_init,
    .find_lib       = elf_find_lib,
    .append_object  = elf_append_object,
    .append_library = elf_append_library,
    .parse_reloc    = elf_parse_reloc,
    .export         = elf_export
};

