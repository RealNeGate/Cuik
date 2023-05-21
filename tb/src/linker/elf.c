#define NL_STRING_MAP_IMPL
#include "linker.h"
#include <tb_elf.h>

static void init(TB_Linker* l) {
    l->entrypoint = "_start";
}

static void append_object(TB_Linker* l, TB_Slice obj_name, TB_ObjectFile* obj) {
    // implement this
}

static void append_library(TB_Linker* l, TB_Slice ar_name, TB_Slice ar_file) {
    // implement this
}

static void append_module(TB_Linker* l, TB_Module* m) {
    CUIK_TIMED_BLOCK("layout section") {
        tb_module_layout_sections(m);
    }

    tb__append_module_section(l, m, &m->text, ".text", PF_X | PF_R);
    tb__append_module_section(l, m, &m->data, ".data", PF_W | PF_R);
    tb__append_module_section(l, m, &m->rdata, ".rdata", PF_R);

    CUIK_TIMED_BLOCK("apply symbols") {
        static const enum TB_SymbolTag tags[] = { TB_SYMBOL_FUNCTION, TB_SYMBOL_GLOBAL };

        TB_FOR_FUNCTIONS(f, m) {
            const char* name = f->super.name;
            TB_LinkerSymbol ls = {
                .name = { strlen(name), (const uint8_t*) name },
                .tag = TB_LINKER_SYMBOL_TB,
                .tb = { m->text.piece, &f->super }
            };
            tb__append_symbol(&l->symtab, &ls);
        }

        // for globals
        TB_FOR_GLOBALS(g, m) if (g->super.name) {
            const char* name = g->super.name;
            TB_LinkerSymbol ls = {
                .name = { strlen(name), (const uint8_t*) name },
                .tag = TB_LINKER_SYMBOL_TB,
                .tb = { g->parent->piece, &g->super }
            };
            tb__append_symbol(&l->symtab, &ls);
        }
    }

    dyn_array_put(l->ir_modules, m);
}

static TB_LinkerSymbol* resolve_sym(TB_Linker* l, TB_LinkerSymbol* sym, TB_Slice name, TB_Slice* alt, uint32_t reloc_i) {
    // resolve any by-name symbols
    if (sym == NULL) {
        sym = tb__find_symbol(&l->symtab, name);
        if (sym != NULL) goto done;

        if (alt) {
            sym = tb__find_symbol(&l->symtab, *alt);
            if (sym != NULL) goto done;
        }

        tb__unresolved_symbol(l, name)->reloc = reloc_i;
        return NULL;
    }

    done:
    return sym;
}

static void gc_mark(TB_Linker* l, TB_LinkerSectionPiece* p) {
    if (p == NULL || p->size == 0 || (p->flags & TB_LINKER_PIECE_LIVE) || (p->parent->generic_flags & TB_LINKER_SECTION_DISCARD)) {
        return;
    }

    p->flags |= TB_LINKER_PIECE_LIVE;

    // mark module content
    if (p->kind == PIECE_MODULE_SECTION && p->module != NULL) {
        gc_mark(l, p->module->text.piece);
        gc_mark(l, p->module->data.piece);
        gc_mark(l, p->module->rdata.piece);
        gc_mark(l, p->module->tls.piece);
    }

    // mark any kid symbols
    for (TB_LinkerSymbol* sym = p->first_sym; sym != NULL; sym = sym->next) {
        gc_mark(l, tb__get_piece(l, sym));
    }

    // mark any relocations
    dyn_array_for(i, p->abs_refs) {
        TB_LinkerRelocAbs* r = &p->abs_refs[i].info->absolutes[p->abs_refs[i].index];

        // resolve symbol
        r->target = resolve_sym(l, r->target, r->name, r->alt, r->obj_file);
        gc_mark(l, tb__get_piece(l, r->target));
    }

    dyn_array_for(i, p->rel_refs) {
        TB_LinkerRelocRel* r = &p->rel_refs[i].info->relatives[p->rel_refs[i].index];

        // resolve symbol
        r->target = resolve_sym(l, r->target, r->name, r->alt, r->obj_file);
        gc_mark(l, tb__get_piece(l, r->target));
    }

    if (p->associate) {
        gc_mark(l, p->associate);
    }
}

#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
static TB_Exports export(TB_Linker* l) {
    CUIK_TIMED_BLOCK("GC sections") {
        // mark roots
        TB_LinkerSymbol* sym = tb__find_symbol_cstr(&l->symtab, l->entrypoint);
        TB_LinkerSectionPiece* entry = tb__get_piece(l, sym);
        gc_mark(l, entry);
    }

    if (!tb__finalize_sections(l)) {
        return (TB_Exports){ 0 };
    }

    TB_Emitter strtbl = { 0 };
    tb_out_reserve(&strtbl, 1024);
    tb_out1b(&strtbl, 0); // null string in the table

    size_t final_section_count = 0;
    nl_map_for(i, l->sections) {
        if (l->sections[i].v->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

        // reserve space for names
        NL_Slice name = l->sections[i].v->name;
        l->sections[i].v->name_pos = tb_outs(&strtbl, name.length + 1, name.data);
        tb_out1b_UNSAFE(&strtbl, 0);

        // we're keeping it for export
        final_section_count += 1;
    }

    TB_Elf64_Shdr strtab = {
        .name = tb_outstr_nul_UNSAFE(&strtbl, ".strtab"),
        .type = SHT_STRTAB,
        .flags = 0,
        .addralign = 1,
        .size = strtbl.count,
    };

    size_t size_of_headers = sizeof(TB_Elf64_Ehdr)
        + (final_section_count * sizeof(TB_Elf64_Phdr))
        + ((2+final_section_count) * sizeof(TB_Elf64_Shdr));

    size_t section_content_size = 0;
    uint64_t virt_addr = size_of_headers;
    CUIK_TIMED_BLOCK("layout sections") {
        nl_map_for(i, l->sections) {
            TB_LinkerSection* s = l->sections[i].v;
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

            s->offset = size_of_headers + section_content_size;
            section_content_size += s->total_size;

            s->address = virt_addr;
            virt_addr += s->total_size;
            // virt_addr = align_up(virt_addr + s->total_size, 4096);
        }
    }

    strtab.offset = size_of_headers + section_content_size;
    section_content_size += strtbl.count;

    uint16_t machine = 0;
    switch (l->target_arch) {
        case TB_ARCH_X86_64: machine = EM_X86_64; break;
        case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
        default: tb_todo();
    }

    size_t output_size = size_of_headers + section_content_size;
    size_t write_pos = 0;
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);
    TB_Elf64_Ehdr header = {
        .ident = {
            [EI_MAG0]       = 0x7F, // magic number
            [EI_MAG1]       = 'E',
            [EI_MAG2]       = 'L',
            [EI_MAG3]       = 'F',
            [EI_CLASS]      = 2, // 64bit ELF file
            [EI_DATA]       = 1, // little-endian
            [EI_VERSION]    = 1, // 1.0
            [EI_OSABI]      = 0,
            [EI_ABIVERSION] = 0
        },
        .type = ET_EXEC, // executable
        .version = 1,
        .machine = machine,
        .entry = 0,

        .flags = 0,

        .ehsize = sizeof(TB_Elf64_Ehdr),

        .phentsize = sizeof(TB_Elf64_Phdr),
        .phoff     = sizeof(TB_Elf64_Ehdr),
        .phnum     = final_section_count,

        .shoff = sizeof(TB_Elf64_Ehdr) + (sizeof(TB_Elf64_Phdr) * final_section_count),
        .shentsize = sizeof(TB_Elf64_Shdr),
        .shnum = final_section_count + 2,
        .shstrndx  = 1,
    };

    // text section crap
    TB_LinkerSection* text = tb__find_section(l, ".text", PF_X | PF_R);
    TB_LinkerSymbol* sym = tb__find_symbol_cstr(&l->symtab, l->entrypoint);
    if (text && sym) {
        if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
            header.entry = text->address + sym->normal.piece->offset + sym->normal.secrel;
        } else if (sym->tag == TB_LINKER_SYMBOL_TB) {
            header.entry = text->address + sym->tb.piece->offset + tb__get_symbol_pos(sym->tb.sym);
        } else {
            tb_todo();
        }
    } else {
        fprintf(stderr, "tblink: could not find entrypoint!\n");
    }
    WRITE(&header, sizeof(header));

    // write program headers
    nl_map_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i].v;
        TB_Elf64_Phdr sec = {
            .type   = PT_LOAD,
            .flags  = s->flags,
            .offset = s->offset,
            .vaddr  = s->address,
            .filesz = s->total_size,
            .memsz  = s->total_size,
            .align  = 1,
        };
        WRITE(&sec, sizeof(sec));
    }

    // write section headers
    memset(&output[write_pos], 0, sizeof(TB_Elf64_Shdr)), write_pos += sizeof(TB_Elf64_Shdr);
    WRITE(&strtab, sizeof(strtab));
    nl_map_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i].v;
        TB_Elf64_Shdr sec = {
            .name = s->name_pos,
            .type = SHT_PROGBITS,
            .flags = SHF_ALLOC | ((s->flags & PF_X) ? SHF_EXECINSTR : 0) | ((s->flags & PF_W) ? SHF_WRITE : 0),
            .addralign = 1,
            .size = s->total_size,
            .addr = s->address,
            .offset = s->offset,
        };
        WRITE(&sec, sizeof(sec));
    }

    TB_LinkerSection* data  = tb__find_section(l, ".data", PF_W | PF_R);
    TB_LinkerSection* rdata = tb__find_section(l, ".rdata", PF_R);

    // write section contents
    write_pos = tb__apply_section_contents(l, output, write_pos, text, data, rdata, 1, 0);
    WRITE(strtbl.data, strtbl.count);
    assert(write_pos == output_size);

    // TODO(NeGate): multithread this too
    CUIK_TIMED_BLOCK("apply final relocations") {
        dyn_array_for(i, l->ir_modules) {
            tb__apply_module_relocs(l, l->ir_modules[i], output);
        }

        // tb__apply_external_relocs(l, output, opt_header.image_base);
    }

    // write section contents
    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}

TB_LinkerVtbl tb__linker_elf = {
    .init           = init,
    .append_object  = append_object,
    .append_library = append_library,
    .append_module  = append_module,
    .export         = export
};
