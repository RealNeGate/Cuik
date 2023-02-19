#define NL_STRING_MAP_IMPL
#include "linker.h"
#include "../objects/elf64.h"

static void append_object(TB_Linker* l, TB_Slice obj_name, TB_ObjectFile* obj) {
    // implement this
}

static void append_library(TB_Linker* l, TB_Slice ar_file) {
    // implement this
}

static void append_module(TB_Linker* l, TB_Module* m) {
    // Convert module into sections which we can then append to the output
    TB_LinkerSection* text = tb__find_or_create_section(l, ".text", PF_X | PF_R);
    m->linker.text = tb__append_piece(text, PIECE_TEXT, tb__layout_text_section(m), NULL, m);

    if (m->data_region_size > 0) {
        TB_LinkerSection* data = tb__find_or_create_section(l, ".data", PF_W | PF_R);
        m->linker.data = tb__append_piece(data, PIECE_DATA, m->data_region_size, NULL, m);
    }

    TB_LinkerSection* rdata = NULL;
    if (m->rdata_region_size > 0) {
        rdata = tb__find_or_create_section(l, ".rdata", PF_R);
        m->linker.rdata = tb__append_piece(rdata, PIECE_RDATA, m->rdata_region_size, NULL, m);
    }

    CUIK_TIMED_BLOCK("apply symbols") {
        static const enum TB_SymbolTag tags[] = { TB_SYMBOL_FUNCTION, TB_SYMBOL_GLOBAL };

        FOREACH_N(i, 0, COUNTOF(tags)) {
            enum TB_SymbolTag tag = tags[i];
            TB_LinkerSectionPiece* piece = i ? m->linker.data : m->linker.text;

            for (TB_Symbol* sym = m->first_symbol_of_tag[tag]; sym != NULL; sym = sym->next) {
                TB_LinkerSymbol ls = {
                    .name = { strlen(sym->name), (const uint8_t*) sym->name },
                    .tag = TB_LINKER_SYMBOL_TB,
                    .tb = { piece, sym }
                };
                tb__append_symbol(&l->symtab, &ls);
            }
        }
    }

    dyn_array_put(l->ir_modules, m);
}

#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
static TB_Exports export(TB_Linker* l) {
    size_t final_section_count = 0;
    nl_strmap_for(i, l->sections) {
        final_section_count += (l->sections[i]->generic_flags & TB_LINKER_SECTION_DISCARD) == 0;
    }

    size_t size_of_headers = sizeof(Elf64_Ehdr)
        + (final_section_count * sizeof(Elf64_Phdr));

    size_t section_content_size = 0;
    uint64_t virt_addr = size_of_headers; // align_up(size_of_headers, 4096);
    CUIK_TIMED_BLOCK("layout sections") {
        nl_strmap_for(i, l->sections) {
            TB_LinkerSection* s = l->sections[i];
            s->offset = size_of_headers + section_content_size;
            section_content_size += s->total_size;

            s->address = virt_addr;
            virt_addr += s->total_size;
        }
    }

    uint16_t machine = 0;
    switch (l->target_arch) {
        case TB_ARCH_X86_64: machine = EM_X86_64; break;
        case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
        default: tb_todo();
    }

    size_t output_size = size_of_headers + section_content_size;
    size_t write_pos = 0;
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);
    Elf64_Ehdr header = {
        .e_ident = {
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
        .e_type = ET_EXEC, // executable
        .e_version = 1,
        .e_machine = machine,
        .e_entry = 0,

        .e_phoff = sizeof(Elf64_Ehdr),
        .e_flags = 0,

        .e_ehsize = sizeof(Elf64_Ehdr),

        .e_phentsize = sizeof(Elf64_Phdr),
        .e_phnum     = final_section_count,
    };

    // text section crap
    TB_LinkerSection* text  = tb__find_section(l, ".text", PF_X | PF_R);
    TB_LinkerSymbol* sym = tb__find_symbol_cstr(&l->symtab, "_start");
    if (text && sym) {
        if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
            header.e_entry = text->address + sym->normal.piece->offset + sym->normal.secrel;
        } else if (sym->tag == TB_LINKER_SYMBOL_TB) {
            header.e_entry = text->address + sym->tb.piece->offset + tb__get_symbol_pos(sym->tb.sym);
        } else {
            tb_todo();
        }
    } else {
        fprintf(stderr, "tblink: could not find entrypoint!\n");
    }
    WRITE(&header, sizeof(header));

    // write section headers
    nl_strmap_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i];
        Elf64_Phdr sec = {
            .p_type   = PT_LOAD,
            .p_flags  = s->flags,
            .p_offset = s->offset,
            .p_vaddr  = s->address,
            .p_filesz = s->total_size,
            .p_memsz  = s->total_size,
            .p_align  = 0,
        };
        WRITE(&sec, sizeof(sec));
    }

    TB_LinkerSection* data  = tb__find_section(l, ".data", PF_W | PF_R);
    TB_LinkerSection* rdata = tb__find_section(l, ".rdata", PF_R);

    write_pos = tb__apply_section_contents(l, output, write_pos, text, data, rdata, 1, 0);
    assert(write_pos == output_size);

    // TODO(NeGate): multithread this too
    CUIK_TIMED_BLOCK("apply final relocations") {
        dyn_array_for(i, l->ir_modules) {
            tb__apply_external_relocs(l, l->ir_modules[i], output);
        }
    }

    // write section contents
    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}

TB_LinkerVtbl tb__linker_elf = {
    .append_object  = append_object,
    .append_library = append_library,
    .append_module  = append_module,
    .export         = export
};
