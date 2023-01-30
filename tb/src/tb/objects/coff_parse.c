#include "coff.h"

// let's ignore error handling for now :p
// buffered reading i guess?
TB_ObjectFile* tb_object_parse_coff(const TB_Slice file) {
    COFF_FileHeader* header = (COFF_FileHeader*) &file.data[0];

    TB_ObjectFile* obj_file = tb_platform_heap_alloc(sizeof(TB_ObjectFile) + (header->num_sections * sizeof(TB_ObjectSection)));

    // not using calloc since i only really wanna clear the header
    memset(obj_file, 0, sizeof(TB_ObjectFile));
    obj_file->type = TB_OBJECT_FILE_COFF;

    switch (header->machine) {
        case COFF_MACHINE_AMD64: obj_file->arch = TB_ARCH_X86_64; break;
        case COFF_MACHINE_ARM64: obj_file->arch = TB_ARCH_AARCH64; break;
        default: obj_file->arch = TB_ARCH_UNKNOWN; break;
    }

    size_t string_table_pos = header->symbol_table + (header->symbol_count * sizeof(COFF_Symbol));

    // Read string table
    TB_Slice string_table = {
        .length = file.length - string_table_pos,
        .data   = &file.data[string_table_pos]
    };

    obj_file->symbols = tb_platform_heap_alloc(header->symbol_count * sizeof(TB_ObjectSymbol));
    obj_file->symbol_count = 0;

    size_t sym_id = 0;
    while (sym_id < header->symbol_count) {
        size_t symbol_offset = header->symbol_table + (sym_id * sizeof(COFF_Symbol));
        COFF_Symbol* sym = (COFF_Symbol*) &file.data[symbol_offset];

        TB_ObjectSymbol* out_sym = &obj_file->symbols[obj_file->symbol_count++];
        *out_sym = (TB_ObjectSymbol) { 0 };

        // Parse string table name stuff
        if (sym->long_name[0] == 0) {
            // string table access (read a cstring)
            // TODO(NeGate): bounds check this
            const uint8_t* data = &string_table.data[sym->long_name[1]];
            out_sym->name = (TB_Slice){ strlen((const char*) data), data };
        } else {
            // normal inplace string
            size_t len = strlen((const char*) sym->short_name);
            out_sym->name = (TB_Slice){ len, sym->short_name };
        }

        // Process aux symbols
        // FOREACH_N(j, 0, sym->aux_symbols_count) {
        // TODO(NeGate): idk do something
        // }

        sym_id += sym->aux_symbols_count + 1;
    }

    // trim the symbol table
    obj_file->symbols = realloc(obj_file->symbols, obj_file->symbol_count * sizeof(TB_ObjectSymbol));

    obj_file->section_count = header->num_sections;
    FOREACH_N(i, 0, header->num_sections) {
        // TODO(NeGate): bounds check this
        size_t section_offset = sizeof(COFF_FileHeader) + (i * sizeof(COFF_SectionHeader));
        COFF_SectionHeader* sec = (COFF_SectionHeader*) &file.data[section_offset];

        TB_ObjectSection* restrict out_sec = &obj_file->sections[i];
        *out_sec = (TB_ObjectSection) { .flags = sec->characteristics };

        // Parse string table name stuff
        uint32_t long_name[2];
        memcpy(long_name, sec->name, sizeof(uint8_t[8]));
        if (long_name[0] == 0) {
            // string table access
            tb_todo();
        } else {
            // normal inplace string
            size_t len = strlen(sec->name);
            out_sec->name = (TB_Slice){ len, (uint8_t*) sec->name };
        }

        // Parse relocations
        if (sec->num_reloc > 0) {
            out_sec->relocation_count = sec->num_reloc;
            COFF_ImageReloc* src_relocs = (COFF_ImageReloc*) &file.data[sec->pointer_to_reloc];

            TB_ObjectReloc* dst_relocs = tb_platform_heap_alloc(sec->num_reloc * sizeof(TB_ObjectReloc));
            FOREACH_N(j, 0, sec->num_reloc) {
                dst_relocs[j] = (TB_ObjectReloc){ 0 };
                switch (src_relocs[j].Type) {
                    case IMAGE_REL_AMD64_ADDR32NB: dst_relocs[j].type = TB_OBJECT_RELOC_ADDR32NB; break;
                    case IMAGE_REL_AMD64_ADDR32:   dst_relocs[j].type = TB_OBJECT_RELOC_ADDR32; break;
                    case IMAGE_REL_AMD64_ADDR64:   dst_relocs[j].type = TB_OBJECT_RELOC_ADDR64; break;
                    case IMAGE_REL_AMD64_SECREL:   dst_relocs[j].type = TB_OBJECT_RELOC_SECREL; break;
                    case IMAGE_REL_AMD64_SECTION:  dst_relocs[j].type = TB_OBJECT_RELOC_SECTION; break;

                    case IMAGE_REL_AMD64_REL32:
                    case IMAGE_REL_AMD64_REL32_1:
                    case IMAGE_REL_AMD64_REL32_2:
                    case IMAGE_REL_AMD64_REL32_3:
                    case IMAGE_REL_AMD64_REL32_4:
                    case IMAGE_REL_AMD64_REL32_5:
                    dst_relocs[j].type = TB_OBJECT_RELOC_REL32;
                    break;

                    default: tb_todo();
                }

                if (src_relocs[j].Type >= IMAGE_REL_AMD64_REL32 && src_relocs[j].Type <= IMAGE_REL_AMD64_REL32_5) {
                    dst_relocs[j].addend = src_relocs[j].Type - IMAGE_REL_AMD64_REL32;
                }

                dst_relocs[j].symbol_index = src_relocs[j].SymbolTableIndex;
                dst_relocs[j].virtual_address = src_relocs[j].VirtualAddress;
            }

            out_sec->relocations = dst_relocs;
        }

        // Parse virtual region
        out_sec->virtual_address = sec->virtual_address;
        out_sec->virtual_size = sec->misc.virtual_size;

        // Read raw data (if applies)
        if (sec->raw_data_size) {
            assert(sec->raw_data_pos + sec->raw_data_size < file.length);
            out_sec->raw_data = (TB_Slice){ sec->raw_data_size, &file.data[sec->raw_data_pos] };
        }
    }

    return obj_file;
}
