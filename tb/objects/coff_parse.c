#include <tb_coff.h>
#include "parse_prelude.h"

// sanity checks
static_assert(sizeof(COFF_SectionHeader) == 40, "COFF Section header size != 40 bytes");
static_assert(sizeof(COFF_ImageReloc) == 10,    "COFF Image Relocation size != 10 bytes");
static_assert(sizeof(COFF_FileHeader) == 20,    "COFF File header size != 20 bytes");
static_assert(sizeof(COFF_Symbol) == 18,        "COFF Symbol size != 18 bytes");

bool tb_coff_parse_init(TB_COFF_Parser* restrict parser) {
    TB_Slice file = parser->file;

    if (file.length < sizeof(COFF_FileHeader)) return false;
    COFF_FileHeader* header = (COFF_FileHeader*) &parser->file.data[0];

    // locate string table (it spans until the end of the file)
    size_t string_table_pos = header->symbol_table + (header->symbol_count * sizeof(COFF_Symbol));
    if (file.length < string_table_pos) return false;

    parser->symbol_count = header->symbol_count;
    parser->symbol_table = header->symbol_table;
    parser->section_count = header->section_count;
    parser->string_table = (TB_Slice){
        .length = file.length - string_table_pos,
        .data   = &file.data[string_table_pos]
    };

    return true;
}

bool tb_coff_parse_section(TB_COFF_Parser* restrict parser, size_t i, TB_ObjectSection* restrict out_sec) {
    TB_Slice file = parser->file;
    size_t section_offset = sizeof(COFF_FileHeader) + (i * sizeof(COFF_SectionHeader));

    if (file.length < section_offset + sizeof(COFF_SectionHeader)) {
        return false;
    }

    COFF_SectionHeader* sec = (COFF_SectionHeader*) &file.data[section_offset];
    *out_sec = (TB_ObjectSection) { .flags = sec->characteristics };

    // Parse string table name stuff
    if (sec->name[0] == '/') {
        // string table access
        int offset = parse_decimal_int(7, &sec->name[1]);
        if (offset >= file.length) {
            return false;
        }

        const uint8_t* data = &parser->string_table.data[offset];
        out_sec->name = (TB_Slice){ data, ideally_fast_strlen((const char*) data) };
    } else {
        // normal inplace string
        size_t len = ideally_fast_strlen(sec->name);
        out_sec->name = (TB_Slice){ (uint8_t*) sec->name, len };
    }

    // Parse relocations
    if (sec->num_reloc > 0) {
        out_sec->relocation_offset = sec->pointer_to_reloc;
        out_sec->relocation_count = sec->num_reloc;
    }

    // Parse virtual region
    out_sec->virtual_address = sec->virtual_address;
    out_sec->virtual_size = sec->misc.virtual_size;

    // Read raw data (if applies)
    if (sec->raw_data_size) {
        assert(sec->raw_data_pos + sec->raw_data_size < file.length);
        out_sec->raw_data = (TB_Slice){ &file.data[sec->raw_data_pos], sec->raw_data_size };
    }

    return true;
}

TB_ObjectReloc tb_coff_parse_reloc(const COFF_ImageReloc* relocs, size_t i) {
    TB_ObjectReloc r = { 0 };
    switch (relocs[i].Type) {
        case IMAGE_REL_AMD64_ADDR32NB: r.type = TB_OBJECT_RELOC_ADDR32NB; break;
        case IMAGE_REL_AMD64_ADDR32:   r.type = TB_OBJECT_RELOC_ADDR32; break;
        case IMAGE_REL_AMD64_ADDR64:   r.type = TB_OBJECT_RELOC_ADDR64; break;
        case IMAGE_REL_AMD64_SECREL:   r.type = TB_OBJECT_RELOC_SECREL; break;
        case IMAGE_REL_AMD64_SECTION:  r.type = TB_OBJECT_RELOC_SECTION; break;

        case IMAGE_REL_AMD64_REL32:
        case IMAGE_REL_AMD64_REL32_1:
        case IMAGE_REL_AMD64_REL32_2:
        case IMAGE_REL_AMD64_REL32_3:
        case IMAGE_REL_AMD64_REL32_4:
        case IMAGE_REL_AMD64_REL32_5:
        r.type = TB_OBJECT_RELOC_REL32;
        break;

        default: tb_todo();
    }

    if (relocs[i].Type >= IMAGE_REL_AMD64_REL32 && relocs[i].Type <= IMAGE_REL_AMD64_REL32_5) {
        r.addend = -4 + (relocs[i].Type - IMAGE_REL_AMD64_REL32);
    }

    r.symbol_index    = relocs[i].SymbolTableIndex;
    r.virtual_address = relocs[i].VirtualAddress;
    return r;
}

TB_ObjectSymbolType classify_symbol_type(uint16_t st_class) {
    switch (st_class) {
        case 2:    return TB_OBJECT_SYMBOL_EXTERN;
        case 3:    return TB_OBJECT_SYMBOL_STATIC;
        case 6:    return TB_OBJECT_SYMBOL_STATIC;
        case 0x68: return TB_OBJECT_SYMBOL_SECTION;
        case 0x69: return TB_OBJECT_SYMBOL_WEAK_EXTERN;
        default: return TB_OBJECT_SYMBOL_UNKNOWN;
    }
}

size_t tb_coff_skim_symbol(TB_COFF_Parser* restrict parser, size_t i, TB_ObjectSymbol* restrict out_sym) {
    TB_Slice file = parser->file;
    size_t symbol_offset = parser->symbol_table + (i * sizeof(COFF_Symbol));

    if (file.length < symbol_offset + sizeof(COFF_Symbol)) {
        return 0;
    }

    COFF_Symbol* sym = (COFF_Symbol*) &file.data[symbol_offset];
    *out_sym = (TB_ObjectSymbol) {
        .ordinal = i,
        .type = sym->storage_class == 2 ? TB_OBJECT_SYMBOL_EXTERN : TB_OBJECT_SYMBOL_UNKNOWN,
        .section_num = sym->section_number,
        .value = sym->value
    };

    // Parse string table name stuff
    if (sym->long_name[0] == 0) {
        // string table access (read a cstring)
        // TODO(NeGate): bounds check this
        const uint8_t* data = &parser->string_table.data[sym->long_name[1]];
        out_sym->name = (TB_Slice){ data, ideally_fast_strlen((const char*) data) };
    } else {
        out_sym->name.data = sym->short_name;

        // normal inplace string
        uint64_t name;
        memcpy(&name, sym->short_name, 8);

        static const uint64_t mask = (~(uint64_t)0) / 255 * (uint64_t)(0);
        uint64_t x = name ^ mask;
        x = ((x - 0x0101010101010101ull) & ~x & 0x8080808080808080ull);
        if (x) {
            out_sym->name.length = ((__builtin_ffsll(x) / 8) - 1);
        } else {
            out_sym->name.length = 8;
        }
    }

    // TODO(NeGate): Process aux symbols
    if (sym->aux_symbols_count) {
        out_sym->extra = &sym[1];
    }

    return sym->aux_symbols_count + 1;
}

size_t tb_coff_parse_symbol(TB_COFF_Parser* restrict parser, size_t i, TB_ObjectSymbol* restrict out_sym) {
    TB_Slice file = parser->file;
    size_t symbol_offset = parser->symbol_table + (i * sizeof(COFF_Symbol));

    if (file.length < symbol_offset + sizeof(COFF_Symbol)) {
        return 0;
    }

    COFF_Symbol* sym = (COFF_Symbol*) &file.data[symbol_offset];
    *out_sym = (TB_ObjectSymbol) {
        .ordinal = i,
        .type = classify_symbol_type(sym->storage_class),
        .section_num = sym->section_number,
        .value = sym->value
    };

    // Parse string table name stuff
    if (sym->long_name[0] == 0) {
        // string table access (read a cstring)
        // TODO(NeGate): bounds check this
        const uint8_t* data = &parser->string_table.data[sym->long_name[1]];
        out_sym->name = (TB_Slice){ data, ideally_fast_strlen((const char*) data) };
    } else {
        out_sym->name.data = sym->short_name;

        // normal inplace string
        uint64_t name;
        memcpy(&name, sym->short_name, 8);

        static const uint64_t mask = (~(uint64_t)0) / 255 * (uint64_t)(0);
        uint64_t x = name ^ mask;
        x = ((x - 0x0101010101010101ull) & ~x & 0x8080808080808080ull);
        if (x) {
            out_sym->name.length = ((__builtin_ffsll(x) / 8) - 1);
        } else {
            out_sym->name.length = 8;
        }
    }

    // TODO(NeGate): Process aux symbols
    if (sym->aux_symbols_count) {
        out_sym->extra = &sym[1];

        // FOR_N(j, 0, sym->aux_symbols_count) {}
    }

    return sym->aux_symbols_count + 1;
}
