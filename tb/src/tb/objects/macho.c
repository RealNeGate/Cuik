#include "macho.h"

struct TB_ModuleExporter {
    const ICodeGen* code_gen;
    size_t write_pos;
};

#define WRITE(data, length_) write_data(e, output, length_, data)
static void write_data(TB_ModuleExporter* e, uint8_t* output, size_t length, const void* data) {
    memcpy(output + e->write_pos, data, length);
    e->write_pos += length;
}

TB_API TB_Exports tb_macho_write_output(TB_Module* m, const IDebugFormat* dbg) {
    TB_ModuleExporter* e = tb_platform_heap_alloc(sizeof(TB_ModuleExporter));
    memset(e, 0, sizeof(TB_ModuleExporter));

    e->code_gen = tb__find_code_generator(m);

    //TB_TemporaryStorage* tls = tb_tls_allocate();
    TB_Emitter string_table = { 0 };

    // function layout
    size_t text_section_size = tb_helper_get_text_section_layout(m, 0);

    // segments
    MO_Section64 sections[] = {
        {
            .sectname = { "__text" },
            .segname  = { "__TEXT" },
            .align    = 2,
            .size     = text_section_size,
            .flags    = S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS
        }
    };
    enum { NUMBER_OF_SECTIONS = COUNTOF(sections) };

    size_t output_size = sizeof(MO_Header64)
        + sizeof(MO_SymtabCmd)
        + sizeof(MO_SegmentCmd64)
        + (sizeof(MO_Section64) * NUMBER_OF_SECTIONS);

    MO_SegmentCmd64 segment_cmd = {
        .header = {
            .cmd = LC_SEGMENT_64,
            .cmdsize = sizeof(MO_SegmentCmd64) + sizeof(MO_Section64)*NUMBER_OF_SECTIONS,
        },
        .segname = { "__TEXT" },
        .nsects = NUMBER_OF_SECTIONS,
        .vmsize = text_section_size,
        .fileoff = output_size,
        .filesize = text_section_size
    };

    // layout section data
    sections[0].offset = output_size;
    output_size += text_section_size;

    // generate symbol table
    MO_SymtabCmd symtab_cmd = {
        .header = {
            .cmd     = LC_SYMTAB,
            .cmdsize = sizeof(MO_SymtabCmd)
        }
    };

    // count symbols
    {
        /*symtab_cmd.nsyms += m->compiled_function_count;

        FOREACH_N(i, 0, m->max_threads) {
            size_t external_len = pool_popcount(m->thread_info[i].externals);
            symtab_cmd.nsyms += external_len ? external_len - 1 : 0;
        }

        FOREACH_N(i, 0, m->max_threads) {
            symtab_cmd.nsyms += pool_popcount(m->thread_info[i].globals);
        }*/
    }

    MO_Header64 header = {
        .magic      = MH_MAGIC_64,
        .filetype   = MH_OBJECT,
        .ncmds      = 2,
        .sizeofcmds = symtab_cmd.header.cmdsize + segment_cmd.header.cmdsize,
        .flags      = 0x2000
    };

    // fill in CPU type and subtype based on target
    switch (m->target_arch) {
        case TB_ARCH_X86_64:  header.cputype = CPU_TYPE_X86_64; header.cpusubtype = 3; break;
        case TB_ARCH_AARCH64: header.cputype = CPU_TYPE_AARCH64; header.cpusubtype = 0; break;
        default: tb_todo();
    }

    //size_t load_cmds_start = sizeof(MO_Header64);
    // fprintf(stderr, "TB warning: Mach-O output isn't ready yet :p sorry\n");

    // Allocate memory now
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);

    // General layout is:
    //        HEADER
    //        LOAD COMMAND
    //        LOAD COMMAND
    //        ...
    //        SEGMENT
    //        SEGMENT
    //        ...
    WRITE(&header, sizeof(header));
    WRITE(&symtab_cmd, sizeof(symtab_cmd));
    WRITE(&segment_cmd, sizeof(segment_cmd));
    WRITE(&sections, sizeof(MO_Section64) * NUMBER_OF_SECTIONS);

    e->write_pos = tb_helper_write_text_section(e->write_pos, m, output, sections[0].offset);

    // emit section contents
    FOREACH_N(i, 0, NUMBER_OF_SECTIONS) {

    }

    // fwrite(string_table.data, string_table.count, 1, f);

    tb_platform_heap_free(string_table.data);
    tb_platform_heap_free(e);
    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}
