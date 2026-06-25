// TODO:
// * Process COMDAT rules correctly.
#include "linker.h"
#include "../tb/objects/coff.h"

#include <ctype.h>
#include <tb_coff.h>
#include <file_map.h>

enum {
    IMP_PREFIX_LEN = sizeof("__imp_") - 1,
    PDB_BLOCK_SIZE = 4096,
};

typedef struct {
    uint32_t offset;
    TB_LinkerSegment* segment;
} PE_BaseReloc;

// https://llvm.org/docs/PDB/MsfFile.html#the-stream-directory
typedef struct {
    char file_magic[32];
    uint32_t block_size;
    uint32_t free_block_map_block;
    uint32_t num_blocks;
    uint32_t num_directory_bytes;
    uint32_t unknown;
    uint32_t dir_blocks[];
} PDB_SuperBlock;

// serialized form
typedef struct {
    uint32_t version;
    uint32_t signature;
    uint32_t age;
    uint32_t unique_id[4];
} PDB_InfoHeader;

// https://llvm.org/docs/PDB/DbiStream.html
typedef struct {
    // I have no idea why they didn't make this
    // part of the streams consistent btw. Info
    // does ver+sign+age, here it's sign+ver+age
    uint32_t signature;
    uint32_t version;
    uint32_t age;

    uint16_t global_stream; // StreamIndex
    uint16_t build_num;

    uint16_t public_stream; // StreamIndex
    uint16_t pdb_dll_version;
    uint16_t sym_record_stream; // StreamIndex
    uint16_t pdb_dll_rebuild;
    uint32_t mod_info_size;
    uint32_t section_contrib_size;
    uint32_t section_map_size;
    uint32_t source_info_size;
    uint32_t type_server_size;
    uint32_t mfc_type_server; // StreamIndex
    uint32_t optional_debug_header_size;
    uint32_t ec_subsystem_size;

    uint16_t flags;
    uint16_t machine;

    uint32_t pad;
    uint8_t data[]; // PDB_ModuleInfo[]
} PDB_DBIHeader;

// https://llvm.org/docs/PDB/DbiStream.html
typedef struct {
    uint32_t unused1;
    struct {
        uint16_t section;
        char     padding1[2];
        int32_t  offset;
        int32_t  size;
        uint32_t characteristics;
        uint16_t module_index;
        char     padding2[2];
        uint32_t data_crc;
        uint32_t reloc_crc;
    } section_contr;
    uint16_t flags;
    uint16_t module_sym_stream;
    uint32_t sym_size;
    uint32_t c11_size;
    uint32_t c13_size;
    uint16_t source_file_count;
    char     padding[2];
    uint32_t unused2;
    uint32_t source_file_name_index;
    uint32_t pdb_file_path_name_index;
    char     module_name[];
} PDB_ModuleInfo;

typedef struct {
    size_t size;
    // We assume that all blocks are contiguous for now
    size_t first_block;
} DebugStream;

typedef struct {
    TB_Linker* linker;
    TB_LinkerObject* lib;
    TB_ArchiveFileParser* parser;
    size_t symbol_i;
    size_t string_i;
    size_t count;
} LazyImportTask;

const static uint8_t dos_stub[] = {
    // header
    0x4d,0x5a,0x78,0x00,0x01,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x78,0x00,0x00,0x00,

    // machine code
    0x0e,0x1f,0xba,0x0e,0x00,0xb4,0x09,0xcd,0x21,0xb8,0x01,0x4c,0xcd,0x21,0x54,0x68,
    0x69,0x73,0x20,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d,0x20,0x63,0x61,0x6e,0x6e,0x6f,
    0x74,0x20,0x62,0x65,0x20,0x72,0x75,0x6e,0x20,0x69,0x6e,0x20,0x44,0x4f,0x53,0x20,
    0x6d,0x6f,0x64,0x65,0x2e,0x24,0x00,0x00
};

static int symbol_cmp(const void* a, const void* b) {
    const TB_ObjectSymbol* sym_a = (const TB_ObjectSymbol*)a;
    const TB_ObjectSymbol* sym_b = (const TB_ObjectSymbol*)b;

    return sym_a->ordinal - sym_b->ordinal;
}

static int compare_symbols(const void* a, const void* b) {
    TB_LinkerSymbol* sym_a = *(TB_LinkerSymbol**)a;
    TB_LinkerSymbol* sym_b = *(TB_LinkerSymbol**)b;

    return tb__get_symbol_rva(sym_a) - tb__get_symbol_rva(sym_b);
}

static int compare_rva(const void* a, const void* b) {
    const uint32_t* aa = a;
    const uint32_t* bb = b;

    if (*aa < *bb) return -1;
    if (*aa > *bb) return 1;
    return 0;
}

// true if we replace the old one
static bool process_comdat(int select, TB_LinkerSectionPiece* old_p, TB_LinkerSectionPiece* new_p) {
    switch (select) {
        case 2: return false;                     // any
        case 6: return new_p->size > old_p->size; // largest
        default: tb_todo();
    }
}

static bool strprefix(const char* str, const char* pre, size_t len) {
    size_t prelen = strlen(pre);
    return tb_string_case_cmp(pre, str, len < prelen ? len : prelen) == 0;
}

static bool strsuffix(const uint8_t* str, const char* suf, size_t len) {
    size_t suflen = strlen(suf);
    return len >= suflen && memcmp(&str[len - suflen], suf, suflen) == 0;
}

static void parse_directives(TB_Linker* l, const uint8_t* curr, const uint8_t* end_directive) {
    while (curr != end_directive && *curr == ' ') curr++;

    while (curr != end_directive) {
        const uint8_t* end = curr;
        while (end != end_directive && *end != ' ') end++;

        // printf("directive: %.*s\n", (int) (end - curr), curr);

        if (*curr == 0 || curr == end_directive) {
            break;
        } else if (strprefix((const char*) curr, "/merge:", end - curr)) {
            curr += sizeof("/merge:")-1;

            // printf("merge: %.*s\n", (int) (end - curr), curr);

            const uint8_t* equals = curr;
            while (*equals && *equals != '=') equals++;

            if (*equals == '=') {
                TB_LinkerCmd cmd = {
                    .from = { curr, equals - curr },
                    .to   = { equals + 1, (end - equals) - 1 },
                };

                // low contention, don't care
                mtx_lock(&l->lock);
                dyn_array_put(l->merges, cmd);
                mtx_unlock(&l->lock);
            }
        } else if (strprefix((const char*) curr, "/include:", end - curr)) {
            curr += sizeof("/include:")-1;

            // forcibly include symbol
            tb_linker_import_symbol(l, (TB_Slice){ curr, end - curr });
        } else if (strprefix((const char*) curr, "/disallowlib:", end - curr)) {
            curr += sizeof("/disallowlib:")-1;

            int len = end - curr;
            if (curr[0] == '"') {
                curr += 1;
                len -= 2;
            }

            // force the lib list to think this library is already opened
            char* str = tb_arena_alloc(&linker_perm_arena, len + 1);
            memcpy(str, curr, len + 1);
            str[len] = 0;

            if (strhs_intern(&l->libs, str) != str) {
                tb_arena_free(&linker_perm_arena, str, len + 1);
            }
        } else if (strprefix((const char*) curr, "/defaultlib:", end - curr)) {
            curr += sizeof("/defaultlib:")-1;

            int len = end - curr;
            if (curr[0] == '"') {
                curr += 1;
                len -= 2;
            }

            char* path = tb_arena_alloc(&linker_perm_arena, FILENAME_MAX);
            snprintf(path, FILENAME_MAX, "%.*s", len, curr);

            // low contention, don't care
            mtx_lock(&l->lock);
            dyn_array_put(l->default_libs, path);
            mtx_unlock(&l->lock);
        } else if (strprefix((const char*) curr, "/alternatename:", end - curr)) {
            curr += sizeof("/alternatename:")-1;

            // If the symbol isn't defined yet
            const uint8_t* equals = curr;
            while (*equals && *equals != '=') equals++;

            if (*equals == '=') {
                // printf("alternate: %.*s\n", (int) (end - curr), curr);

                TB_LinkerCmd cmd = {
                    .from = { curr, equals - curr },
                    .to   = { equals + 1, (end - equals) - 1 },
                };

                // low contention, don't care
                mtx_lock(&l->lock);
                dyn_array_put(l->alternate_names, cmd);
                mtx_unlock(&l->lock);
            }
        } else {
            // log_warn("unknown linker directive: %.*s", (int) (end - curr), curr);
        }

        curr = end+1;
    }
}

FileMap pe_find_lib(TB_Linker* l, const char* file_name, char* path) {
    FileMap fm = open_file_map_read(file_name);
    if (fm.data !=  NULL) {
        strncpy(path, file_name, FILENAME_MAX);
        return fm;
    }

    dyn_array_for(j, l->libpaths) {
        snprintf(path, FILENAME_MAX, "%s/%s", l->libpaths[j], file_name);
        fm = open_file_map_read(path);
        if (fm.data != NULL) {
            break;
        }
        path[0] = 0;
    }

    if (path[0] == 0) {
        printf("tblink: could not find library: %s\n", file_name);
        dyn_array_for(j, l->libpaths) {
            snprintf(path, FILENAME_MAX, "%s/%s", l->libpaths[j], file_name);
            printf("  searched at %s\n", path);
        }
        return (FileMap){ 0 };
    }
    return fm;
}

// insert into global symbol table
static TB_LinkerSymbol* insert_global_symbol(TB_Linker* l, TB_LinkerSymbol* s, bool is_static) {
    if (s != NULL && !is_static) {
        TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
        if (new_s != s) {
            tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
            return new_s;
        }
    }

    return s;
}

static TB_Slice cstr_into_slice(const char* str) {
    size_t len = strlen(str);
    return (TB_Slice){ (const uint8_t*) str, len };
}

void pe_append_module(TPool* pool, void** args) {
    TB_LinkerObject* obj = args[0];
    TB_Linker* l = obj->linker;

    cuikperf_region_start("module", NULL);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_Module* m = obj->module;
    ExportList exports;
    CUIK_TIMED_BLOCK("layout section") {
        exports = tb_module_layout_sections(m);
    }

    // accumulate all sections
    DynArray(TB_ModuleSection) sections = m->sections;

    const ICodeGen* restrict code_gen = tb_codegen_info(m);
    tb__layout_relocations(m, sections, code_gen, 0, sizeof(TB_LinkerReloc));

    size_t reloc_cap = 0;
    dyn_array_for(i, sections) {
        reloc_cap += sections[i].reloc_count;
    }

    TB_LinkerReloc* relocs = tb_arena_alloc(&linker_perm_arena, reloc_cap * sizeof(TB_LinkerReloc));

    // mark correct flags on sections
    #define ON(mask, a, b) flags |= (sections[i].flags & mask ? (a) : (b))
    size_t reloc_count = 0;
    dyn_array_for(i, sections) {
        uint32_t flags = TB_COFF_SECTION_READ;
        ON(TB_MODULE_SECTION_WRITE, TB_COFF_SECTION_WRITE, 0);
        ON(TB_MODULE_SECTION_EXEC,  TB_COFF_SECTION_EXECUTE | TB_COFF_SECTION_CODE, TB_COFF_SECTION_INIT);
        if (sections[i].comdat.type != 0) flags |= TB_COFF_SECTION_COMDAT;

        // fill out the section data
        unsigned char* raw_data = tb_arena_alloc(&linker_perm_arena, sections[i].total_size);
        tb_helper_write_section(m, 0, &sections[i], raw_data, 0);

        // add to global pool of sections
        TB_LinkerSection* ls = tb_linker_find_or_create_section(l, strlen(sections[i].name), sections[i].name, flags & ~0x00F00000);

        TB_LinkerSectionPiece* p;
        p = tb_linker_append_piece(ls, PIECE_BUFFER, sections[i].total_size, obj);
        p->buffer = raw_data;
        p->buffer_size = sections[i].total_size;
        if (sections[i].comdat.type != 0) {
            p->flags |= TB_LINKER_PIECE_COMDAT;
        }
        if (sections[i].flags & TB_MODULE_SECTION_EXEC) {
            p->flags |= TB_LINKER_PIECE_CODE;
        }
        sections[i].piece = p;

        // convert relocations into the linker-friendly format
        p->reloc_count = sections[i].reloc_count;
        p->relocs = &relocs[reloc_count];
        reloc_count += sections[i].reloc_count;
    }
    #undef ON

    // import symbols
    FOR_N(i, 0, exports.count) {
        TB_External* e = exports.data[i];

        TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
        *s = (TB_LinkerSymbol){
            .name   = cstr_into_slice(e->super.name),
            .tag    = TB_LINKER_SYMBOL_UNKNOWN,
        };
        e->super.address = insert_global_symbol(l, s, e->super.linkage == TB_LINKAGE_PRIVATE);
    }

    // populate symbols
    dyn_array_for(i, sections) {
        TB_LinkerSectionPiece* p = sections[i].piece;
        if (p == NULL) { continue; }

        DynArray(TB_FunctionOutput*) funcs = sections[i].funcs;
        DynArray(TB_Global*) globals = sections[i].globals;

        dyn_array_for(j, funcs) {
            TB_FunctionOutput* func_out = funcs[j];
            size_t source_offset = func_out->code_pos;

            TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
            *s = (TB_LinkerSymbol){
                .name   = cstr_into_slice(func_out->parent->super.name),
                .tag    = TB_LINKER_SYMBOL_NORMAL,
                .normal = { p, source_offset }
            };

            if (p->flags & TB_LINKER_PIECE_COMDAT) {
                s->comdat = TB_LINKER_COMDAT_ANY;
            }
            func_out->parent->super.address = insert_global_symbol(l, s, func_out->parent->super.linkage == TB_LINKAGE_PRIVATE);
        }

        dyn_array_for(j, globals) {
            TB_Global* global = globals[j];
            size_t source_offset = global->pos;

            TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
            *s = (TB_LinkerSymbol){
                .name   = cstr_into_slice(global->super.name),
                .tag    = TB_LINKER_SYMBOL_NORMAL,
                .normal = { p, source_offset }
            };
            global->super.address = insert_global_symbol(l, s, global->super.linkage == TB_LINKAGE_PRIVATE);
        }
    }

    // populate relocations
    reloc_count = 0;
    dyn_array_for(i, sections) {
        DynArray(TB_FunctionOutput*) funcs = sections[i].funcs;
        DynArray(TB_Global*) globals = sections[i].globals;

        dyn_array_for(j, funcs) {
            TB_FunctionOutput* func_out = funcs[j];
            size_t source_offset = func_out->code_pos;

            for (TB_SymbolPatch* p = func_out->first_patch; p; p = p->next) {
                if (p->internal) continue;

                size_t actual_pos = source_offset + p->pos;

                TB_Symbol* target = p->target;
                if (target->tag == TB_SYMBOL_EXTERNAL) {
                    TB_Symbol* resolved = atomic_load_explicit(&((TB_External*) target)->resolved, memory_order_relaxed);
                    if (resolved) target = resolved;
                }

                relocs[reloc_count++] = (TB_LinkerReloc){
                    .src_offset = actual_pos,
                    .type = p->type,
                    .target = target->address,
                    .addend = -4,
                };
            }
        }

        dyn_array_for(j, globals) {
            TB_Global* g = globals[j];

            FOR_N(k, 0, g->obj_count) {
                size_t actual_pos = g->pos + g->objects[k].offset;
                if (g->objects[k].type == TB_INIT_OBJ_RELOC) {
                    const TB_Symbol* s = g->objects[k].reloc;
                    relocs[reloc_count++] = (TB_LinkerReloc){
                        .src_offset = actual_pos,
                        .type = TB_OBJECT_RELOC_ADDR64,
                        .target = s->address
                    };
                }
            }
        }
    }

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }

    cuikperf_region_end();
}

void pe_append_object(TPool* pool, void** args) {
    TB_LinkerObject* obj = args[0];

    size_t slash = 0;
    FOR_REV_N(i, 0, obj->name.length) {
        if (obj->name.data[i] == '/' || obj->name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }

    cuikperf_region_start2("object", obj->name.length - slash, (const char*) obj->name.data + slash);
    // printf("tb-link: Reading (%.*s), %#llx:\n", (int) (obj->name.length - slash), (const char*) obj->name.data + slash, obj->time);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_Linker* l = obj->linker;
    TB_Slice name = obj->name;
    TB_Slice content = obj->content;

    TB_COFF_Parser parser = { name, content };
    tb_coff_parse_init(&parser);

    TB_ArenaSavepoint sp = tb_arena_save(&linker_tmp_arena);

    // Apply all sections (generate lookup for sections based on ordinals)
    TB_LinkerSectionPiece *text_piece = NULL, *pdata_piece = NULL, *debug_piece = NULL;
    TB_ObjectSection* sections = tb_arena_alloc(&linker_tmp_arena, parser.section_count * sizeof(TB_ObjectSection));

    uint64_t order = obj->time;
    CUIK_TIMED_BLOCK("parse sections") {
        FOR_N(i, 0, parser.section_count) {
            TB_ObjectSection* restrict s = &sections[i];
            tb_coff_parse_section(&parser, i, s);

            int dollar = find_char(s->name, '$');
            size_t drectve_len = sizeof(".drectve")-1;
            if (dollar >= drectve_len && memcmp(s->name.data, ".drectve", drectve_len) == 0) {
                // printf("tb-link: Directives: %.*s\n", (int) s->raw_data.length, s->raw_data.data);
                parse_directives(l, s->raw_data.data, s->raw_data.data + s->raw_data.length);
                continue;
            }

            // remove all the alignment flags, they don't appear in linker sections
            TB_LinkerSection* ls = tb_linker_find_or_create_section(l, s->name.length, (const char*) s->name.data, s->flags & ~0x00F00000);

            if (s->flags & (IMAGE_SCN_LNK_REMOVE | IMAGE_SCN_MEM_DISCARDABLE)) {
                ls->generic_flags |= TB_LINKER_SECTION_DISCARD;
            }

            TB_LinkerSectionPiece* p;
            p = tb_linker_append_piece(ls, PIECE_BSS, s->raw_data.length, obj);
            if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                p->kind = PIECE_BUFFER;
                p->buffer = s->raw_data.data;
                p->buffer_size = s->raw_data.length;
            }
            if (s->flags & 0x00F00000) {
                // go stare at the table, it'll make sense
                p->align_log2 = ((s->flags >> 20) & 0xF) - 1;
            }
            s->user_data = p;
            p->order = order + i;
            p->flags = (s->flags & IMAGE_SCN_MEM_EXECUTE) ? TB_LINKER_PIECE_CODE : 0;
            p->reloc_count = s->relocation_count;
            p->relocs = &parser.file.data[s->relocation_offset];
            if (s->flags & IMAGE_SCN_LNK_COMDAT) {
                p->flags |= TB_LINKER_PIECE_COMDAT;
            } else {
                if (dollar == 5 && memcmp(s->name.data, ".text", 5) == 0) {
                    // assert(text_piece == NULL);
                    text_piece = p;
                } else if (dollar == 6 && memcmp(s->name.data, ".pdata", 6) == 0) {
                    // assert(pdata_piece == NULL);
                    pdata_piece = p;
                } else if (s->name.length == 8 && memcmp(s->name.data, ".debug$S", 8) == 0) {
                    obj->debug_s = p;
                } else if (s->name.length == 8 && memcmp(s->name.data, ".debug$T", 8) == 0) {
                    obj->debug_t = p;
                }
            }
        }
    }

    // associate the debug and pdata with the text
    if (text_piece && pdata_piece) {
        tb_linker_associate(l, text_piece, pdata_piece);
    }

    // append all symbols
    size_t sym_count = 0;
    TB_ObjectSymbol* syms = tb_arena_alloc(&linker_tmp_arena, parser.symbol_count * sizeof(TB_ObjectSymbol));
    TB_LinkerSymbol** symbol_map = tb_arena_alloc(&linker_perm_arena, parser.symbol_count * sizeof(TB_LinkerSymbol*));

    DynArray(TB_ObjectSymbol*) weak_syms = NULL;
    NL_Map(int, COFF_AuxSectionSymbol*) comdat_sections = NULL;
    CUIK_TIMED_BLOCK("apply symbols") {
        size_t i = 0;
        while (i < parser.symbol_count) {
            TB_ObjectSymbol* restrict sym = &syms[sym_count++];
            size_t c = tb_coff_parse_symbol(&parser, i, sym);
            TB_ASSERT(c > 0);

            TB_LinkerSymbol* s = NULL;
            if (sym->section_num > 0) {
                TB_ObjectSection* sec = &sections[sym->section_num - 1];

                bool is_section = false;
                if (sym->type == TB_OBJECT_SYMBOL_STATIC && sym->value == 0) {
                    if (sec->name.length == sym->name.length && memcmp(sec->name.data, sym->name.data, sym->name.length) == 0) {
                        is_section = true;

                        // printf("L: %d %zu %.*s\n", sym->name.length, (int) sym->name.length, sym->name.data);

                        // COMDAT is how linkers handle merging of inline functions in C++
                        if ((sec->flags & IMAGE_SCN_LNK_COMDAT)) {
                            // the next symbol in the section is the COMDAT symbol
                            nl_map_put(comdat_sections, sym->section_num, sym->extra);
                        }

                        // sections without a piece are ok
                        if (sec->user_data == NULL) {
                            goto skip;
                        }
                    }
                }

                TB_LinkerSectionPiece* p = sec->user_data;
                TB_ASSERT(p != NULL);

                s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name   = sym->name,
                    .tag    = TB_LINKER_SYMBOL_NORMAL,
                    .normal = { p, sym->value }
                };

                ptrdiff_t search = nl_map_get(comdat_sections, sym->section_num);
                COFF_AuxSectionSymbol* comdat_aux = search >= 0 ? comdat_sections[search].v : NULL;

                TB_ASSERT(sym->type != TB_OBJECT_SYMBOL_WEAK_EXTERN);
                if (sym->type == TB_OBJECT_SYMBOL_EXTERN && (!is_section && comdat_aux)) {
                    if (comdat_aux->selection == 1) {
                        s->comdat = TB_LINKER_COMDAT_NODUP;
                    } else if (comdat_aux->selection == 5) {
                        s->comdat = TB_LINKER_COMDAT_ASSOCATIVE;

                        TB_ObjectSection* assoc = &sections[comdat_aux->number - 1];
                        TB_LinkerSectionPiece* assoc_piece = (TB_LinkerSectionPiece*) assoc->user_data;
                        tb_linker_associate(l, assoc_piece, sec->user_data);
                    } else {
                        s->comdat = TB_LINKER_COMDAT_ANY;
                    }

                    // nl_map_remove(comdat_sections, sym->section_num);
                }
            } else if (sym->type == TB_OBJECT_SYMBOL_EXTERN || sym->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                // symbols without a section number are proper externals (ones defined somewhere
                // else that we might want)
                s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name = sym->name,
                    .tag  = TB_LINKER_SYMBOL_UNKNOWN,
                };

                if (sym->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                    dyn_array_put(weak_syms, sym);
                }
            } else {
                // log_debug("skipped %.*s", (int) sym->name.length, sym->name.data);
            }

            // insert into global symbol table
            if (s != NULL && sym->type != TB_OBJECT_SYMBOL_STATIC) {
                TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
                if (new_s != s) {
                    tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
                    s = new_s;
                }
            }
            sym->user_data = s;

            skip:;
            // write into symbol mapping (including whatever aux data "padding")
            symbol_map[i] = s;
            FOR_N(j, 1, c) { symbol_map[i+j] = NULL; }
            i += c;
        }
    }

    // broadcast to all sections how to find symbols (for relocation resolution later)
    FOR_N(i, 0, parser.section_count) {
        TB_ObjectSection* restrict s = &sections[i];
        TB_LinkerSectionPiece* p = s->user_data;

        if (p != NULL) {
            p->symbol_map = symbol_map;
        }
    }

    if (dyn_array_length(weak_syms) > 0) {
        dyn_array_for(i, weak_syms) {
            TB_ObjectSymbol* src_symbol = weak_syms[i];

            // weak aux
            uint32_t* weak_sym = src_symbol->extra;
            TB_ObjectSymbol* restrict alt_sym = bsearch(
                &(TB_ObjectSymbol){ .ordinal = *weak_sym },
                syms, sym_count, sizeof(TB_ObjectSymbol),
                symbol_cmp
            );

            tb_linker_symbol_weak(l, src_symbol->user_data, alt_sym->user_data);
        }
        dyn_array_destroy(weak_syms);
    }
    nl_map_free(comdat_sections);
    tb_arena_restore(&linker_tmp_arena, sp);

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }

    cuikperf_region_end();
}

static void lazy_import_task(TPool* pool, void** args) {
    cuikperf_region_start("lazy parse", NULL);
    LazyImportTask* task = args[0];

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_ArchiveFileParser* parser = task->parser;
    TB_LinkerObject* lib = task->lib;
    TB_Linker* l = task->linker;
    uint64_t t = lib->time;

    char* strtab = parser->symbol_strtab;
    size_t j = task->string_i;
    FOR_N(i, task->symbol_i, task->symbol_i + task->count) {
        uint16_t offset_index = parser->symbols[i] - 1;

        const char* name = &strtab[j];
        size_t len = ideally_fast_strlen(name);

        j += len + 1;
        if (tb_archive_member_is_short(parser, offset_index)) {
            continue;
        }

        TB_ArchiveEntry e = tb_archive_member_get(parser, offset_index);
        assert(e.content.length);

        // We don't *really* care about this info beyond nicer errors (use an arena tho)
        TB_LinkerObject* obj_file = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
        *obj_file = (TB_LinkerObject){ e.name, l, e.content, t + offset_index*65536, lib };

        TB_LinkerObject* k = namehs_intern(&l->objects, obj_file);
        if (k != obj_file) {
            tb_arena_free(&linker_perm_arena, k, sizeof(TB_LinkerObject));
            obj_file = k;
        }

        // printf("%s : %u : %#x (%.*s)\n", name, offset_index, parser->members[offset_index], (int) e.name.length, e.name.data);
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
    }

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }

    cuikperf_region_end();
}

void pe_append_library(TPool* pool, void** args) {
    TB_LinkerObject* lib = args[0];

    size_t slash = 0;
    FOR_REV_N(i, 0, lib->name.length) {
        if (lib->name.data[i] == '/' || lib->name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }

    cuikperf_region_start2("library", lib->name.length - slash, (const char*) lib->name.data + slash);
    log_debug("linking against %.*s %#llx", (int) lib->name.length, lib->name.data, lib->time);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

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

        if (l->jobs.pool != NULL) {
            #if CUIK_ALLOW_THREADS
            TB_ArchiveFileParser* p = tb_arena_alloc(&linker_perm_arena, sizeof(TB_ArchiveFileParser));
            *p = ar_parser;

            size_t i = 0, str_head = 0;
            for (size_t i = 0; i < ar_parser.symbol_count; i += 250) {
                size_t limit = i + 250;
                if (limit > ar_parser.symbol_count) {
                    limit = ar_parser.symbol_count;
                }

                l->jobs.count += 1;
                LazyImportTask* task = tb_arena_alloc(&linker_perm_arena, sizeof(LazyImportTask));
                *task = (LazyImportTask){ l, lib, p, i, str_head, limit - i };
                tpool_add_task(l->jobs.pool, lazy_import_task, task);

                // Skip strings
                FOR_N(j, i, limit) {
                    uint16_t offset_index = ar_parser.symbols[j] - 1;
                    const char* name = &strtab[str_head];
                    str_head += ideally_fast_strlen(name) + 1;
                }
            }
            #else
            abort(); // Unreachable
            #endif
        } else {
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

                // printf("%s : %u : %#x (%.*s)\n", name, offset_index, ar_parser.members[offset_index], (int) e.name.length, e.name.data);
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
    }

    // we get a lot of imports to the same table in the same LIB (think of
    // kernel32.lib being completely kernel32.dll imports), because of this
    // we keep the lock open across archive entry iteration for like 1000
    // ops, it's manual lock elision.
    CUIK_TIMED_BLOCK("imports") {
        int imp_ticker = 0;
        ImportTable* imp_cache = NULL;
        FOR_N(i, 0, ar_parser.member_count) {
            if (!tb_archive_member_is_short(&ar_parser, i)) { continue; }
            TB_ArchiveEntry e = tb_archive_member_get(&ar_parser, i);

            if (imp_cache) {
                imp_ticker -= 1;
                if (imp_ticker == 0) {
                    mtx_unlock(&imp_cache->lock);
                    imp_cache = NULL;
                }
            }

            // import from DLL:
            //   we don't lock up to insert a new import table but we
            //   do to fill it up, this is probably fine since the filling
            //   process is most likely done by one thread while multiple
            //   threads might be poking multiple separate import tables.
            ImportTable* imp = NULL;
            if (imp_cache == NULL || imp_cache->libpath.length != e.name.length || memcmp(imp_cache->libpath.data, e.name.data, e.name.length) != 0) {
                if (imp_cache != NULL) {
                    mtx_unlock(&imp_cache->lock);
                }

                imp = tb_arena_alloc(&linker_perm_arena, sizeof(ImportTable));
                *imp = (ImportTable){ .libpath = e.name };
                mtx_init(&imp->lock, mtx_plain);

                ImportTable* old = namehs_intern(&l->imports, imp);
                if (old != imp) {
                    tb_arena_free(&linker_perm_arena, imp, sizeof(ImportTable));
                    imp = old;
                }

                // insert thunk
                mtx_lock(&imp->lock);
                imp_cache = imp;
                imp_ticker = 2000;

                if (imp->thunks == NULL) {
                    imp->thunks = dyn_array_create(TB_LinkerSymbol*, 4096);
                }
            } else {
                imp = imp_cache;
            }

            // printf("Import: %.*s (%d)\n", (int) e.import_name.length, e.import_name.data, e.ordinal);

            cuikperf_region_start2("archive", e.import_name.length, (const char*) e.import_name.data);
            // make __imp_ form which refers to raw address
            size_t newlen = e.import_name.length + sizeof("__imp_") - 1;
            uint8_t* newstr = tb_arena_alloc(&linker_perm_arena, newlen);
            memcpy(newstr, "__imp_", sizeof("__imp_"));
            memcpy(newstr + sizeof("__imp_") - 1, e.import_name.data, e.import_name.length);
            newstr[newlen] = 0;

            TB_LinkerSymbol* import_sym = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
            *import_sym = (TB_LinkerSymbol){
                .name   = { newstr, newlen },
                .tag    = TB_LINKER_SYMBOL_IMPORT,
                .import = { .table = imp, .ordinal = e.ordinal }
            };

            TB_LinkerSymbol* new_sym = tb_linker_symbol_insert(l, import_sym);
            if (new_sym == import_sym) {
                // first time we're importing this symbol, swag
                import_sym = new_sym;

                // make the thunk-like symbol
                TB_LinkerSymbol* sym = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *sym = (TB_LinkerSymbol){
                    .name   = e.import_name,
                    .tag    = TB_LINKER_SYMBOL_THUNK,
                    .thunk  = import_sym
                };
                tb_linker_symbol_insert(l, sym);
                dyn_array_put(imp->thunks, import_sym);
            } else {
                tb_arena_free(&linker_perm_arena, import_sym, sizeof(TB_LinkerSymbol));
            }
            cuikperf_region_end();
        }

        if (imp_cache) {
            mtx_unlock(&imp_cache->lock);
        }
    }

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }
    cuikperf_region_end();
}

static void pe_parse_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, size_t reloc_i, TB_LinkerReloc* out_reloc) {
    TB_ObjectReloc rel = tb_coff_parse_reloc(p->relocs, reloc_i);
    out_reloc->src_offset = rel.virtual_address;
    out_reloc->type       = rel.type;
    out_reloc->addend     = rel.addend;
    out_reloc->target     = p->symbol_map[rel.symbol_index];
}

static int compare_name(const void* a, const void* b) {
    const TB_Slice* aa = *(const TB_Slice**) a;
    const TB_Slice* bb = *(const TB_Slice**) b;

    if (aa->length != bb->length) {
        return aa->length - bb->length;
    }
    return memcmp(aa->data, bb->data, aa->length);
}

// returns the two new section pieces for the IAT and ILT
static COFF_ImportDirectory* gen_imports(TB_Linker* l, PE_ImageDataDirectory* imp_dir, PE_ImageDataDirectory* iat_dir) {
    // cull unused imports
    size_t j = 0;
    size_t import_entry_count = 0;

    DynArray(ImportTable*) sorted_imports = dyn_array_create(ImportTable*, 32);
    nbhs_for(e, &l->imports) {
        ImportTable* imp = *e;
        DynArray(TB_LinkerSymbol*) tbl = imp->thunks;

        // prune dead thunks
        size_t cnt = 0;
        dyn_array_for(k, tbl) if (tbl[k]->flags & TB_LINKER_SYMBOL_USED) {
            tbl[cnt++] = tbl[k];
        }
        dyn_array_set_length(tbl, cnt);
        imp->thunks = tbl;

        if (cnt > 0) {
            // there's an extra NULL terminator for the import entry lists
            import_entry_count += cnt + 1;

            CUIK_TIMED_BLOCK("sort import syms") {
                qsort(tbl, dyn_array_length(tbl), sizeof(TB_LinkerSymbol*), compare_name);
            }

            dyn_array_put(sorted_imports, imp);
        }
    }

    CUIK_TIMED_BLOCK("sort import tables") {
        qsort(sorted_imports, dyn_array_length(sorted_imports), sizeof(ImportTable*), compare_name);
        l->sorted_imports = sorted_imports;
    }

    if (import_entry_count == 0) {
        *imp_dir = (PE_ImageDataDirectory){ 0 };
        *iat_dir = (PE_ImageDataDirectory){ 0 };
        return NULL;
    }

    // Generate import thunks
    uint32_t thunk_id_counter = 0;
    size_t import_dir_count = 0;
    l->trampolines = (TB_Emitter){ 0 };
    dyn_array_for(i, sorted_imports) {
        ImportTable* imp = sorted_imports[i];
        // printf("IMPORT %.*s\n", (int) imp->libpath.length, imp->libpath.data);

        dyn_array_for(j, imp->thunks) {
            imp->thunks[j]->import.ds_address = l->trampolines.count;
            imp->thunks[j]->import.thunk_id = thunk_id_counter++;

            // TODO(NeGate): This trampoline is x64 specific, we should
            // implement a system to separate this from the core PE export
            // code.
            tb_out1b(&l->trampolines, 0xFF);
            tb_out1b(&l->trampolines, 0x25);
            // we're gonna relocate this to map onto an import thunk later
            tb_out4b(&l->trampolines, 0);
        }
        import_dir_count++, thunk_id_counter++;
    }

    ////////////////////////////////
    // Generate import table
    ////////////////////////////////
    size_t import_dir_size = (1 + import_dir_count) * sizeof(COFF_ImportDirectory);
    size_t iat_size = import_entry_count * sizeof(uint64_t);
    size_t total_size = import_dir_size + 2*iat_size;
    dyn_array_for(i, sorted_imports) {
        ImportTable* imp = sorted_imports[i];

        total_size += imp->libpath.length + 1;
        dyn_array_for(j, imp->thunks) {
            TB_LinkerSymbol* t = imp->thunks[j];
            total_size += (t->name.length - IMP_PREFIX_LEN) + 3;
        }
    }

    uint8_t* output = cuik_malloc(total_size);

    COFF_ImportDirectory* import_dirs = (COFF_ImportDirectory*) &output[0];
    uint64_t* iat = (uint64_t*) &output[import_dir_size];
    uint64_t* ilt = (uint64_t*) &output[import_dir_size + iat_size];
    size_t strtbl_pos = import_dir_size + iat_size*2;

    // We put both the IAT and ILT into the rdata, the PE loader doesn't care but it
    // means the user can't edit these... at least not easily
    TB_LinkerSectionPiece* import_piece;
    uint32_t import_piece_offset;
    {
        TB_LinkerSegment* rdata = tb_linker_find_segment(l, ".rdata");
        TB_ASSERT(rdata != NULL);

        TB_LinkerSection* rdata_sec = rdata->sections[dyn_array_length(rdata->sections) - 1];
        import_piece = tb_linker_append_piece(rdata_sec, PIECE_BUFFER, total_size, 0);
        import_piece->flags |= TB_LINKER_PIECE_LIVE;
        import_piece->buffer = output;
        import_piece->buffer_size = total_size;
        import_piece->offset = rdata_sec->size;

        dyn_array_put(rdata_sec->pieces, import_piece);
        rdata_sec->size += total_size;
        rdata->size += total_size;

        import_piece_offset = rdata_sec->offset + import_piece->offset;
    }

    *imp_dir = (PE_ImageDataDirectory){ import_piece_offset, import_dir_size };
    *iat_dir = (PE_ImageDataDirectory){ import_piece_offset + import_dir_size, iat_size };

    size_t p = 0, q = 0;
    dyn_array_for(i, sorted_imports) {
        ImportTable* imp = sorted_imports[i];

        COFF_ImportDirectory* header = &import_dirs[q++];
        *header = (COFF_ImportDirectory){
            .import_lookup_table  = import_piece_offset + import_dir_size + iat_size + p*sizeof(uint64_t),
            .import_address_table = import_piece_offset + import_dir_size + p*sizeof(uint64_t),
            .name                 = import_piece_offset + strtbl_pos,
        };

        // after we resolve RVAs we need to backpatch stuff
        imp->header = header;
        imp->iat = &iat[p], imp->ilt = &ilt[p];

        TB_Slice lib = imp->libpath;
        memcpy(&output[strtbl_pos], lib.data, lib.length), strtbl_pos += lib.length;
        output[strtbl_pos++] = 0;

        dyn_array_for(j, imp->thunks) {
            TB_LinkerSymbol* t = imp->thunks[j];

            const uint8_t* name = t->name.data + IMP_PREFIX_LEN;
            size_t len = t->name.length - IMP_PREFIX_LEN;

            // import-by-name
            uint64_t value = import_piece_offset + strtbl_pos;
            memcpy(&output[strtbl_pos], &t->import.ordinal, sizeof(uint16_t)), strtbl_pos += 2;
            memcpy(&output[strtbl_pos], name, len), strtbl_pos += len;
            output[strtbl_pos++] = 0;

            // both the ILT and IAT are practically identical at this point
            iat[p] = ilt[p] = value, p++;
        }

        // NULL terminated
        iat[p] = ilt[p] = 0, p++;
    }
    TB_ASSERT(p == import_entry_count);

    // add NULL import directory at the end
    import_dirs[import_dir_count] = (COFF_ImportDirectory){ 0 };

    // trampolines require .text section
    if (l->trampolines.count) {
        TB_LinkerSegment* text = tb_linker_find_segment(l, ".text");
        TB_ASSERT(text != NULL);

        TB_LinkerSection* text_sec = text->sections[dyn_array_length(text->sections) - 1];
        TB_LinkerSectionPiece* piece = tb_linker_append_piece(text_sec, PIECE_BUFFER, l->trampolines.count, 0);
        piece->flags |= TB_LINKER_PIECE_LIVE;
        piece->buffer = l->trampolines.data;
        piece->buffer_size = l->trampolines.count;
        piece->offset = text_sec->size;

        dyn_array_put(text_sec->pieces, piece);
        text_sec->size += l->trampolines.count;
        text->size += l->trampolines.count;

        l->trampoline_pos = text_sec->offset + piece->offset;
    }

    return import_dirs;
}

static void pe_init(TB_Linker* l) {
    l->entrypoint = "mainCRTStartup";
    l->subsystem = TB_WIN_SUBSYSTEM_CONSOLE;

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    {
        TB_LinkerSymbol* sym = tb_linker_new_symbol(l, sizeof("__ImageBase") - 1, "__ImageBase");
        sym->tag = TB_LINKER_SYMBOL_IMAGEBASE;
    }

    #define symbol(name_) (tb_linker_new_symbol(l, sizeof(name_) - 1, name_)->tag = TB_LINKER_SYMBOL_ABSOLUTE)
    // This is practically just ripped from LLD
    //   https://github.com/llvm/llvm-project/blob/3d0a5bf7dea509f130c51868361a38daeee7816a/lld/COFF/Driver.cpp#L2192
    symbol("__AbsoluteZero");
    symbol("__volatile_metadata");
    symbol("__guard_memcpy_fptr");
    symbol("__guard_fids_count");
    symbol("__guard_fids_table");
    symbol("__guard_flags");
    symbol("__guard_iat_count");
    symbol("__guard_iat_table");
    symbol("__guard_longjmp_count");
    symbol("__guard_longjmp_table");
    // Needed for MSVC 2017 15.5 CRT.
    symbol("__enclave_config");
    // Needed for MSVC 2019 16.8 CRT.
    symbol("__guard_eh_cont_count");
    symbol("__guard_eh_cont_table");
    #undef symbol

    // mark entrypoint as something that's accessed, so the lazy symbols get resolved.
    tb_linker_import_symbol(l, (TB_Slice){ (const uint8_t*) l->entrypoint, strlen(l->entrypoint) });
    tb_linker_import_symbol(l, (TB_Slice){ (const uint8_t*) "_load_config_used", sizeof("_load_config_used")-1 });
    tb_linker_import_symbol(l, (TB_Slice){ (const uint8_t*) "_tls_used", sizeof("_tls_used")-1 });
}

static DynArray(PE_BaseReloc) find_base_relocs(TB_Linker* l) {
    DynArray(PE_BaseReloc) base_relocs = NULL;

    // compile all absolute relocations
    size_t reloc_section_size = 0;
    dyn_array_for(i, l->sections_arr) {
        TB_LinkerSection* section = l->sections_arr[i];

        uint32_t last_page = UINT32_MAX;
        dyn_array_for(j, section->pieces) {
            TB_LinkerSectionPiece* p = section->pieces[j];
            TB_ASSERT(p->flags & TB_LINKER_PIECE_LIVE);

            #ifdef CONFIG_HAS_TB
            RelocParser parse_reloc = p->obj && p->obj->module ? tb__linker_module_parse_reloc : pe_parse_reloc;
            #else
            RelocParser parse_reloc = pe_parse_reloc;
            #endif

            FOR_N(k, 0, p->reloc_count) {
                TB_LinkerReloc rel;
                parse_reloc(l, p, k, &rel);
                if (rel.type == TB_OBJECT_RELOC_ADDR64) {
                    PE_BaseReloc br = { section->offset + p->offset + rel.src_offset, section->segment };
                    dyn_array_put(base_relocs, br);
                }
            }
        }
    }

    uint32_t last_page = UINT32_MAX;
    TB_LinkerSegment* last_segment = NULL;
    int count_offset = 0;
    dyn_array_for(i, base_relocs) {
        size_t actual_page = base_relocs[i].offset & ~4095;
        if (last_page != actual_page || last_segment != base_relocs[i].segment) {
            if (reloc_section_size & 3) {
                // pad to 4 bytes
                reloc_section_size += 2;
            }

            last_segment = base_relocs[i].segment;
            last_page = actual_page;

            reloc_section_size += 8;
        }

        reloc_section_size += 2;
    }

    if (reloc_section_size > 0) {
        TB_LinkerSegment* rdata = tb_linker_find_segment(l, ".rdata");
        TB_ASSERT(rdata != NULL);

        TB_LinkerSection* rdata_sec = rdata->sections[dyn_array_length(rdata->sections) - 1];
        l->main_reloc = tb_linker_append_piece(rdata_sec, PIECE_BUFFER, reloc_section_size, 0);
        l->main_reloc->flags |= TB_LINKER_PIECE_LIVE;
        l->main_reloc->offset = rdata_sec->size;

        dyn_array_put(rdata_sec->pieces, l->main_reloc);
        rdata_sec->size += reloc_section_size;
        rdata->size += reloc_section_size;
    }
    return base_relocs;
}

#define WRITE8(data)  (output[write_pos++] = (data))
#define WRITE16(data) (memcpy(&output[write_pos], &(uint16_t){ data }, 2), write_pos += (2))
#define WRITE32(data) (memcpy(&output[write_pos], &(uint32_t){ data }, 4), write_pos += (4))
#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
static bool pe_export(TB_Linker* l, const char* file_name) {
    cuikperf_region_start("linker", NULL);
    tb_linker_complete_appends(l);

    /* for (TB_LinkerThreadInfo* restrict info = l->first_thread_info; info; info = info->next) {
        dyn_array_for(i, info->merges) {
            NL_Slice to_name = { info->merges[i].to.length, info->merges[i].to.data };
            ptrdiff_t to = nl_map_get(l->sections, to_name);
            if (to < 0) continue;

            NL_Slice from_name = { info->merges[i].from.length, info->merges[i].from.data };
            ptrdiff_t from = nl_map_get(l->sections, from_name);
            if (from < 0) continue;

            tb__merge_sections(l, l->sections[from].v, l->sections[to].v);
        }
    }*/

    TB_LinkerSection* rdata = tb_linker_find_section(l, ".rdata");
    CUIK_TIMED_BLOCK("resize barrier") {
        namehs_resize_barrier(&l->symbols);
        namehs_resize_barrier(&l->sections);
        namehs_resize_barrier(&l->imports);
        namehs_resize_barrier(&l->objects);
    }

    // this will resolve the sections, GC any pieces which aren't used and
    // resolve symbols.
    CUIK_TIMED_BLOCK("Resolve & GC") {
        tb_linker_push_named(l, "_load_config_used");
        tb_linker_push_named(l, "_tls_used");
        tb_linker_mark_live(l);
    }

    /* CUIK_TIMED_BLOCK("Merge ops") {
        tb_linker_merge_sections(l, tb_linker_find_section(l, ".00cfg"), rdata);
        tb_linker_merge_sections(l, tb_linker_find_section(l, ".idata"), rdata);
        tb_linker_merge_sections(l, tb_linker_find_section(l, ".xdata"), rdata);
        tb_linker_merge_sections(l, tb_linker_find_section(l, ".CRT"), rdata);
    } */

    if (!tb_linker_layout(l)) {
        cuikperf_region_end();
        return false;
    }

    PE_ImageDataDirectory imp_dir, iat_dir;
    COFF_ImportDirectory* import_dirs;
    CUIK_TIMED_BLOCK("generate imports") {
        import_dirs = gen_imports(l, &imp_dir, &iat_dir);
    }

    DynArray(PE_BaseReloc) base_relocs;
    CUIK_TIMED_BLOCK("base relocs") {
        base_relocs = find_base_relocs(l);
    }

    size_t final_section_count = dyn_array_length(l->segments);
    size_t size_of_headers = sizeof(dos_stub)
        + sizeof(uint32_t) // PE magic number
        + sizeof(COFF_FileHeader)
        + sizeof(PE_OptionalHeader64)
        + (final_section_count * sizeof(PE_SectionHeader));

    size_of_headers = align_up(size_of_headers, 512);

    size_t pe_code_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_CODE
    size_t pe_init_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_INITIALIZED_DATA
    size_t pe_uninit_size = 0; // bytes in total marked as IMAGE_SCN_CNT_UNINITIALIZED_DATA

    size_t section_content_size = 0;
    uint64_t virt_addr = align_up(size_of_headers, 4096); // this area is reserved for the PE header stuff
    dyn_array_for(i, l->segments) {
        TB_LinkerSegment* s = l->segments[i];

        if (s->flags & IMAGE_SCN_CNT_CODE) pe_code_size += s->size;
        if (s->flags & IMAGE_SCN_CNT_INITIALIZED_DATA) pe_init_size += s->size;
        if (s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) pe_uninit_size += s->size;

        s->offset = size_of_headers + section_content_size;
        if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
            section_content_size += align_up(s->size, 512);
        }

        s->address = virt_addr;
        virt_addr += align_up(s->size, 4096);

        // printf("Segment %.*s: %#zx - %#zx\n", (int) s->name.length, s->name.data, s->offset, s->offset + s->size - 1);
    }

    if (l->main_reloc != NULL) {
        CUIK_TIMED_BLOCK(".reloc") {
            TB_Emitter relocs = { .capacity = l->main_reloc->size, .data = cuik_malloc(l->main_reloc->size) };

            // generates .reloc for everyone
            uint32_t last_page = UINT32_MAX;
            TB_LinkerSegment* last_segment = NULL;
            int count_offset = 0;
            dyn_array_for(i, base_relocs) {
                size_t actual_pos  = base_relocs[i].segment->address + base_relocs[i].offset;
                size_t actual_page = actual_pos & ~4095;

                if (last_page != actual_page) {
                    if ((relocs.count & 3) != 0) {
                        // pad to 4 bytes
                        tb_out2b(&relocs, 0x0);
                        *((uint32_t*) &relocs.data[count_offset]) += 2;
                    }

                    last_page    = actual_page;
                    count_offset = relocs.count + 4;

                    tb_out4b(&relocs, actual_page);
                    tb_out4b(&relocs, 8); // block size field (includes RVA and itself)
                }

                tb_out2b(&relocs, 0xA000 | (actual_pos & 4095));
                *((uint32_t*) &relocs.data[count_offset]) += 2;
            }

            TB_ASSERT(l->main_reloc->size == relocs.count);
            l->main_reloc->buffer = relocs.data;
            l->main_reloc->buffer_size = relocs.count;
        }
    }

    if (0) {
        tb_linker_print_map(l);
    }

    TB_LinkerSegment* text = tb_linker_find_segment(l, ".text");
    CUIK_TIMED_BLOCK("sort .pdata") {
        TB_LinkerSection* pdata = tb_linker_find_section(l, ".pdata");
        uint8_t* pdata_buffer   = cuik_malloc(pdata->size);

        uint32_t pdata_rva = pdata->segment->address + pdata->offset;
        dyn_array_for(i, pdata->pieces) {
            TB_LinkerSectionPiece* p = pdata->pieces[i];
            assert(p->kind == PIECE_BUFFER);

            size_t rem = 0;
            uint8_t* out = &pdata_buffer[p->offset];
            if (p->buffer) {
                memcpy(out, p->buffer, p->buffer_size);
                rem = p->buffer_size;
            }

            // zero the remaining space (or CC if it's code)
            if (rem < p->size) {
                int b = (p->flags & TB_LINKER_PIECE_CODE) ? 0xCC : 0;
                memset(&out[rem], b, p->size - rem);
            }

            tb_linker_apply_reloc(l, p, out, pdata_rva, text->address + l->trampoline_pos, 0, 0, p->size);
        }
        qsort(pdata_buffer, pdata->size / sizeof(uint32_t[3]), sizeof(uint32_t[3]), compare_rva);

        // this doesn't change the section contents size, just resolves it into one big piece
        TB_LinkerSectionPiece* piece = cuik_malloc(sizeof(TB_LinkerSectionPiece));
        *piece = (TB_LinkerSectionPiece){
            .kind        = PIECE_BUFFER,
            .flags       = TB_LINKER_PIECE_LIVE,
            .parent      = pdata,
            .size        = pdata->size,
            .buffer      = pdata_buffer,
            .buffer_size = pdata->size,
        };

        dyn_array_clear(pdata->pieces);
        dyn_array_put(pdata->pieces, piece);
    }

    if (import_dirs != NULL) {
        uint32_t rdata_rva = rdata->segment->address;

        iat_dir.virtual_address += rdata_rva;
        imp_dir.virtual_address += rdata_rva;
        l->iat_pos = iat_dir.virtual_address;

        CUIK_TIMED_BLOCK("relocate imports and trampolines") {
            dyn_array_for(i, l->sorted_imports) {
                ImportTable* imp = l->sorted_imports[i];

                COFF_ImportDirectory* header = imp->header;
                header->import_lookup_table  += rdata_rva;
                header->import_address_table += rdata_rva;
                header->name += rdata_rva;

                uint64_t *ilt = imp->ilt, *iat = imp->iat;
                uint64_t iat_rva = header->import_address_table;
                uint64_t trampoline_rva = text->address + l->trampoline_pos;

                dyn_array_for(j, imp->thunks) {
                    if (iat[j] != 0) {
                        iat[j] += rdata_rva;
                        ilt[j] += rdata_rva;
                    }

                    // Relocate trampoline entries to point into the IAT, the PE loader will
                    // fill these slots in with absolute addresses of the symbols.
                    int32_t* trampoline_dst = (int32_t*) &l->trampolines.data[imp->thunks[j]->import.ds_address + 2];
                    TB_ASSERT(*trampoline_dst == 0 && "We set this earlier... why isn't it here?");

                    uint32_t target_rva = iat_rva + j*8;
                    assert(target_rva == l->iat_pos + imp->thunks[j]->import.thunk_id*8);

                    uint32_t source_pos = trampoline_rva + imp->thunks[j]->import.ds_address + 6;
                    (*trampoline_dst) += target_rva - source_pos;
                }
            }
        }
    }

    size_t output_size = size_of_headers + section_content_size;
    COFF_FileHeader header = {
        .machine = 0x8664,
        .section_count = final_section_count,
        .timestamp = 1056582000u, // my birthday since that's consistent :P
        .symbol_table = 0,
        .symbol_count = 0,
        .optional_header_size = sizeof(PE_OptionalHeader64),
        .flags = 0x2 | 0x0020 /* executable, >2GB */
    };

    if (l->subsystem == TB_WIN_SUBSYSTEM_EFI_APP) {
        header.flags |= 0x2000; // DLL
    }

    static const uint32_t subsys[] = {
        [TB_WIN_SUBSYSTEM_WINDOWS] = IMAGE_SUBSYSTEM_WINDOWS_GUI,
        [TB_WIN_SUBSYSTEM_CONSOLE] = IMAGE_SUBSYSTEM_WINDOWS_CUI,
        [TB_WIN_SUBSYSTEM_EFI_APP] = IMAGE_SUBSYSTEM_EFI_APPLICATION,
    };

    PE_OptionalHeader64 opt_header = {
        .magic = 0x20b,
        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x140000000,

        .size_of_code = pe_code_size,
        .size_of_initialized_data = pe_init_size,
        .size_of_uninitialized_data = pe_uninit_size,

        .size_of_image = virt_addr,
        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = subsys[l->subsystem],
        .dll_characteristics = 0x40 | 0x20 | 0x0100 | 0x8000, /* dynamic base, high entropy, nx compat, terminal server aware */

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .data_directories = {
            [IMAGE_DIRECTORY_ENTRY_IMPORT] = imp_dir,
            [IMAGE_DIRECTORY_ENTRY_IAT] = iat_dir,
        }
    };

    if (l->subsystem != TB_WIN_SUBSYSTEM_EFI_APP) {
        opt_header.major_os_ver = 6;
        opt_header.minor_os_ver = 0;
        opt_header.major_subsystem_ver = 6;
        opt_header.minor_subsystem_ver = 0;
    }

    TB_LinkerSymbol* tls_used_sym = tb_linker_symbol_find(tb_linker_find_symbol2(l, "_tls_used"));
    if (tls_used_sym && (tls_used_sym->flags & TB_LINKER_SYMBOL_USED)) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_TLS] = (PE_ImageDataDirectory){ tb__get_symbol_rva(tls_used_sym), sizeof(PE_TLSDirectory) };
    }

    TB_LinkerSymbol* load_config_used_sym = tb_linker_symbol_find(tb_linker_find_symbol2(l, "_load_config_used"));
    if (load_config_used_sym && (load_config_used_sym->flags & TB_LINKER_SYMBOL_USED)) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG] = (PE_ImageDataDirectory){ tb__get_symbol_rva(load_config_used_sym), 0x140 };
    }

    TB_LinkerSection* pdata = tb_linker_find_section(l, ".pdata");
    if (pdata) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_EXCEPTION] = (PE_ImageDataDirectory){ pdata->segment->address + pdata->offset, pdata->size };
    }

    TB_LinkerSectionPiece* reloc = l->main_reloc;
    if (reloc) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_BASERELOC] = (PE_ImageDataDirectory){ reloc->parent->segment->address + reloc->parent->offset + reloc->offset, reloc->size };
    }

    // text section crap
    if (text) {
        opt_header.base_of_code = text->address;
        opt_header.size_of_code = align_up(text->size, 4096);

        TB_LinkerSymbol* sym = tb_linker_symbol_find(tb_linker_find_symbol2(l, l->entrypoint));
        if (sym) {
            opt_header.entrypoint = tb__get_symbol_rva(sym);
        } else {
            printf("tblink: could not find entrypoint! %s\n", l->entrypoint);
        }
    }

    // do the final exporting, we can thread this
    bool result = true;
    CUIK_TIMED_BLOCK("output") {
        FileMap fm = open_file_map_write(file_name, output_size);
        if (fm.data == NULL) {
            printf("tblink: could not open file! %s", file_name);
            return false;
        }

        size_t write_pos = 0;
        uint8_t* output = fm.data;

        uint32_t pe_magic = 0x00004550;
        WRITE(dos_stub,    sizeof(dos_stub));
        WRITE(&pe_magic,   sizeof(pe_magic));
        WRITE(&header,     sizeof(header));
        WRITE(&opt_header, sizeof(opt_header));
        dyn_array_for(i, l->segments) {
            TB_LinkerSegment* s = l->segments[i];
            PE_SectionHeader sec_header = {
                .virtual_size    = s->size,
                .virtual_address = s->address,
                .characteristics = s->flags & ~IMAGE_SCN_LNK_COMDAT,
            };

            if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                sec_header.pointer_to_raw_data = s->offset;
                sec_header.size_of_raw_data = align_up(s->size, 512);
            }

            size_t len = s->name.length;
            memcpy(sec_header.name, s->name.data, len > 8 ? 8 : len);
            WRITE(&sec_header, sizeof(sec_header));
        }
        write_pos = tb__pad_file(output, write_pos, 0x00, 0x200);

        l->output_cap = output_size;
        l->output = output;

        tb_linker_export_pieces(l);
        close_file_map(&fm);
    }

    CUIK_TIMED_BLOCK("pdb output") {
        // Collect all modules
        DynArray(TB_LinkerObject*) sorted_objs = dyn_array_create(TB_LinkerObject*, 8);
        nbhs_for(e, &l->objects) {
            TB_LinkerObject* obj = *e;
            if (obj->live && obj->name.data[obj->name.length - 1] != '/') {
                if (obj->name.length < 4 || memcmp(&obj->name.data[obj->name.length - 4], ".dll", 4) != 0) {
                    dyn_array_put(sorted_objs, obj);
                }
            }
        }
        qsort(sorted_objs, dyn_array_length(sorted_objs), sizeof(TB_LinkerObject*), compare_name);
        // dyn_array_set_length(sorted_objs, 100);

        DynArray(DebugStream*) streams = dyn_array_create(DebugStream*, 8);
        FOR_N(i, 0, 6) {
            DebugStream* s = tb_arena_alloc(&linker_perm_arena, sizeof(DebugStream));
            s->size = 0;
            if (i == 1) {
                s->size = 71; // Info stream
            } else if (i == 3) {
                s->size = sizeof(PDB_DBIHeader);
            }
            dyn_array_put(streams, s);
        }

        size_t mod_info_size = 0;
        dyn_array_for(i, sorted_objs) {
            TB_LinkerObject* obj = sorted_objs[i];

            DebugStream* s = tb_arena_alloc(&linker_perm_arena, sizeof(DebugStream));
            s->size = 0;
            dyn_array_put(streams, s);

            mod_info_size += sizeof(PDB_ModuleInfo);
            mod_info_size += obj->name.length + 1;
            if (obj->parent) {
                mod_info_size += obj->parent->name.length + 1;
            } else {
                mod_info_size += obj->name.length + 1;
            }
            mod_info_size = (mod_info_size + 3) & ~3;
        }

        // DBI
        streams[3]->size += mod_info_size;
        streams[3]->size += sizeof(uint16_t[2]) + dyn_array_length(sorted_objs)*sizeof(uint16_t[2]);

        size_t blocks_needed = 5;
        dyn_array_for(i, streams) {
            streams[i]->first_block = blocks_needed;
            blocks_needed += (streams[i]->size + PDB_BLOCK_SIZE - 1) / PDB_BLOCK_SIZE;
        }

        // 0          1     2     3    4       5      6       7     8
        // Superblock Free0 Free1 Dirs Headers Block Block Block Block
        size_t pdb_size = blocks_needed*PDB_BLOCK_SIZE;
        FileMap fm = open_file_map_write("a.pdb", pdb_size);
        if (fm.data == NULL) {
            printf("tblink: could not open file! %s", "a.pdb");
            return false;
        }

        size_t write_pos = 0;
        uint8_t* output = fm.data;

        PDB_SuperBlock super = {
            .file_magic = "Microsoft C/C++ MSF 7.00\r\n\x1A\x44\x53\0\0",
            .block_size = PDB_BLOCK_SIZE,
            .free_block_map_block = 1,
            .num_blocks = blocks_needed,
            .num_directory_bytes = 4 + dyn_array_length(streams)*8,
        };

        // SuperBlock + directory page blocks
        WRITE(&super, sizeof(super));
        WRITE32(3);

        // Block3: Directory
        write_pos = 3*PDB_BLOCK_SIZE;
        WRITE32(4);

        // Block4: Stream Headers
        write_pos = 4*PDB_BLOCK_SIZE;
        WRITE32(dyn_array_length(streams));
        dyn_array_for(i, streams) { WRITE32(streams[i]->size); }
        dyn_array_for(i, streams) {
            size_t block_count = (streams[i]->size + PDB_BLOCK_SIZE - 1) / PDB_BLOCK_SIZE;
            FOR_N(j, 0, block_count) {
                WRITE32(streams[i]->first_block + j);
            }
        }

        dyn_array_for(i, streams) {
            size_t start = write_pos = streams[i]->first_block*PDB_BLOCK_SIZE;
            if (i == 1) {
                PDB_InfoHeader stream = {
                    .version = 20000404, // VC70
                };
                WRITE(&stream, sizeof(stream));

                // PDB Names streams
                WRITE32(sizeof("/names"));
                WRITE("/names", sizeof("/names"));

                // https://llvm.org/docs/PDB/HashTable.html
                //   There's a really weird hash map stuffed here rather than
                //   some direct stream indices but whatever, my job isn't to
                //   question microsoft, that's a hobby, my job is to put up
                //   with microsoft.
                //
                // Size & Capacity
                WRITE32(1);
                WRITE32(1);
                // Present Bit Vector
                WRITE32(1);
                WRITE32(1);
                // Deleted Bit Vector
                WRITE32(1);
                WRITE32(0);
                // Entries
                WRITE32(0);
                WRITE32(5);
            } else if (i == 3) {
                PDB_DBIHeader stream = {
                    .signature = -1,
                    .version = 19990903, // VC70
                    .machine = 0xD0,
                    .mod_info_size = mod_info_size,
                    .source_info_size = sizeof(uint16_t[2]) + dyn_array_length(sorted_objs)*sizeof(uint16_t[2]),
                };
                WRITE(&stream, sizeof(stream));

                dyn_array_for(i, sorted_objs) {
                    TB_LinkerObject* obj = sorted_objs[i];
                    PDB_ModuleInfo mod = {
                        .module_sym_stream = 6 + i,
                    };
                    WRITE(&mod, sizeof(mod));

                    WRITE(obj->name.data, obj->name.length);
                    WRITE8(0);
                    if (obj->parent) {
                        WRITE(obj->parent->name.data, obj->parent->name.length);
                        WRITE8(0);
                    } else {
                        WRITE(obj->name.data, obj->name.length);
                        WRITE8(0);
                    }

                    int next = (write_pos + 3) & ~3;
                    while (write_pos < next) { WRITE8(0); }
                }

                // Source File info
                WRITE16(dyn_array_length(sorted_objs));
                WRITE16(0);

                dyn_array_for(i, sorted_objs) {
                    WRITE16(i);
                }

                dyn_array_for(i, sorted_objs) {
                    WRITE16(0);
                }
            }
            assert(write_pos - start == streams[i]->size);
        }

        close_file_map(&fm);
    }

    cuikperf_region_end();
    return true;
}
#undef WRITE32
#undef WRITE16
#undef WRITE8
#undef WRITE

TB_LinkerVtbl tb__linker_pe = {
    .init           = pe_init,
    .find_lib       = pe_find_lib,
    .append_module  = pe_append_module,
    .append_object  = pe_append_object,
    .append_library = pe_append_library,
    .parse_reloc    = pe_parse_reloc,
    .export         = pe_export
};
