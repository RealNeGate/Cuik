
enum {
    LAZY_IMPORT_BATCH_SIZE = 1024
};

static void lazy_import_task(TPool* pool, void** args) {
    cuikperf_region_start("lazy parse", NULL);

    TB_LinkerArchive* lib = args[0];
    TB_Linker* l = lib->header.linker;
    tb_linker_worker_init(l);

    size_t i = (size_t) args[1], limit = i + LAZY_IMPORT_BATCH_SIZE;
    if (limit > lib->symbol_count) {
        limit = lib->symbol_count;
    }

    size_t j = (size_t) args[2];
    char* strtab = lib->symbol_strtab;
    while (i < limit) {
        uint16_t offset_index = lib->symbols[i] - 1;
        const char* name = &strtab[j];
        size_t len = ideally_fast_strlen(name);

        TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
        *s = (TB_LinkerSymbol){
            .name   = { (const uint8_t*) name, len },
            .tag    = TB_LINKER_SYMBOL_LAZY,
            .lazy   = { lib, lib->members[offset_index] },
        };
        s = tb_linker_symbol_insert(l, s, true);
        i += 1, j += len + 1;
    }

    cuikperf_region_end();
    tb_linker_job_done(l);
}

static const char* LIB_STAGE_NAMES[] = {
    "fetch_lib", "parse_lib"
};

static bool fetch_lib_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    TB_LinkerArchive* lib = (TB_LinkerArchive*) obj;

    int fd = lib->header.fd;
    size_t file_offset = 8; // magic number was already checked
    COFF_ArchiveMemberHeader first, second, longnames;

    // Process first member
    tb_linker_read_imm(fd, file_offset, sizeof(COFF_ArchiveMemberHeader), &first);
    if (memcmp(first.name, (char[16]) { "/               " }, 16) != 0) {
        fprintf(stderr, "TB archive parser: first archive member name is invalid\n");
        return false;
    }
    size_t first_content_length = tb__parse_decimal_int(sizeof(first.size), first.size);
    file_offset += sizeof(COFF_ArchiveMemberHeader) + first_content_length;
    file_offset = (file_offset + 1u) & ~1u;

    // Process second member
    tb_linker_read_imm(fd, file_offset, sizeof(COFF_ArchiveMemberHeader), &second);
    if (memcmp(second.name, (char[16]) { "/               " }, 16) != 0) {
        fprintf(stderr, "TB archive parser: second archive member name is invalid\n");
        return false;
    }
    lib->second_base = file_offset + sizeof(COFF_ArchiveMemberHeader);
    lib->second_size = tb__parse_decimal_int(sizeof(second.size), second.size);

    // Advance
    file_offset += sizeof(COFF_ArchiveMemberHeader) + lib->second_size;
    file_offset = (file_offset + 1u) & ~1u;

    // Process long name member
    lib->longnames_base = file_offset + sizeof(COFF_ArchiveMemberHeader);
    tb_linker_read_imm(fd, file_offset, sizeof(COFF_ArchiveMemberHeader), &longnames);
    if (memcmp(longnames.name, (char[16]) { "//              " }, 16) == 0) {
        lib->longnames_size = tb__parse_decimal_int(sizeof(longnames.size), longnames.size);

        // Advance
        file_offset += sizeof(COFF_ArchiveMemberHeader) + lib->longnames_size;
        file_offset = (file_offset + 1u) & ~1u;
    }

    // Read the archive up until the end of the longnames
    lib->header.io_rem = 1;
    lib->second_longnames = tb_linker_moar_mem(file_offset - lib->second_base);
    tb_linker_read_req(l, false, lib->second_base, file_offset - lib->second_base, lib->second_longnames, &lib->header);
    return false;
}

static void process_lib_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    TB_LinkerArchive* lib = (TB_LinkerArchive*) obj;
    char* second = lib->second_longnames;
    if (lib->second_size > 0) {
        // TODO(NeGate): better error checking for bad files
        memcpy(&lib->member_count, &second[0], sizeof(uint32_t));
        memcpy(&lib->symbol_count, &second[4 + lib->member_count*sizeof(uint32_t)], sizeof(uint32_t));

        lib->members = (uint32_t*) &second[4];
        lib->symbols = (uint16_t*) &second[8 + lib->member_count*sizeof(uint32_t)];
        lib->symbol_strtab = (char*) &lib->symbols[lib->symbol_count];
    }

    if (lib->longnames_size > 0) {
        lib->longnames = (TB_Slice){
            (const uint8_t*) &lib->second_longnames[lib->longnames_base - lib->second_base],
            lib->longnames_size
        };
    }

    CUIK_TIMED_BLOCK("lazy") {
        uint64_t t = lib->header.time;
        char* strtab = lib->symbol_strtab;

        if (l->jobs.pool != NULL) {
            #if CUIK_ALLOW_THREADS
            size_t i = 0, str_head = 0;
            for (size_t i = 0; i < lib->symbol_count; i += LAZY_IMPORT_BATCH_SIZE) {
                size_t limit = i + LAZY_IMPORT_BATCH_SIZE;
                if (limit > lib->symbol_count) {
                    limit = lib->symbol_count;
                }

                void* args[3] = { lib, (void*) i, (void*) str_head };
                tb_linker_job_submit_N(l, lazy_import_task, 3, args);

                #if USE_INTRIN && CUIK__IS_X64
                size_t j = i;
                while (j < limit) {
                    // Skip 16B each time we don't reach the limit
                    __m128i str128  = _mm_loadu_si128((__m128i*) &strtab[str_head]);
                    __m128i zero128 = _mm_set1_epi8('\0');
                    uint32_t null_mask = _mm_movemask_epi8(_mm_cmpeq_epi8(str128, zero128));
                    int nulls = __builtin_popcount(null_mask);
                    if (j + nulls >= limit) {
                        break;
                    }
                    str_head += 16, j += nulls;
                }
                #else
                size_t j = i;
                #endif

                // Skip strings
                while (j < limit) {
                    uint16_t offset_index = lib->symbols[j] - 1;
                    const char* name = &strtab[str_head];
                    j += 1, str_head += ideally_fast_strlen(name) + 1;
                }
            }
            #else
            abort(); // Unreachable
            #endif
        } else {
            size_t i = 0, j = 0;
            while (i < lib->symbol_count) {
                uint16_t offset_index = lib->symbols[i] - 1;
                const char* name = &strtab[j];
                size_t len = ideally_fast_strlen(name);

                TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name   = { (const uint8_t*) name, len },
                    .tag    = TB_LINKER_SYMBOL_LAZY,
                    .lazy   = { lib, lib->members[offset_index] },
                };
                s = tb_linker_symbol_insert(l, s, true);
                i += 1, j += len + 1;
            }
        }
    }
}

#if 0
void pe_append_library(TPool* pool, void** args) {
    // Lazy library loading requires 3 streams to be read:
    //   symbols[i]     members[symbols[i] - 1]     strtab[j]
    //
    // because of this design, the smallest list is going to be the symbols and we can
    // simply choose to load those all together, then it's a matter of distributing work.
    // there's a serial dependency on the string table but the insertion of symbols can
    // be forked off from a main thread that scans the string.
    char* strtab = lib->symbol_strtab;
    size_t i = 0, j = 0;
    while (i < lib->symbol_count) {
        uint16_t offset_index = lib->symbols[i] - 1;
        const char* name = &strtab[j];
        size_t len = ideally_fast_strlen(name);

        printf("READ %hu %u %s\n", offset_index, lib->members[offset_index], name);
        i += 1, j += len + 1;
    }
    tb_linker_job_done(l);
}
#endif

