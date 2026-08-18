
enum {
    LAZY_IMPORT_BATCH_SIZE = 1024,
    LAZY_IMPORT_STRTAB_MUNCH = 128*1024,
};

static size_t ideally_fast_skip16(const char* strtab, int limit, size_t str_head) {
    #if USE_INTRIN && CUIK__IS_X64
    size_t j = 0;
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
        const char* name = &strtab[str_head];
        j += 1, str_head += ideally_fast_strlen(name) + 1;
    }

    assert(j == limit);
    return str_head;
}

static void lazy_import_task(TPool* pool, void** args) {
    TB_LinkerArchive* lib = args[0];
    BCache_File* file = lib->header.file;

    TB_Slice name = tb_linker_get_base_name(lib->header.name);
    cuikperf_region_start2("lazy parse", name.length, (const char*) name.data);

    TB_Linker* l = lib->header.linker;
    tb_linker_worker_init(l);

    size_t i = (size_t) args[1], limit = i + LAZY_IMPORT_BATCH_SIZE;
    if (limit > lib->symbol_count) {
        limit = lib->symbol_count;
    }

    size_t j = (size_t) args[2];
    char* strtab = (char*) &lib->header.file->raw_map[lib->symbol_strtab];
    while (i < limit) {
        uint16_t offset_index = lib->symbols[i] - 1;
        const char* name = &strtab[j];
        size_t next = ideally_fast_skip16(strtab, 1, j);
        // size_t len = ideally_fast_strlen(name);

        #if 0
        printf("SYMBOL %zu | %d | %d | %s\n", i, offset_index, lib->members[offset_index], name);
        #endif

        assert(offset_index < lib->member_count);
        assert(lib->members[offset_index] < lib->header.file->size);
        TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
        *s = (TB_LinkerSymbol){
            .name   = { (const uint8_t*) name, (next - j) - 1 },
            .tag    = TB_LINKER_SYMBOL_LAZY,
            .lazy   = { NULL, lib, lib->members[offset_index] },
        };
        s = tb_linker_symbol_insert(l, s, true);
        i += 1, j = next;
    }

    cuikperf_region_end();
    tb_linker_job_done(l);
}

static bool fetch_lazy(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    TB_LinkerArchive* lib = (TB_LinkerArchive*) obj;
    BCache_File* file = lib->header.file;

    size_t symbol_i = lib->symbol_i;
    size_t string_head = lib->string_head;
    size_t string_tail = lib->string_tail;
    size_t second_size = lib->second_size;

    size_t strtab_size = (lib->second_base + second_size) - lib->symbol_strtab;
    char* strtab = (char*) &lib->header.file->raw_map[lib->symbol_strtab];

    // printf("LAZY %zu blocks\n", (strtab_size + 4095) / 4096);
    cuikperf_region_start("chunk", NULL);

    bool distribute = l->jobs.pool != NULL && tpool_num_threads(l->jobs.pool) > 1;
    while (symbol_i < lib->symbol_count) {
        // Fetch ahead on the string table, the only reason we're even breaking it
        // up into pieces is to allow avoid stalling all other workers and read requests
        // while we wait for ours.
        if (string_tail != strtab_size && string_head + 4096 >= string_tail) {
            size_t readahead = string_tail + LAZY_IMPORT_STRTAB_MUNCH;
            if (readahead > strtab_size) {
                readahead = strtab_size;
            }
            assert(readahead == strtab_size || string_head + 4096 < readahead);
            // printf("READAHEAD %p %zu %zu\n", lib, symbol_i, readahead - string_head);

            // writeback
            lib->symbol_i    = symbol_i;
            lib->string_head = string_head;
            lib->string_tail = string_tail = readahead;
            lib->header.stage = 1;

            cuikperf_region_end();
            if (!tb_linker_read_req_FAST(l, file, lib->symbol_strtab + string_head, readahead - string_head, NULL, &lib->header, NULL)) {
                return false;
            }
            cuikperf_region_start("chunk", NULL);
        }

        assert(string_head < second_size);
        uint16_t offset_index = lib->symbols[symbol_i] - 1;
        const char* name = &strtab[string_head];
        size_t next = ideally_fast_skip16(strtab, 1, string_head);

        #if 0
        printf("SYMBOL %zu | %d | %d | %s\n", symbol_i, offset_index, lib->members[offset_index], name);
        #endif

        if (!distribute) {
            assert(offset_index < lib->member_count);
            assert(lib->members[offset_index] < lib->header.file->size);
            TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
            *s = (TB_LinkerSymbol){
                .name   = { (const uint8_t*) name, (next - string_head) - 1 },
                .tag    = TB_LINKER_SYMBOL_LAZY,
                .lazy   = { NULL, lib, lib->members[offset_index] },
            };
            s = tb_linker_symbol_insert(l, s, true);
        }

        assert(next - string_head < LAZY_IMPORT_STRTAB_MUNCH);
        symbol_i += 1, string_head = next;
        if (distribute && symbol_i - lib->munch_start == LAZY_IMPORT_BATCH_SIZE) {
            cuikperf_region_end();

            void* args[3] = { lib, (void*) lib->munch_start, (void*) lib->munch_start_sym };
            tb_linker_job_submit_N(l, lazy_import_task, 3, args);

            lib->munch_start = symbol_i;
            lib->munch_start_sym = string_head;

            cuikperf_region_start("chunk", NULL);
        }
    }

    if (distribute && symbol_i != lib->munch_start) {
        void* args[3] = { lib, (void*) lib->munch_start, (void*) lib->munch_start_sym };
        tb_linker_job_submit_N(l, lazy_import_task, 3, args);
    }
    cuikperf_region_end();

    lib->header.stage = 2;
    return true;
}

// Called 3 times
// (1) Process first
// (2) Process second
// (3) Process longnames
static bool fetch_lib_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    assert(file_header_offset == 0 && "No nested libs... yet?");

    TB_LinkerArchive* lib = (TB_LinkerArchive*) obj;
    BCache_File* file = lib->header.file;
    uint8_t* raw_map = file->raw_map;

    lib->header.stage = 1;

    if (lib->second_base == 0) {
        COFF_ArchiveMemberHeader first;
        assert(prefetch.length >= 8 + sizeof(COFF_ArchiveMemberHeader));
        memcpy(&first, &prefetch.data[8], sizeof(COFF_ArchiveMemberHeader));
        if (memcmp(first.name, (char[16]) { "/               " }, 16) != 0) {
            fprintf(stderr, "TB archive parser: first archive member name is invalid\n");
            return false;
        }
        size_t first_content_length = tb__parse_decimal_int(sizeof(first.size), first.size);

        // Advance
        size_t file_offset = 8 + sizeof(COFF_ArchiveMemberHeader) + first_content_length;
        file_offset = (file_offset + 1u) & ~1u;

        // Fetch second headeer and member list
        lib->second_base = file_offset + sizeof(COFF_ArchiveMemberHeader);
        if (!tb_linker_read_req_FAST(l, file, file_offset, sizeof(COFF_ArchiveMemberHeader) + sizeof(uint32_t), NULL, &lib->header, NULL)) {
            return false;
        }
    }

    // Find placement for longnames and load member list
    if (lib->longnames_base == 0) {
        size_t file_offset = lib->second_base - sizeof(COFF_ArchiveMemberHeader);
        uint8_t* second_data = &raw_map[lib->second_base];

        COFF_ArchiveMemberHeader second;
        memcpy(&second, &raw_map[file_offset], sizeof(COFF_ArchiveMemberHeader));
        if (memcmp(second.name, (char[16]) { "/               " }, 16) != 0) {
            fprintf(stderr, "TB archive parser: second archive member name is invalid\n");
            return false;
        }
        lib->second_size = tb__parse_decimal_int(sizeof(second.size), second.size);

        // Advance
        file_offset += sizeof(COFF_ArchiveMemberHeader) + lib->second_size;
        file_offset = (file_offset + 1u) & ~1u;
        lib->longnames_base = file_offset + sizeof(COFF_ArchiveMemberHeader);

        // TODO(NeGate): better error checking for bad files
        memcpy(&lib->member_count, second_data, sizeof(uint32_t));
        lib->members = (uint32_t*) &second_data[4];

        // Fetch symbols
        lib->symbol_base = lib->second_base + 4 + lib->member_count*sizeof(uint32_t);

        size_t readahead = lib->symbol_base + sizeof(uint32_t);
        if (readahead < LAZY_IMPORT_STRTAB_MUNCH) {
            readahead = LAZY_IMPORT_STRTAB_MUNCH;
        }

        if (readahead > lib->header.file->size) {
            readahead = lib->header.file->size;
        }
        lib->prefetch_pos = readahead;

        if (!tb_linker_read_req_FAST(l, file, lib->second_base, readahead - lib->second_base, NULL, &lib->header, NULL)) {
            return false;
        }
    }

    // Load symbols
    if (lib->symbols == NULL) {
        uint8_t* second = &raw_map[lib->second_base];

        // TODO(NeGate): better error checking for bad files
        memcpy(&lib->symbol_count, &second[4 + lib->member_count*sizeof(uint32_t)], sizeof(uint32_t));
        lib->symbols = (uint16_t*) &second[8 + lib->member_count*sizeof(uint32_t)];

        size_t file_offset = lib->second_base;
        file_offset += 4 + lib->member_count*sizeof(uint32_t);
        file_offset += 4 + lib->symbol_count*sizeof(uint16_t);
        lib->symbol_strtab = file_offset;

        // Fetch members
        if (!tb_linker_read_req_FAST(l, file, lib->symbol_base, lib->symbol_strtab - lib->symbol_base, NULL, &lib->header, NULL)) {
            return false;
        }
    }

    if (!lib->loaded_members) {
        lib->loaded_members = true;

        // Fetch longnames
        if (!tb_linker_read_req_FAST(l, file, lib->longnames_base, sizeof(COFF_ArchiveMemberHeader), NULL, &lib->header, NULL)) {
            return false;
        }
    }

    size_t file_offset = lib->longnames_base - sizeof(COFF_ArchiveMemberHeader);

    COFF_ArchiveMemberHeader longnames;
    memcpy(&longnames, &raw_map[file_offset], sizeof(COFF_ArchiveMemberHeader));
    if (memcmp(longnames.name, (char[16]) { "//              " }, 16) == 0) {
        lib->longnames_size = tb__parse_decimal_int(sizeof(longnames.size), longnames.size);

        // Advance
        file_offset += sizeof(COFF_ArchiveMemberHeader) + lib->longnames_size;
        file_offset = (file_offset + 1u) & ~1u;
    }
    // printf("A %zu %zu\n", lib->second_size, lib->longnames_size);

    #if 1
    if (lib->symbol_count == 0 || lib->member_count == 0) {
        return true;
    }

    if (lib->prefetch_pos < lib->symbol_strtab) {
        lib->string_tail = 0;
    } else {
        lib->string_tail = lib->prefetch_pos - lib->symbol_strtab;
    }
    lib->header.fetch = fetch_lazy;

    return fetch_lazy(l, obj, prefetch, file_header_offset);
    #else
    // Read the archive up until the end of the longnames
    lib->header.io_rem = 1;
    lib->header.stage  = 2;
    tb_linker_read_req(l, file, lib->second_base, lib->second_size, NULL, &lib->header, NULL);
    return false;
    #endif
}

static void process_lib_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    #if 0
    TB_LinkerArchive* lib = (TB_LinkerArchive*) obj;
    char* second = (char*) &lib->header.file->raw_map[lib->second_base];

    CUIK_TIMED_BLOCK("lazy") {
        uint64_t t = lib->header.time;
        char* strtab = (char*) &lib->header.file->raw_map[lib->symbol_strtab];

        if (l->jobs.pool != NULL && tpool_num_threads(l->jobs.pool) > 1) {
            #if CUIK_ALLOW_THREADS
            size_t i = 0, str_head = 0;
            for (size_t i = 0; i < lib->symbol_count; i += LAZY_IMPORT_BATCH_SIZE) {
                size_t limit = i + LAZY_IMPORT_BATCH_SIZE;
                if (limit > lib->symbol_count) {
                    limit = lib->symbol_count;
                }

                void* args[3] = { lib, (void*) i, (void*) str_head };
                tb_linker_job_submit_N(l, lazy_import_task, 3, args);

                str_head = ideally_fast_skip16(strtab, limit - i, str_head);
            }
            #else
            abort(); // Unreachable
            #endif
        } else {
            // uint64_t start = __rdtsc();
            size_t last_page = 0;
            size_t cache_lo = SIZE_MAX, cache_hi = 0;

            size_t i = 0, j = 0;
            while (i < lib->symbol_count) {
                uint16_t offset_index = lib->symbols[i] - 1;
                const char* name = &strtab[j];
                size_t next = ideally_fast_skip16(strtab, 1, j);
                // size_t len = ideally_fast_strlen(name);

                size_t obj_base = lib->members[offset_index];
                if ((obj_base / 4096) != last_page) {
                    last_page = (obj_base / 4096);

                    cache_lo = TB_MIN(cache_lo, last_page);
                    cache_hi = TB_MAX(cache_hi, last_page);
                }

                TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name   = { (const uint8_t*) name, (next - j) - 1 },
                    .tag    = TB_LINKER_SYMBOL_LAZY,
                    .lazy   = { NULL, lib, obj_base },
                };
                s = tb_linker_symbol_insert(l, s, true);
                i += 1, j = next;
            }

            #if 0
            double elapsed = cuik_special_time(__rdtsc() - start);
            double iters = lib->symbol_count;
            printf("Stats %.*s | %.3f iter/ns\n", (int) lib->header.name.length, lib->header.name.data, elapsed / iters);
            #endif
        }
    }
    #endif
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
}
#endif

