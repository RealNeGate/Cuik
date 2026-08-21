
enum {
    LAZY_IMPORT_BATCH_SIZE = 1024,
    LAZY_IMPORT_STRTAB_MUNCH = 8*1024,
};

static size_t ideally_fast_skip16(const char* strtab, int limit, size_t str_head, size_t str_tail) {
    #if USE_INTRIN && CUIK__IS_X64
    size_t j = 0;
    while (j < limit && str_head + 16 <= str_tail) {
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
    while (j < limit && str_head < str_tail) {
        j += strtab[str_head] == 0;
        str_head += 1;
    }

    assert(str_head == str_tail || j == limit);
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

    size_t strtab_size = (lib->second_base + lib->second_size) - lib->symbol_strtab;

    size_t j = (size_t) args[2];
    char* strtab = (char*) &lib->header.file->raw_map[lib->symbol_strtab];
    while (i < limit) {
        uint16_t offset_index = lib->symbols[i] - 1;
        const char* name = &strtab[j];
        size_t next = ideally_fast_skip16(strtab, 1, j, strtab_size - j);
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

// static void process_lib_file(TB_Linker* l, BCache_Job* job, TB_LinkerObject* obj);
static bool step_lib_file(TB_Linker* l, BCache_Job* job, TB_LinkerObject* obj, TB_Slice prefetch) {
    BCache_File* file = obj->file;
    uint8_t* raw_map = file->raw_map;

    assert(obj->offset + obj->skip_header == 0 && "No nested libs... yet?");
    TB_LinkerArchive* lib = (TB_LinkerArchive*) obj;
    switch (job->state) {
        case 0: {
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
            JOB_READ(1, file_offset, sizeof(COFF_ArchiveMemberHeader) + sizeof(uint32_t), NULL);
        }

        // Find placement for longnames and load member list
        case 1: {
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
            JOB_READ(2, lib->second_base, readahead - lib->second_base, NULL);
        }

        // Load symbols
        case 2: {
            uint8_t* second = &raw_map[lib->second_base];

            // TODO(NeGate): better error checking for bad files
            memcpy(&lib->symbol_count, &second[4 + lib->member_count*sizeof(uint32_t)], sizeof(uint32_t));
            lib->symbols = (uint16_t*) &second[8 + lib->member_count*sizeof(uint32_t)];

            size_t file_offset = lib->second_base;
            file_offset += 4 + lib->member_count*sizeof(uint32_t);
            file_offset += 4 + lib->symbol_count*sizeof(uint16_t);
            lib->symbol_strtab = file_offset;
            JOB_READ(3, lib->symbol_base, lib->symbol_strtab - lib->symbol_base, NULL);
        }

        // Load longnames
        case 3: {
            JOB_READ(4, lib->longnames_base, sizeof(COFF_ArchiveMemberHeader), NULL);
        }

        // Parse longnames
        case 4: {
            size_t file_offset = lib->longnames_base - sizeof(COFF_ArchiveMemberHeader);

            COFF_ArchiveMemberHeader longnames;
            memcpy(&longnames, &raw_map[file_offset], sizeof(COFF_ArchiveMemberHeader));
            if (memcmp(longnames.name, (char[16]) { "//              " }, 16) == 0) {
                lib->longnames_size = tb__parse_decimal_int(sizeof(longnames.size), longnames.size);

                // Advance
                file_offset += sizeof(COFF_ArchiveMemberHeader) + lib->longnames_size;
                file_offset = (file_offset + 1u) & ~1u;
            }

            lib->string_tail = 4096;
            JOB_READ(5, lib->symbol_strtab, 4096, NULL);
        }

        // Parse lazy symbols
        case 5: {
            cuikperf_region_start("chunk", NULL);

            size_t symbol_i = lib->symbol_i;
            size_t string_head = lib->string_head;
            size_t string_tail = lib->string_tail;
            size_t second_size = lib->second_size;

            size_t strtab_size = (lib->second_base + second_size) - lib->symbol_strtab;
            char* strtab = (char*) &lib->header.file->raw_map[lib->symbol_strtab];
            assert(strtab != NULL);

            bool distribute = false; // l->jobs.pool != NULL && tpool_num_threads(l->jobs.pool) > 1;
            while (symbol_i < lib->symbol_count) {
                // Fetch ahead on the string table, the only reason we're even breaking it
                // up into pieces is to allow avoid stalling all other workers and read requests
                // while we wait for ours.
                if (lib->string_split == string_tail || (string_tail != strtab_size && string_head >= string_tail)) {
                    size_t readahead = string_tail + LAZY_IMPORT_STRTAB_MUNCH;
                    if (readahead > strtab_size) {
                        readahead = strtab_size;
                    }
                    assert(readahead == strtab_size || readahead > string_head);
                    // printf("READAHEAD %p %08zx %08zx\n", lib, string_head, readahead);

                    // writeback
                    lib->symbol_i    = symbol_i;
                    lib->string_head = string_head;
                    lib->string_tail = string_tail = readahead;
                    lib->header.stage = 1;

                    cuikperf_region_end();
                    JOB_READ(5, lib->symbol_strtab + string_head, readahead - string_head, NULL);
                    cuikperf_region_start("chunk", NULL);
                }

                assert(string_head < strtab_size);
                uint16_t offset_index = lib->symbols[symbol_i] - 1;
                const char* name = &strtab[string_head];

                size_t next;
                if (lib->string_split) {
                    next = ideally_fast_skip16(strtab, 1, lib->string_split, string_tail);
                    lib->string_split = 0;
                } else {
                    next = ideally_fast_skip16(strtab, 1, string_head, string_tail);
                }

                // check for split strings and insert it before properly continuing,
                // technically you can run into an arbitrary count of these but
                // it's unlikely you'll hit even 2 in a row.
                if (string_tail != strtab_size && next == string_tail && strtab[next - 1] != 0) {
                    lib->string_split = next;
                    continue;
                }

                #if 0
                printf("SYMBOL %zu | %d | %d | %s\n", symbol_i, offset_index, lib->members[offset_index], name);
                #endif

                if (!distribute) {
                    assert(offset_index < lib->member_count);
                    assert(lib->members[offset_index] < lib->header.file->size);
                    TB_LinkerSymbol* s = tb_linker_moar_mem(sizeof(TB_LinkerSymbol));
                    *s = (TB_LinkerSymbol){
                        .name   = { (const uint8_t*) name, (next - string_head) - 1 },
                        .tag    = TB_LINKER_SYMBOL_LAZY,
                        .lazy   = { NULL, lib, lib->members[offset_index] },
                    };
                    s = tb_linker_symbol_insert(l, s, true);

                    uint64_t start = __rdtsc();
                    lib->hashes += symhs_hash(s);
                    lib->total_time += __rdtsc() - start;
                }

                symbol_i += 1, string_head = next;
                if (distribute && symbol_i - lib->munch_start == LAZY_IMPORT_BATCH_SIZE) {
                    void* args[3] = { lib, (void*) lib->munch_start, (void*) lib->munch_start_sym };
                    l->jobs.count += 1;
                    tb_linker_job_submit_N(l, lazy_import_task, 3, args);

                    lib->munch_start = symbol_i;
                    lib->munch_start_sym = string_head;
                }
            }

            if (distribute && symbol_i != lib->munch_start) {
                void* args[3] = { lib, (void*) lib->munch_start, (void*) lib->munch_start_sym };
                tb_linker_job_submit_N(l, lazy_import_task, 3, args);
            }

            printf("THROUGHPUT %.3f ns/op | %.3f clk/op | %d\n", cuik_special_time(lib->total_time) / (double) lib->symbol_count, lib->total_time / (double) lib->symbol_count, lib->symbol_count);
            cuikperf_region_end();
            return true;
        }
    }

    tb_todo();
}

