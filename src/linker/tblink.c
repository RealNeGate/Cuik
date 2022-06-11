#include <back/linker.h>
#include <back/tb.h>

#define NL_STRING_MAP_IMPL
#include <string_map.h>

#include <stdio.h>
#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

static NL_Strmap(void*) import_nametable = {0};

static TB_Slice read_file_into_slice(FILE* file) {
    if (!file) {
        printf("Internal error: could not read file\n");
        abort();
    }

    int descriptor = fileno(file);
    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        fclose(file);
        abort();
    }

    fseek(file, 0, SEEK_SET);
    uint8_t* buffer = malloc(file_stats.st_size);
    size_t length_read = fread(buffer, 1, file_stats.st_size, file);

    fclose(file);
    return (TB_Slice){length_read, buffer};
}

static FILE* locate_file(Linker* l, OS_String path) {
#ifdef _WIN32
    FILE* file = _wfopen(path, L"rb");
    if (file) return file;

    wchar_t temp_str[MAX_PATH];
    size_t i = l->libpaths_count;
    OS_String str = l->libpaths_buffer;
    while (i--) {
        swprintf(temp_str, MAX_PATH, L"%s\\%s", str, path);

        file = _wfopen(temp_str, L"rb");
        if (file) {
            return file;
        }
        str += wcslen(str) + 1;
    }

    return NULL;
#else
    FILE* file = fopen(path, "rb");
    if (file) return file;

    char temp_str[MAX_PATH];
    size_t i = l->libpaths_count;
    OS_String str = l->libpaths_buffer;
    while (i--) {
        snprintf(temp_str, MAX_PATH, "%s\\%s", str, path);

        file = fopen(temp_str, "rb");
        if (file) {
            return file;
        }
        str += strlen(str) + 1;
    }

    return NULL;
#endif
}

static void summarize_object_file(TB_ObjectFile* obj, OS_String filename) {
    printf("\nSUMMARY FOR %" OS_STR_FMT "\n", filename);

    for (size_t j = 0; j < obj->section_count; j++) {
        printf("%-20.*s    %zu\n",
               (int)obj->sections[j].name.length,
               (char*)obj->sections[j].name.data,
               obj->sections[j].raw_data.length);
    }
}

static void import_symbols(TB_ObjectFile* obj) {
    printf("Symbols:\n");
    for (size_t j = 0; j < obj->symbol_count; j++) {
        NL_Slice key = {obj->symbols[j].name.length, obj->symbols[j].name.data};
        nl_strmap_put(import_nametable, key, NULL);

        printf("%.*s\n",
               (int)obj->symbols[j].name.length,
               (char*)obj->symbols[j].name.data);
    }
}

bool linker_invoke_tb(Linker* l, const char* filename, bool verbose) {
    import_nametable = nl_strmap_alloc(void*, 1024);

    OS_String str = l->input_file_buffer;
    for (size_t i = 0; i < l->input_file_count; i++) {
        OS_String ext = str_reverse_find_ch(str, '.');

        if (ext) {
            TB_Slice buffer = read_file_into_slice(locate_file(l, str));

            if (str_compare(ext, OS_STR(".lib")) == 0) {
                TB_ArchiveFile* archive = tb_archive_parse_lib(buffer);

                for (size_t i = 0; i < archive->object_file_count; i++) {
                    TB_ObjectFile* obj = tb_object_parse_coff(archive->object_files[i]);
                    summarize_object_file(obj, str);
                    import_symbols(obj);
                }
            } else {
                //fprintf(stderr, "linker: warning: file extension sucks! %"OS_STR_FMT", assumed object file\n", ext);

                TB_ObjectFile* obj = tb_object_parse_coff(buffer);
                summarize_object_file(obj, str);
                import_symbols(obj);
            }

            str += str_length(str) + 1;
        } else {
            printf("tb linker: file has no extension! %" OS_STR_FMT "\n", str);
            return false;
        }
    }

    return true;
}
