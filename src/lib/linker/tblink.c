#include <common.h>
#include <cuik.h>
#include <tb.h>
#include <time.h>

#define NL_STRING_MAP_IMPL
#include <string_map.h>

#include <stdio.h>
#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

#pragma pack(push, 1)
typedef struct {
	uint16_t e_magic;    // Magic number
	uint16_t e_cblp;     // Bytes on last page of file
	uint16_t e_cp;       // Pages in file
	uint16_t e_crlc;     // Relocations
	uint16_t e_cparhdr;  // Size of header in paragraphs
	uint16_t e_minalloc; // Minimum extra paragraphs needed
	uint16_t e_maxalloc; // Maximum extra paragraphs needed
	uint16_t e_ss;       // Initial (relative) SS value
	uint16_t e_sp;       // Initial SP value
	uint16_t e_csum;     // Checksum
	uint16_t e_ip;       // Initial IP value
	uint16_t e_cs;       // Initial (relative) CS value
	uint16_t e_lfarlc;   // File address of relocation table
	uint16_t e_ovno;     // Overlay number
	uint16_t e_res[4];   // Reserved words
	uint16_t e_oemid;    // OEM identifier (for e_oeminfo)
	uint16_t e_oeminfo;  // OEM information; e_oemid specific
	uint16_t e_res2[10]; // Reserved words
    uint32_t e_lfanew;   // File address of new exe header
} PE_DosHeader;

typedef struct {
	uint32_t magic; // PE\0\0 or 0x00004550
	uint16_t machine;
	uint16_t section_count;
	uint32_t timestamp;
	uint32_t symbol_table;
	uint32_t symbol_count;
	uint16_t size_of_optional_header;
	uint16_t characteristics;
} PE_Header;

typedef struct {
    uint32_t virtual_address;
    uint32_t size;
} PE_ImageDataDirectory;

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16
typedef struct {
    uint16_t magic;
    uint8_t major_linker_version;
    uint8_t minor_linker_version;
    uint32_t size_of_code;
    uint32_t size_of_initialized_data;
    uint32_t size_of_uninitialized_data;
    uint32_t entrypoint;
    uint32_t base_of_code;
    uint64_t image_base;
    uint32_t section_alignment;
    uint32_t file_alignment;
    uint16_t major_os_ver;
    uint16_t minor_os_ver;
    uint16_t major_image_ver;
    uint16_t minor_image_ver;
    uint16_t major_subsystem_ver;
    uint16_t minor_subsystem_ver;
    uint32_t win32_version_value;
    uint32_t size_of_image;
    uint32_t size_of_headers;
    uint32_t checksum;
    uint16_t subsystem;
    uint16_t dll_characteristics;
    uint64_t size_of_stack_reserve;
    uint64_t size_of_stack_commit;
    uint64_t size_of_heap_reserve;
    uint64_t size_of_heap_commit;
	uint32_t loader_flags;
	uint32_t rva_size_count;
    PE_ImageDataDirectory data_directories[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} PE_OptionalHeader64;

typedef struct { // size 40 bytes
	char name[8];
	uint32_t virtual_size;
	uint32_t virtual_address;
	uint32_t size_of_raw_data;
	uint32_t pointer_to_raw_data;
	uint32_t pointer_to_relocs;
	uint32_t pointer_to_linenos;
	uint16_t relocation_count;
    uint16_t linenos_count;
	uint32_t characteristics;
} PE_SectionHeader;
#pragma pack(pop)

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

static NL_Strmap(void*) import_nametable = {0};

static void zero_pad_file(FILE* file, size_t align) {
    size_t align_mask = align - 1;
    char pad = 0;

    long i = ftell(file);
    long end = (i + align_mask) & ~align_mask;
    while (i < end) {
        fwrite(&pad, 1, 1, file);
        i += 1;
    }
}

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

static FILE* locate_file(Cuik_Linker* l, OS_String path) {
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
    /*printf("\nSUMMARY FOR %" OS_STR_FMT "\n", filename);

    for (size_t j = 0; j < obj->section_count; j++) {
        printf("%-20.*s    %zu\n",
            (int)obj->sections[j].name.length,
            (char*)obj->sections[j].name.data,
            obj->sections[j].raw_data.length);
    }*/
}

static void import_symbols(TB_ObjectFile* obj) {
    //printf("Symbols:\n");
    for (size_t j = 0; j < obj->symbol_count; j++) {
        NL_Slice key = {obj->symbols[j].name.length, obj->symbols[j].name.data};
        nl_strmap_put(import_nametable, key, NULL);

        /*printf("%.*s\n",
            (int)obj->symbols[j].name.length,
            (char*)obj->symbols[j].name.data);*/
    }
}

bool cuiklink_invoke_tb(Cuik_Linker* l, const char* filename) {
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

    char exe_path[FILENAME_MAX];
    sprintf_s(exe_path, FILENAME_MAX, "%s.exe", filename);
    FILE* outf = fopen(exe_path, "wb");

    // write DOS header
    fwrite(dos_stub, sizeof(dos_stub), 1, outf);

    // write PE header
    PE_Header header = {
        .magic = 0x00004550,
        .machine = 0x8664,
        .section_count = 1,
        .timestamp = time(NULL),
        .symbol_table = 0,
        .symbol_count = 0,
        .size_of_optional_header = sizeof(PE_OptionalHeader64),
        .characteristics = 2 /* executable */
    };
    fwrite(&header, sizeof(header), 1, outf);

    long size_of_headers = ftell(outf)
        + sizeof(PE_OptionalHeader64)
        + (header.section_count * sizeof(PE_SectionHeader));

    PE_OptionalHeader64 opt_header = {
        .magic = 0x20b,

        .size_of_code = 4096,
        .size_of_initialized_data = 0,
        .size_of_uninitialized_data = 0,

        .base_of_code = 0x1000,

        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x00400000,
        .entrypoint = 0x1000,

        // 6.0
        .major_os_ver = 6,
        .minor_os_ver = 0,

        // 6.0
        .major_subsystem_ver = 6,
        .minor_subsystem_ver = 0,

        .size_of_image = 0x2000,
        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = 2 /* WINDOWS_GUI */,

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit  = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .data_directories = {
            0 // fill in the shits here
        }
    };
    fwrite(&opt_header, sizeof(opt_header), 1, outf);

    // generate text section
    {
        long data_pos = ftell(outf) + sizeof(PE_SectionHeader);
        data_pos = (data_pos + 0x1FF) & ~0x1FF;

        PE_SectionHeader sec = {
            .name = { ".text" },
            .virtual_size = 3,
            .virtual_address = 0x1000,
            .size_of_raw_data = 0x200,
            .pointer_to_raw_data = data_pos,
            .characteristics = 0x60000020,
        };
        fwrite(&sec, sizeof(sec), 1, outf);

        zero_pad_file(outf, 0x200);

        uint8_t data[3] = { 0x31, 0xC0, 0xC3 };
        fwrite(&data, sizeof(data), 1, outf);

        char* f = malloc(0x200 - 3);
        memset(f, 0xCC, 0x200 - 3);
        fwrite(f, 0x200 - 3, 1, outf);
        free(f);
    }

    fclose(outf);
    return true;
}
