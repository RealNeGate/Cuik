#include "../front/atoms.h"

typedef struct InternalFile InternalFile;
struct InternalFile {
    InternalFile* next;
    const char* name;
    size_t size;
    char data[];
};

typedef struct LoadResult {
    bool found;

    size_t length;
    char* data;
} LoadResult;

extern InternalFile* cuik__ifiles_root;

static LoadResult get_file(const char* path) {
    #ifdef _WIN32
    // actual file reading
    HANDLE file = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    if (file == INVALID_HANDLE_VALUE) {
        return (LoadResult){ .found = false };
    }

    LARGE_INTEGER file_size;
    if (!GetFileSizeEx(file, &file_size)) {
        fprintf(stderr, "error: could not check file size of '%s'!\n", path);
        return (LoadResult){ .found = false };
    }

    // normal file with a normal length
    if (file_size.HighPart) {
        fprintf(stderr, "error: file '%s' is too big!\n", path);
        return (LoadResult){ .found = false };
    }

    char* buffer = cuik__valloc((file_size.QuadPart + 16 + 4095) & ~4095);
    DWORD bytes_read;
    if (!ReadFile(file, buffer, file_size.LowPart, &bytes_read, NULL)) {
        fprintf(stderr, "error: could not read file '%s'!\n", path);
        return (LoadResult){ .found = false };
    }

    CloseHandle(file);

    // fat null terminator
    memset(&buffer[file_size.QuadPart], 0, 16);
    cuiklex_canonicalize(file_size.QuadPart, buffer);

    return (LoadResult){ .found = true, .length = file_size.QuadPart, buffer };
    #else
    // actual file reading
    FILE* file = fopen(path, "rb");
    if (!file) {
        return (LoadResult){ .found = false };
    }

    int descriptor = fileno(file);

    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        fprintf(stderr, "Could not figure out file size: %s\n", path);
        return (LoadResult){ .found = false };
    }

    size_t len = file_stats.st_size;
    char* text = cuik__valloc((len + 16 + 4095) & ~4095);

    fseek(file, 0, SEEK_SET);
    len = fread(text, 1, len, file);
    fclose(file);

    // fat null terminator
    memset(&text[len], 0, 16);
    cuiklex_canonicalize(len, text);

    return (LoadResult){ .found = true, .length = len, .data = text };
    #endif
}

typedef enum {
    PATH_NORMAL,  //  baz.c
    PATH_DIR,     //  foo/
    PATH_ZIP,     //  bar.zip/
} PathPieceType;

const char* read_path(PathPieceType* out_t, const char* str) {
    PathPieceType t = PATH_NORMAL;
    const char* ext = NULL;

    for (; *str; str++) {
        if (*str == '/' || *str == '\\') {
            t = (ext+4 == str) && strncmp(ext, ".zip", 3) == 0 ? PATH_ZIP : PATH_DIR;
            str++;
            break;
        } else if (*str == '.') {
            ext = str;
        }
    }

    *out_t = t;
    return str;
}

// TODO(NeGate): implement smarter cache for open zip files
typedef struct {
    char path[FILENAME_MAX];
    FileMap file_map;
    struct zip_t* zip;
    NL_Strmap(int) listing;
} OpenZipFile;

enum { MAX_OPEN_ZIP_FILES = 32 };
static OpenZipFile zip_cache[MAX_OPEN_ZIP_FILES];

thread_local static Arena str_arena;

static uint32_t fnv1a(size_t len, const void *key) {
    const uint8_t* data = key;
    uint32_t h = 0x811C9DC5;
    for (size_t i = 0; i < len; i++) {
        h = (data[i] ^ h) * 0x01000193;
    }

    return h;
}

// looks dumb but C doesn't have multireturns
static struct R_get_file_in_zip {
    OpenZipFile* zip;
    int index;
} get_file_in_zip(const char* og_path, const char* path) {
    // we do a little trolling... essentially the windows file system is SOOOO BADD
    // that using a fucking ZIP file to store my stuff is not even that bad of an idea.
    char tmp[FILENAME_MAX];
    int zip_path_len = snprintf(tmp, FILENAME_MAX, "%.*s", (int)(path - og_path) - 1, og_path);

    char* newpath = tmp + zip_path_len + 1;
    strncpy(newpath, path, FILENAME_MAX - (zip_path_len + 1));

    int total = zip_path_len + strlen(path) + 1;
    for (size_t i = 0; i < total; i++) {
        if (tmp[i] == '\\') tmp[i] = '/';
        if (tmp[i] >= 'A' && tmp[i] <= 'Z') tmp[i] -= ('A' - 'a');
    }

    // compute hash
    uint32_t hash = fnv1a(zip_path_len, tmp);
    OpenZipFile* open_zip = &zip_cache[hash & (MAX_OPEN_ZIP_FILES - 1)];

    if (strcmp(tmp, open_zip->path) != 0) {
        // Invalidate old zip
        CUIK_TIMED_BLOCK("invalidate_open_zip") {
            if (open_zip->zip != NULL) {
                printf("[LOG] invalidating old zip: %s\n", tmp);
                zip_close(open_zip->zip);
                close_file_map(&open_zip->file_map);
            }

            strncpy(open_zip->path, tmp, FILENAME_MAX);
        }

        CUIK_TIMED_BLOCK("zip_open") {
            open_zip->file_map = open_file_map(open_zip->path);
            open_zip->zip = zip_stream_open(open_zip->file_map.data, open_zip->file_map.size, 0, 'r');
            if (open_zip->zip == NULL) {
                return (struct R_get_file_in_zip){ .index = -1 };
            }
        }

        CUIK_TIMED_BLOCK("zip_index") {
            size_t n = zip_entries_total(open_zip->zip);
            for (size_t i = 0; i < n; ++i) {
                zip_entry_openbyindex(open_zip->zip, i);
                const char* name = zip_entry_name(open_zip->zip);

                if (!zip_entry_isdir(open_zip->zip)) {
                    size_t len = strlen(name);
                    Atom newstr = arena_alloc(&str_arena, len + 1, 1);

                    for (size_t j = 0; j < len; j++) {
                        if (name[j] == '\\') {
                            newstr[j] = '/';
                        } else if (name[j] >= 'A' && name[j] <= 'Z') {
                            newstr[j] = name[j] - ('A' - 'a');
                        } else {
                            newstr[j] = name[j];
                        }
                    }
                    newstr[len] = 0;

                    // printf("  %s\n", newstr);
                    nl_strmap_put_cstr(open_zip->listing, newstr, i);
                }
                zip_entry_close(open_zip->zip);
            }
        }
    }

    // Load file from ZIP
    ptrdiff_t i = nl_strmap_get_cstr(open_zip->listing, newpath);
    return (struct R_get_file_in_zip){
        open_zip,
        i >= 0 ? open_zip->listing[i] : -1
    };
}

InternalFile* find_internal_file(const char* name) {
    InternalFile* f = cuik__ifiles_root;
    for (; f != NULL; f = f->next) {
        if (strcmp(name, f->name) == 0) return f;
    }

    return NULL;
}

bool cuik_canonicalize_path(Cuik_Path* restrict output, const char* input) {
    #ifdef _WIN32
    char* filepart;
    if (GetFullPathNameA(input, FILENAME_MAX, output->data, &filepart) == 0) {
        return false;
    }

    // Convert file paths into something more comfortable
    // The windows file paths are case insensitive
    char* p = output->data;
    for (; *p; p++) {
        if (*p == '\\') {
            *p = '/';
        } else if (*p >= 'A' && *p <= 'Z') {
            *p -= ('A' - 'a');
        }
    }

    output->length = p - output->data;
    return true;
    #else
    return realpath(input->data, output->data) != NULL;
    #endif
}

bool cuikpp_locate_file(void* user_data, const Cuik_Path* restrict input, Cuik_Path* output) {
    if (cuik_path_is_in(input, "$cuik")) {
        InternalFile* f = find_internal_file(input->data + sizeof("$cuik"));
        if (f) {
            *output = *input;
            return true;
        } else {
            return false;
        }
    }

    const char* path = input->data;
    while (*path) {
        PathPieceType t;
        path = read_path(&t, path);
        if (t == PATH_ZIP) {
            *output = *input;
            return get_file_in_zip(input->data, path).index >= 0;
        }
    }

    #ifdef _WIN32
    bool found = GetFileAttributesA(input->data) != INVALID_FILE_ATTRIBUTES;
    #else
    struct stat buffer;
    bool found = stat(input->data, &buffer) == 0;
    #endif

    if (found) {
        cuik_canonicalize_path(output, input->data);
    }

    return found;
}

bool cuikpp_default_fs(void* user_data, const Cuik_Path* restrict input, Cuik_FileResult* output) {
    if (input->length == 0) {
        if (user_data == NULL) return false;
        String source = *(String*) user_data;

        // allocate proper buffer for source
        char* buffer = cuik__valloc(source.length + 16);
        memcpy(buffer, source.data, source.length);
        memset(buffer + source.length, 0, 16);

        cuiklex_canonicalize(source.length, buffer);

        output->length = source.length;
        output->data = buffer;
        return true;
    } else if (cuik_path_is_in(input, "$cuik")) {
        InternalFile* f = find_internal_file(input->data + sizeof("$cuik"));
        if (f == NULL) return false;

        output->length = f->size;
        output->data = f->data;
        return true;
    }

    const char* path = input->data;
    while (*path) {
        PathPieceType t;
        path = read_path(&t, path);

        if (t == PATH_ZIP) {
            CUIK_TIMED_BLOCK("zip_read") {
                struct R_get_file_in_zip r = get_file_in_zip(input->data, path);
                assert(r.index >= 0);

                struct zip_t* zip = r.zip->zip;
                zip_entry_openbyindex(zip, r.index);

                size_t size = zip_entry_size(zip);
                void* buf = cuik__valloc(size + 16);
                CUIK_TIMED_BLOCK("zip_entry_noallocread") {
                    zip_entry_noallocread(zip, (void*) buf, size);
                }

                zip_entry_close(zip);
                cuiklex_canonicalize(size, buf);

                output->length = size;
                output->data = buf;
            }

            return true;
        }
    }

    LoadResult file = get_file(input->data);
    if (!file.found) {
        return false;
    }

    output->length = file.length;
    output->data = file.data;
    return true;
}

void cuiklex_canonicalize(size_t length, char* data) {
    uint8_t* text = (uint8_t*) data;

    #if !USE_INTRIN
    for (size_t i = 0; i < length; i++) {
        if (text[i] == '\t') text[i] = ' ';
        if (text[i] == '\v') text[i] = ' ';
        if (text[i] == 12)   text[i] = ' ';
    }
    #else
    length = (length + 15ull) & ~15ull;

    // NOTE(NeGate): This code requires SSE4.1, it's not impossible to make
    // ARM variants and such but yea.
    for (size_t i = 0; i < length; i += 16) {
        __m128i bytes = _mm_load_si128((__m128i*)&text[i]);

        // Replace all \t and \v with spaces
        __m128i test_ident = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\t'));
        test_ident = _mm_or_si128(test_ident, _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\v')));
        test_ident = _mm_or_si128(test_ident, _mm_cmpeq_epi8(bytes, _mm_set1_epi8(12)));

        bytes = _mm_blendv_epi8(bytes, _mm_set1_epi8(' '), test_ident);
        _mm_store_si128((__m128i*)&text[i], bytes);
    }
    #endif
}
