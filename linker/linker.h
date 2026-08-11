#pragma once
#include <common.h>
#include <futex.h>
#include <pool.h>
#include <file_map.h>
#include <dyn_array.h>
#include <tb_linker.h>

#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

#ifdef CONFIG_HAS_TB
#include <tb.h>
#endif

#include <nbhm.h>

#if 0 // CONFIG_HAS_TB
#include "../tb/tb_internal.h"
#endif

enum {
    // 60 (archive member header) + 20 (COFF header), rounded to the next pow2
    PREFETCH_BLOCK_SIZE = 256,
    FILE_BLOCK_SIZE = 4096,
};

typedef void TB_LinkerAppendFn(TPool* pool, void** args);
typedef struct TB_LinkerSymbol TB_LinkerSymbol;

typedef struct TB_LinkerObject TB_LinkerObject;
typedef struct TB_LinkerArchive TB_LinkerArchive;

typedef struct {
    // relative to the FD
    uint32_t offset;
    uint32_t size;

    // track how many blocks are missing before we can use this range.
    _Atomic(int) io_rem;

    // track if we're ready to issue tasks on it.
    _Atomic(TB_LinkerSectionPiece*) pending;
} TB_CacheRange;

// basically an object file or a library
struct TB_LinkerObject {
    TB_Slice name;
    TB_Linker* linker;
    uint64_t time;

    TB_LinkerObject* parent;

    // returns true if we're able to process the input file without extra read requests
    bool (*fetch)(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset);
    void (*process)(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset);

    struct {
        int fd;
        size_t offset;
        size_t size;

        int stage;

        // this is the first peek, so we can get a look at the magic numbers and
        // the rest of the header.
        uint8_t* prefetch_page;
        uint8_t* file_bottom;

        size_t symbol_table_pos;
        uint8_t* symbol_table;
    };

    struct {
        // The cache region is completely loaded from the start, because it's small.
        bool fully_resident;

        // Cache for the relocations and section data, aka the stuff which is
        // gonna require grabbing arrays which may or may not share the same file
        // block as another.
        uint64_t cache_lo;
        uint64_t cache_hi;
        char* cache_data;

        // Tracks the sorted ranges
        DynArray(TB_CacheRange) cache_ranges;

        // Track which pages have issued reads
        _Atomic(uint64_t)* reserve;
    };

    #ifdef CONFIG_HAS_TB
    // if not-NULL, the sections for the are in a TB_Module.
    TB_Module* module;
    #endif

    // Some section piece was marked that is contained by this object
    _Atomic bool live;

    // Keep track of how many reads we need to complete before advancing, usually
    // the answer is like 1 or 2.
    _Atomic int io_rem;

    // Windows-specific debug stuff
    TB_LinkerSectionPiece* debug_s;
    TB_LinkerSectionPiece* debug_t;
};

struct TB_LinkerArchive {
    TB_LinkerObject header;

    // offset to the first byte in the archive member (skipping the header)
    size_t second_base;
    size_t longnames_base;

    size_t second_size;
    size_t longnames_size;

    char* second_longnames;

    uint32_t symbol_count;
    uint32_t member_count;

    uint16_t* symbols;
    uint32_t* members;
    char* symbol_strtab;

    TB_Slice longnames;
};

typedef enum {
    TB_LINKER_PIECE_CODE      = 1,
    TB_LINKER_PIECE_COMDAT    = 2,

    // by the time GC is done, this is resolved and we can
    // assume any pieces without this set are dead.
    TB_LINKER_PIECE_LIVE      = 4,
} TB_LinkerPieceFlags;

typedef struct TB_LinkerReloc {
    uint32_t src_offset;
    uint16_t type;
    int16_t addend;
    TB_LinkerSymbol* target;
} TB_LinkerReloc;

// it's a linked list so i can do dumb insertion while parsing the pieces, once
// we're doing layouting a sorted array will be constructed.
struct TB_LinkerSectionPiece {
    union {
        _Atomic(TB_LinkerSectionPiece*) next;
        TB_LinkerSectionPiece* next2;
    };

    enum {
        // doesn't get written to the image, just describes virtual memory
        PIECE_BSS,
        // Write from memory
        PIECE_BUFFER,
        // Range within a file
        PIECE_FILE,
        // Write TB_ModuleSection
        PIECE_MODULE_SECTION,
        // Write the TB module's pdata section
        PIECE_PDATA,
    } kind;

    TB_LinkerSection* parent;
    TB_LinkerObject* obj;

    // offset wrt the final file.
    size_t offset, size, align_log2;
    // for consistent layout (since we're doing so much parallel stuff)
    uint64_t order;

    // mostly for COMDAT associative sections
    TB_LinkerSectionPiece* comdat_parent;
    DynArray(TB_LinkerSectionPiece*) assoc;

    // mostly compact table from per-file symbol index -> symbol (some
    // indices are NULL because they map to COFF aux data)
    TB_LinkerSymbol** symbol_map;
    // object-file specific
    void* section_header;

    // points to where the object-file specific relocation data lies
    size_t reloc_count;
    size_t reloc_size;
    size_t reloc_pos;

    void* relocs;

    // how many data bytes
    uint32_t buffer_size;

    _Atomic(TB_LinkerPieceFlags) flags;

    union {
        // kind=PIECE_FILE
        struct {
            uint32_t file_offset;
            int fd;
        };

        // kind=PIECE_BUFFER
        struct {
            const uint8_t* buffer;
        };

        // kind=PIECE_MODULE_SECTION
        TB_ModuleSection* ir_section;
    };
};

typedef enum {
    TB_LINKER_SECTION_DISCARD = 1,
} TB_LinkerSectionFlags;

typedef struct {
    TB_Slice name;

    uint32_t number;
    uint32_t name_pos;
    uint32_t flags;

    // virtual layout (vsize is just size with virtual alignment)
    uint64_t address;

    size_t offset;
    size_t size;

    DynArray(TB_LinkerSection*) sections;
} TB_LinkerSegment;

struct TB_LinkerSection {
    TB_Slice name;
    TB_LinkerSegment* segment;

    TB_LinkerSectionFlags generic_flags;
    uint32_t flags;

    // in-segment layout (size is just the sum of all final raw datas aligned)
    size_t offset;
    size_t size;

    _Atomic size_t piece_count;
    _Atomic(TB_LinkerSectionPiece*) list;

    // once we finish layouting, it's kinda nice to just
    // operate on the array stuff (mostly just fed to the
    // exporter shit)
    DynArray(TB_LinkerSectionPiece*) pieces;
};

static uint64_t tb_linker_section_rva(TB_LinkerSection* s) { return s->segment->address + s->offset; }
static uint64_t tb_linker_section_file_pos(TB_LinkerSection* s) { return s->segment->offset + s->offset; }

typedef enum TB_LinkerSymbolTag {
    TB_LINKER_SYMBOL_ABSOLUTE = 0,

    TB_LINKER_SYMBOL_UNKNOWN,

    // the rest of the object hasn't been loaded... yet
    TB_LINKER_SYMBOL_LAZY,

    // external linkage
    TB_LINKER_SYMBOL_NORMAL,

    // used for windows stuff as "__ImageBase"
    TB_LINKER_SYMBOL_IMAGEBASE,

    // TB defined
    TB_LINKER_SYMBOL_TB,

    // import thunks
    TB_LINKER_SYMBOL_THUNK,

    // imported from shared object (named with __imp_)
    TB_LINKER_SYMBOL_IMPORT,
} TB_LinkerSymbolTag;

typedef enum TB_LinkerSymbolFlags {
    TB_LINKER_SYMBOL_WEAK   = 1,
    TB_LINKER_SYMBOL_USED   = 2,
    TB_LINKER_SYMBOL_GLOBAL = 4,
} TB_LinkerSymbolFlags;

typedef enum {
    TB_LINKER_COMDAT_NONE,

    TB_LINKER_COMDAT_NODUP,

    // pick whichever (for threading reasons we'll use
    // the piece's order info for consistency).
    TB_LINKER_COMDAT_ANY,

    TB_LINKER_COMDAT_ASSOCATIVE,
} TB_LinkerComdatRule;

typedef struct {
    TB_Slice libpath;

    mtx_t lock;
    DynArray(TB_LinkerSymbol*) thunks;

    void* header;
    uint64_t *iat, *ilt;
} ImportTable;

// all symbols appended to the linker are converted into
// these and used for all kinds of relocation resolution.
struct TB_LinkerSymbol {
    TB_Slice name;

    TB_LinkerSymbolTag  tag;
    TB_LinkerComdatRule comdat;

    struct {
        _Atomic(TB_LinkerSymbolFlags) flags;
        _Atomic(TB_LinkerSymbol*) comdat_assoc;
        _Atomic(TB_LinkerSymbol*) weak_alt;
    };

    union {
        // for normal symbols
        struct {
            TB_LinkerSectionPiece* piece;
            uint32_t secrel;
        } normal;

        uint32_t absolute;
        uint32_t imagebase;

        struct {
            TB_LinkerArchive* lib;
            uint32_t offset;
        } lazy;

        // for IR module symbols
        struct {
            TB_LinkerSectionPiece* piece;
            TB_Symbol* sym;
        } tb;

        // for PE imports
        struct {
            ImportTable* table;
            // this is the location the thunk will call
            uint32_t ds_address;
            // this is the ID of the thunk
            uint32_t thunk_id;
            // TODO(NeGate): i don't remember rn
            uint16_t ordinal;
        } import;

        TB_LinkerSymbol* thunk;
    };
};

typedef struct {
    TB_Slice from, to;
} TB_LinkerCmd;

// Format-specific vtable:
typedef struct TB_LinkerVtbl {
    void (*init)(TB_Linker* l);
    int  (*find_lib)(TB_Linker* l, const char* file_name, char* out_path, size_t* out_size);
    void (*add_input)(TPool* pool, void** args);
    void (*parse_reloc)(TB_Linker* l, TB_LinkerSectionPiece* p, size_t reloc_i, TB_LinkerReloc* out_reloc);
    bool (*export)(TB_Linker* l, const char* file_name);
} TB_LinkerVtbl;

typedef void(*RelocParser)(TB_Linker* l, TB_LinkerSectionPiece* p, size_t reloc_i, TB_LinkerReloc* out_reloc);

typedef struct TB_Linker TB_Linker;
struct TB_Linker {
    TB_Arch target_arch;

    const char* entrypoint;
    TB_WindowsSubsystem subsystem;

    // used for consistent layouting
    _Atomic uint64_t time;

    TB_LinkerVtbl vtbl;

    // namehs
    NBHM symbols;  // TB_LinkerSymbol*
    NBHS sections; // TB_LinkerSection*
    NBHS imports;  // ImportTable*
    // tracking the linker objects
    NBHS objects;  // TB_LinkerObject*

    // sometimes people ask to import
    // the same libs a bunch of times.
    NBHS libs;    // strhs

    NBHS unresolved_symbols;

    // Post layout info:
    DynArray(TB_LinkerSection*) sections_arr;
    DynArray(TB_LinkerSegment*) segments;
    DynArray(ImportTable*) sorted_imports;

    // During symbol inflation, it'll represent which TB_LinkerObject* are waiting to be parsed.
    // During Mark-Live, we track which TB_LinkerSectionPiece* have not been resolved yet.
    DynArray(void*) worklist;
    _Atomic bool defer_jobs;

    size_t trampoline_pos;  // relative to the .text section
    TB_Emitter trampolines; // these are for calling imported functions

    // Exporter info:
    size_t output_cap;
    uint8_t* output;

    // Windows specific:
    //   on windows, we use DLLs to interact with the OS so
    //   there needs to be a way to load these immediately,
    //   imports do just that.
    //
    // this is where all the .reloc stuff from object files goes
    TB_LinkerSectionPiece* main_reloc;
    uint32_t iat_pos;
    // used for a few boring resources like the defaultlib list
    mtx_t lock;
    DynArray(const char*) default_libs;
    DynArray(const char*) libpaths;
    DynArray(TB_LinkerCmd) alternate_names;
    DynArray(TB_LinkerCmd) merges;

    struct {
        TPool* pool;

        _Alignas(64) Futex done;
        _Alignas(64) Futex count;
    } jobs;

    _Alignas(64) bool is_exporting;
};

extern thread_local int linker_thread_id;
extern thread_local bool linker_thread_init;
extern thread_local TB_Arena linker_tmp_arena;
extern thread_local TB_Arena linker_perm_arena;

void tb_linker_unresolved_sym(TB_Linker* l, TB_Slice name);

TB_LinkerSectionPiece* tb_linker_get_piece(TB_Linker* l, TB_LinkerSymbol* restrict sym);
void tb_linker_associate(TB_Linker* l, TB_LinkerSectionPiece* a, TB_LinkerSectionPiece* b);

// TB helpers
size_t tb__get_symbol_pos(TB_Symbol* s);

TB_LinkerSymbol* tb_linker_import_symbol(TB_Linker* l, TB_Slice name);
void tb_linker_lazy_resolve(TB_Linker* l, TB_LinkerSymbol* sym);

size_t tb_linker_apply_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, uint8_t* out, uint32_t section_rva, uint32_t trampoline_rva, size_t reloc_i, size_t head, size_t tail);
void tb_linker_symbol_weak(TB_Linker* l, TB_LinkerSymbol* sym, TB_LinkerSymbol* alt);

TB_LinkerSymbol* tb_linker_symbol_insert(TB_Linker* l, TB_LinkerSymbol* sym, bool owned);
TB_LinkerSymbol* tb_linker_new_symbol(TB_Linker* l, size_t len, const char* name);
TB_LinkerSymbol* tb_linker_find_symbol(TB_Linker* l, TB_Slice name);
TB_LinkerSymbol* tb_linker_find_symbol2(TB_Linker* l, const char* name);

TB_LinkerSymbol* tb_linker_root_symbol(TB_Linker* l, TB_LinkerSymbol* sym);
TB_LinkerSegment* tb_linker_find_segment(TB_Linker* linker, const char* name);

// Sections
TB_LinkerSection* tb_linker_find_section(TB_Linker* linker, const char* name);
TB_LinkerSection* tb_linker_find_or_create_section(TB_Linker* linker, size_t len, const char* name, uint32_t flags);

TB_LinkerSectionPiece* tb_linker_append_piece(TB_LinkerSection* section, int kind, size_t size, TB_LinkerObject* obj);

void tb_linker_merge_sections(TB_Linker* linker, TB_LinkerSection* from, TB_LinkerSection* to);
void tb_linker_append_module_section(TB_Linker* l, TB_LinkerObject* mod, TB_ModuleSection* section, uint32_t flags);
void tb_linker_append_module_symbols(TB_Linker* l, TB_Module* m);

uint64_t tb__get_symbol_rva(TB_LinkerSymbol* sym);

size_t tb__pad_file(uint8_t* output, size_t write_pos, char pad, size_t align);
void tb_linker_apply_module_relocs(TB_Linker* l, TB_Module* m, TB_LinkerSection* text, uint8_t* output);
size_t tb__apply_section_contents(TB_Linker* l, uint8_t* output, size_t write_pos, TB_LinkerSection* text, TB_LinkerSection* data, TB_LinkerSection* rdata, size_t section_alignment, size_t image_base);
bool tb__linker_is_library_new(TB_Linker* l, const char* file_name);
void tb__linker_module_parse_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, size_t reloc_i, TB_LinkerReloc* out_reloc);

void tb_linker_push_named(TB_Linker* l, const char* name);
void tb_linker_mark_live(TB_Linker* l);

void tb_linker_job_tail(TB_Linker* l, tpool_task_proc* fn, int count, void** args);
void tb_linker_job_submit_1(TB_Linker* l, tpool_task_proc* fn, void* arg);
void tb_linker_job_submit_N(TB_Linker* l, tpool_task_proc* fn, int count, void** args);
void tb_linker_job_done(TB_Linker* l);

// General linker job
void tb_linker_export_pieces(TB_Linker* l);

// do layouting (requires GC step to complete)
bool tb_linker_layout(TB_Linker* l);
void tb_linker_print_map(TB_Linker* l);
void tb_linker_complete_appends(TB_Linker* l);

void tb_linker_read_imm(int fd, size_t offset, size_t count, void* data);
void tb_linker_read_req(TB_Linker* l, bool hi_prio, size_t offset, size_t size, void* buffer, TB_LinkerObject* obj);
void tb_linker_read_req2(TB_Linker* l, bool hi_prio, int fd, size_t offset, size_t size, void* buffer, tpool_task_proc* fn);
void tb_linker_read_req3(TB_Linker* l, bool hi_prio, int fd, size_t offset, size_t size, void* buffer, tpool_task_proc* fn, void* arg1, void* arg2, _Atomic(int)* io_rem);

void tb_linker_worker_init(TB_Linker* l);
void* tb_linker_moar_mem(size_t size);
