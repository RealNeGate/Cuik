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

#if 0 // CONFIG_HAS_TB
#define cuikperf_region_start(...) (0)
#define cuikperf_region_end() (0)

#undef CUIK_TIMED_BLOCK
#undef CUIK_TIMED_BLOCK_ARGS
#define CUIK_TIMED_BLOCK(label) for (uint64_t __i = (0); __i < 1; __i++)
#define CUIK_TIMED_BLOCK_ARGS(label, extra) for (uint64_t __i = (0); __i < 1; __i++)
#endif

enum {
    // should at least be 60 (archive member header) + 20 (COFF header), rounded
    // to the next pow2.
    PREFETCH_BLOCK_SIZE = 1024,
    FILE_BLOCK_SIZE     = 4*1024,
};

typedef void TB_LinkerAppendFn(TPool* pool, void** args);
typedef struct TB_LinkerSymbol TB_LinkerSymbol;

typedef struct TB_LinkerObject TB_LinkerObject;
typedef struct TB_LinkerArchive TB_LinkerArchive;

typedef struct BCache_Job BCache_Job;
typedef bool BCache_Fn(TB_Linker* l, BCache_Job* job, void* arg);

enum {
    BCACHE_MAX_SNOOPERS = 1024,
};

typedef struct {
    _Atomic(uint64_t) reserve;
    _Atomic(uint64_t) commit;
} BCache_Row;

typedef struct {
    int fd;
    size_t size;
    size_t row_count;
    // Virtual address range for the file, we're managing our own
    // cache.
    uint8_t* raw_map;
    // Track which blocks are ready in this request
    BCache_Row rows[];
} BCache_File;

// This is a coroutine that's capable of BCache read requests.
struct BCache_Job {
    BCache_File* file;

    // Wait state
    struct {
        // Defines the commit word we're snooping
        uint32_t snoop_row;

        // When doing reads, we only snoop one megablock (64 blocks)
        // at a time and so we have a system to automatically begin looking
        // for the next set of blocks when those come back.
        uint32_t head;
        uint32_t tail;

        // Represents what we need the block word to look like
        // for us to wake this task up again.
        _Atomic uint64_t row_target;

        _Atomic int lock;
    } wait;

    // Run state
    int state;
    BCache_Fn* fn;
    void* arg;
};

// block cache, we're not depending on the OS for file caching so
// we need an approach to avoiding duplicate block reads.
typedef struct {
    _Atomic(BCache_Job*) entries[BCACHE_MAX_SNOOPERS];
} BCache;

// basically an object file or a library
struct TB_LinkerObject {
    TB_Slice name;
    TB_Linker* linker;
    uint64_t time;

    TB_LinkerObject* parent;

    // returns true, when the job has completed
    bool (*step)(TB_Linker* l, BCache_Job* job, TB_LinkerObject* obj, TB_Slice prefetch);

    struct {
        BCache_File* file;
        size_t offset;
        size_t size;
        bool classify;

        int stage;

        // BigCOFF
        bool is_big;
        size_t skip_header;

        uint8_t* sections;

        size_t symbol_count;
        size_t section_count;
        size_t symbol_table_pos;
        uint8_t* symbol_table;
    };

    #ifdef CONFIG_HAS_TB
    // if not-NULL, the sections for the are in a TB_Module.
    TB_Module* module;
    #endif

    // Some section piece was marked that is contained by this object
    _Atomic bool live;
};

struct TB_LinkerArchive {
    TB_LinkerObject header;

    size_t prefetch_pos;

    // offset to the first byte in the archive member (skipping the header)
    size_t second_base;
    size_t longnames_base;

    size_t second_size;
    size_t longnames_size;

    uint32_t symbol_base;
    uint32_t symbol_count;
    uint32_t member_count;
    bool loaded_members;

    uint16_t* symbols;
    uint32_t* members;
    uint32_t symbol_strtab;

    // Lazy parser state
    size_t symbol_i, string_head, string_tail;
    size_t munch_start, munch_start_sym;
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
    _Atomic(TB_LinkerSectionPiece*) next;

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

    _Atomic(TB_LinkerPieceFlags) flags;

    TB_LinkerSection* parent;
    TB_LinkerObject* obj;

    // offset wrt the final file.
    uint32_t offset, size, align_log2;
    // for consistent layout (since we're doing so much parallel stuff)
    uint64_t order;

    // mostly for COMDAT associative sections
    DynArray(TB_LinkerSectionPiece*) assoc;

    // mostly compact table from per-file symbol index -> symbol (some
    // indices are NULL because they map to COFF aux data)
    TB_LinkerSymbol** symbol_map;

    // points to where the object-file specific relocation data lies
    uint32_t reloc_count;
    uint32_t reloc_size;
    uint32_t reloc_pos;
    // how many data bytes
    uint32_t buffer_size;

    void* relocs;

    union {
        // kind=PIECE_FILE
        struct {
            BCache_File* file;
            uint32_t file_offset;
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
    TB_LINKER_SYMBOL_COMDAT = 8,
} TB_LinkerSymbolFlags;

typedef enum {
    TB_LINKER_COMDAT_NONE,

    TB_LINKER_COMDAT_NODUP,

    // pick whichever (for threading reasons we'll use
    // the piece's order info for consistency).
    TB_LINKER_COMDAT_ANY,
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
    TB_LinkerSymbolTag tag;

    struct {
        _Atomic(TB_LinkerSymbolFlags) flags;

        // cache during the mark and export phase to
        // avoid looking up existing entries again.
        _Atomic(TB_LinkerSymbol*) root;
    };

    union {
        // for unknown syms (and lazy symbols)
        struct {
            _Atomic(TB_LinkerSymbol*) weak_alt;
        };

        // for normal symbols
        struct {
            TB_LinkerSectionPiece* piece;
            uint32_t secrel;
        } normal;

        uint32_t absolute;
        uint32_t imagebase;

        struct {
            _Atomic(TB_LinkerSymbol*) weak_alt;
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

    // if the input file is missing a name or size, we handle that here alongside
    // parsing the header to know what we're even looking at.
    bool (*classify_input)(TB_Linker* l, BCache_Job* job, TB_LinkerObject* obj);

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

    uint32_t trampoline_rva;
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

    BCache bcache;

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

BCache_Job* tb_linker_new_job(TB_Linker* l, BCache_File* file, BCache_Fn* fn, void* arg);
void tb_linker_job_read(TB_Linker* l, BCache_Job* job, size_t offset, size_t size, void** buffer);
bool tb_linker_job_read_FAST(TB_Linker* l, BCache_Job* job, size_t offset, size_t size, void** buffer);

void  tb_linker_worker_init(TB_Linker* l);
void* tb_linker_moar_mem(size_t size);
void  tb_linker_free_mem(void* ptr, size_t size);

char* tb_linker_local_push(void);
void  tb_linker_local_pop(char* savepoint);
void* tb_linker_alloc_local(size_t size);

TB_Slice tb_linker_get_base_name(TB_Slice name);

