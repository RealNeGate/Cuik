#pragma once
#include <common.h>
#include <futex.h>
#include <tb_linker.h>

#if 0
#include "../tb_internal.h"
#endif

typedef struct TB_LinkerSymbol TB_LinkerSymbol;

// just represents some region of bytes, usually in file parsing crap
typedef struct {
    const uint8_t* data;
    size_t length;
} TB_Slice;

// basically an object file
typedef struct TB_LinkerObject TB_LinkerObject;
struct TB_LinkerObject {
    TB_Slice name;

    TB_Linker* linker;
    TB_Slice content;
    uint64_t time;

    TB_LinkerObject* parent;

    #ifdef CONFIG_HAS_LINKER
    // if not-NULL, the sections for the are in a TB_Module.
    TB_Module* module;
    #endif

    // matters if we're doing lazy loading
    _Atomic bool loaded;
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

typedef struct {
    uint32_t rva;
    TB_LinkerSection* section;
} PE_BaseReloc;

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
    TB_LinkerPieceFlags flags;

    // mostly for .pdata crap
    TB_LinkerSectionPiece* assoc;

    // mostly compact table from per-file symbol index -> symbol (some
    // indices are NULL because they map to COFF aux data)
    TB_LinkerSymbol** symbol_map;
    // object-file specific
    void* section_header;

    // points to where the object-file specific relocation data lies
    size_t reloc_count;
    const void* relocs;

    union {
        // kind=PIECE_BUFFER
        struct {
            size_t buffer_size;
            const uint8_t* buffer;
        };

        // kind=PIECE_MODULE_SECTION
        TB_ModuleSection* ir_section;
    };
};

typedef enum {
    TB_LINKER_SECTION_DISCARD = 1,
} TB_LinkerSectionFlags;

struct TB_LinkerSection {
    TB_Slice name;

    TB_LinkerSectionFlags generic_flags;
    uint32_t flags;
    uint32_t number;

    uint32_t name_pos;

    // virtual layout (vsize is just size with virtual alignment)
    uint64_t address;
    size_t vsize;

    // file layout (size is just the sum of all final raw datas aligned)
    size_t offset;
    size_t size;

    _Atomic size_t piece_count;
    _Atomic(TB_LinkerSectionPiece*) list;
};

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
} TB_LinkerSymbolFlags;

typedef enum {
    TB_LINKER_COMDAT_NONE,

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

    // union-find
    union {
        _Atomic(TB_LinkerSymbol*) parent;
        TB_LinkerSymbol* parent2;
    };

    TB_LinkerSymbolTag   tag;
    TB_LinkerSymbolFlags flags;
    TB_LinkerComdatRule  comdat;

    _Atomic(TB_LinkerSymbol*) weak_alt;

    union {
        // for normal symbols
        struct {
            TB_LinkerSectionPiece* piece;
            uint32_t secrel;
        } normal;

        uint32_t absolute;
        uint32_t imagebase;

        struct {
            TB_LinkerObject* obj;
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

////////////////////////////////
// Tasks
////////////////////////////////
typedef struct {
    TB_Linker* linker;
    uint8_t* file;
    TB_LinkerSectionPiece* start;
    TB_LinkerSectionPiece* end;
} ExportTask;

// Format-specific vtable:
typedef struct TB_LinkerVtbl {
    void (*init)(TB_Linker* l);
    FileMap (*find_lib)(TB_Linker* l, const char* file_name, char* out_path);
    void (*append_object)(TPool* pool, TB_LinkerObject* task);
    void (*append_library)(TPool* pool, TB_LinkerObject* task);
    void (*parse_reloc)(TB_Linker* l, TB_LinkerSectionPiece* p, size_t reloc_i, TB_LinkerReloc* out_reloc);
    bool (*export)(TB_Linker* l, const char* file_name);
} TB_LinkerVtbl;

typedef struct TB_Linker {
    TB_Arch target_arch;

    const char* entrypoint;
    TB_WindowsSubsystem subsystem;

    // used for consistent layouting
    _Atomic uint64_t time;

    TB_LinkerVtbl vtbl;

    // namehs
    NBHS symbols;
    NBHS sections;
    NBHS imports;
    // tracking the linker objects
    NBHS objects; // -> TB_LinkerObject*

    // sometimes people ask to import
    // the same libs a bunch of times.
    NBHS libs; // strhs

    NBHS unresolved_symbols;

    DynArray(const char*) libpaths;

    // we track which symbols have not been resolved yet
    DynArray(TB_LinkerSectionPiece*) worklist;

    size_t trampoline_pos;  // relative to the .text section
    TB_Emitter trampolines; // these are for calling imported functions

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

    _Alignas(64) struct {
        TPool* pool;

        Futex done;
        _Atomic uint64_t count;
    } jobs;
} TB_Linker;

extern thread_local bool linker_thread_init;
extern thread_local TB_Arena linker_tmp_arena;
extern thread_local TB_Arena linker_perm_arena;

void tb_linker_unresolved_sym(TB_Linker* l, TB_Slice name);

TB_LinkerSectionPiece* tb_linker_get_piece(TB_Linker* l, TB_LinkerSymbol* restrict sym);
void tb_linker_associate(TB_Linker* l, TB_LinkerSectionPiece* a, TB_LinkerSectionPiece* b);

// TB helpers
size_t tb__get_symbol_pos(TB_Symbol* s);

TB_LinkerSymbol* tb_linker_import_symbol(TB_Linker* l, TB_Slice name);
void tb_linker_lazy_resolve(TB_Linker* l, TB_LinkerSymbol* sym, TB_LinkerObject* obj);

size_t tb_linker_apply_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, uint8_t* out, uint32_t section_rva, uint32_t trampoline_rva, size_t reloc_i, size_t head, size_t tail);

// symbols are technically doing a dumb but concurrent disjoint-set
void tb_linker_symbol_union(TB_Linker* l, TB_LinkerSymbol* leader, TB_LinkerSymbol* other_guy);
TB_LinkerSymbol* tb_linker_symbol_find(TB_LinkerSymbol* sym);
TB_LinkerSymbol* tb_linker_symbol_insert(TB_Linker* l, TB_LinkerSymbol* sym);

TB_LinkerSymbol* tb_linker_find_symbol(TB_Linker* l, TB_Slice name);
TB_LinkerSymbol* tb_linker_find_symbol2(TB_Linker* l, const char* name);

// Sections
TB_LinkerSection* tb_linker_find_section(TB_Linker* linker, const char* name);
TB_LinkerSection* tb_linker_find_or_create_section(TB_Linker* linker, size_t len, const char* name, uint32_t flags);

TB_LinkerSectionPiece* tb_linker_append_piece(TB_LinkerSection* section, int kind, size_t size, TB_LinkerObject* obj);

void tb_linker_merge_sections(TB_Linker* linker, TB_LinkerSection* from, TB_LinkerSection* to);
void tb_linker_append_module_section(TB_Linker* l, TB_LinkerObject* mod, TB_ModuleSection* section, uint32_t flags);
void tb_linker_append_module_symbols(TB_Linker* l, TB_Module* m);

uint64_t tb__compute_rva(TB_Linker* l, TB_Module* m, const TB_Symbol* s);
uint64_t tb__get_symbol_rva(TB_Linker* l, TB_LinkerSymbol* sym);

size_t tb__pad_file(uint8_t* output, size_t write_pos, char pad, size_t align);
void tb_linker_apply_module_relocs(TB_Linker* l, TB_Module* m, TB_LinkerSection* text, uint8_t* output);
size_t tb__apply_section_contents(TB_Linker* l, uint8_t* output, size_t write_pos, TB_LinkerSection* text, TB_LinkerSection* data, TB_LinkerSection* rdata, size_t section_alignment, size_t image_base);

bool tb_linker_push_piece(TB_Linker* l, TB_LinkerSectionPiece* p);
void tb_linker_push_named(TB_Linker* l, const char* name);
void tb_linker_mark_live(TB_Linker* l);

// General linker job
void tb_linker_export_piece(TPool* pool, ExportTask* task);
void tb_linker_export_pieces(TB_Linker* l, DynArray(TB_LinkerSection*) sections, uint8_t* output);

// do layouting (requires GC step to complete)
DynArray(TB_LinkerSection*) tb__finalize_sections(TB_Linker* l);
