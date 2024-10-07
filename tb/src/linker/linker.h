#pragma once
#include "../tb_internal.h"
#include <futex.h>

typedef struct TB_LinkerSymbol TB_LinkerSymbol;

// basically an object file
typedef struct {
    TB_Slice name;

    // if not-NULL, the sections for the are in a TB_Module.
    TB_Module* module;
} TB_LinkerObject;

typedef enum {
    TB_LINKER_PIECE_CODE      = 1,
    TB_LINKER_PIECE_COMDAT    = 2,

    // by the time GC is done, this is resolved and we can
    // assume any pieces without this set are dead.
    TB_LINKER_PIECE_LIVE      = 4,
} TB_LinkerPieceFlags;

typedef struct TB_LinkerReloc {
    uint16_t type;
    uint16_t addend;

    uint32_t src_offset;

    // target
    TB_LinkerSymbol* target;
    TB_LinkerSymbol* alt;
} TB_LinkerReloc;

typedef struct {
    uint32_t rva;
    TB_LinkerSection* section;
} PE_BaseReloc;

// it's a linked list so i can do dumb insertion while parsing the pieces, once
// we're doing layouting a sorted array will be constructed.
struct TB_LinkerSectionPiece {
    _Atomic(TB_LinkerSectionPiece*) next;

    enum {
        // Write from memory
        PIECE_BUFFER,
        // Write TB_ModuleSection
        PIECE_MODULE_SECTION,
        // Write the TB module's pdata section
        PIECE_PDATA,
        // Write the TB module's reloc section
        PIECE_RELOC,
    } kind;

    TB_LinkerSection* parent;
    TB_LinkerObject* obj;

    // offset wrt the final file.
    size_t offset, size;
    // this is for COFF $ management AND for consistent layout
    uint64_t order;
    TB_LinkerPieceFlags flags;

    // mostly for .pdata crap
    TB_LinkerSectionPiece* assoc;

    // ordered by address
    DynArray(TB_LinkerReloc) relocs;

    union {
        // kind=PIECE_NORMAL
        uint32_t file_pos;

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
    TB_LINKER_SYMBOL_COMDAT = 2,
    TB_LINKER_SYMBOL_USED   = 4,
} TB_LinkerSymbolFlags;

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

    union {
        // for normal symbols
        struct {
            TB_LinkerSectionPiece* piece;
            uint32_t secrel;
        } normal;

        uint32_t absolute;
        uint32_t imagebase;

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
    TB_LinkerSectionPiece* piece;
} ExportTask;

typedef struct {
    TB_Linker* linker;
    TB_Slice obj_name;
    TB_Slice content;
} ImportObjTask;

// Format-specific vtable:
typedef struct TB_LinkerVtbl {
    void (*init)(TB_Linker* l);
    void (*append_object)(ImportObjTask* task);
    void (*append_library)(ImportObjTask* task);
    bool (*export)(TB_Linker* l, const char* file_name);
} TB_LinkerVtbl;

typedef struct TB_Linker {
    TB_Arch target_arch;
    Cuik_IThreadpool* thread_pool;

    const char* entrypoint;
    TB_WindowsSubsystem subsystem;

    TB_LinkerVtbl vtbl;

    NBHS symbols;
    NBHS sections;
    NBHS imports;

    NBHS unresolved_symbols;

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

    _Alignas(64) struct {
        Futex done;
        uint64_t count;
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
void tb_linker_export_piece(ExportTask* task);

// do layouting (requires GC step to complete)
DynArray(TB_LinkerSection*) tb__finalize_sections(TB_Linker* l);
