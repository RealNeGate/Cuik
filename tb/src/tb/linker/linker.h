#pragma once
#include "../tb_internal.h"

#define NL_STRING_MAP_INLINE
#include <string_map.h>

// we use a linked list to store these because i couldn't be bothered to allocate
// one giant sequential region for the entire linker.
struct TB_LinkerSectionPiece {
    TB_LinkerSectionPiece* next;

    enum {
        // write the data buffer in this struct
        PIECE_NORMAL,
        // Write the TB module's text section
        PIECE_TEXT,
        // Write the TB module's data section
        PIECE_DATA,
        // Write the TB module's rdata section
        PIECE_RDATA,
        // Write the TB module's pdata section
        PIECE_PDATA,
        // Write the TB module's reloc section
        PIECE_RELOC
    } kind;

    TB_Module* module;
    // vsize is the virtual size
    size_t offset, vsize, size;
    const uint8_t* data;
};

struct TB_LinkerSection {
    NL_Slice name;

    // probably memory characteristics like EXECUTE (output format specific)
    uint32_t flags;

    size_t address; // usually a relative virtual address.
    size_t offset;  // in the file.

    size_t total_size;
    TB_LinkerSectionPiece *first, *last;
};

typedef enum TB_LinkerSymbolTag {
    // external linkage
    TB_LINKER_SYMBOL_NORMAL,

    // TB defined
    TB_LINKER_SYMBOL_TB,

    // local to the translation unit
    TB_LINKER_SYMBOL_STATIC,

    // imported from shared object
    TB_LINKER_SYMBOL_IMPORT,
} TB_LinkerSymbolTag;

// all symbols appended to the linker are converted into
// these and used for all kinds of relocation resolution.
typedef struct TB_LinkerSymbol {
    // key
    TB_Slice name;

    // value
    TB_LinkerSymbolTag tag;
    union {
        // for normal symbols
        uint32_t rva;

        // for IR module symbols
        TB_Symbol* sym;

        // for imports, refers to the imports array in TB_Linker
        struct {
            uint32_t id;
            uint16_t ordinal;
        } import;
    };
} TB_LinkerSymbol;

// MSI hash table
typedef struct TB_SymbolTable {
    size_t exp, len;
    TB_LinkerSymbol* ht; // [1 << exp]
} TB_SymbolTable;

typedef struct {
    TB_Slice name;
    // this is the location the thunk will call
    uint32_t ds_address;
    // this is the ID of the thunk
    uint32_t thunk_id;
    uint16_t ordinal;
} ImportThunk;

typedef struct {
    TB_Slice libpath;
    DynArray(ImportThunk) thunks;

    uint64_t *iat, *ilt;
} ImportTable;

// Format-specific vtable:
typedef struct TB_LinkerVtbl {
    void(*append_object)(TB_Linker* l, TB_ObjectFile* obj);
    void(*append_library)(TB_Linker* l, TB_Slice ar_file);
    void(*append_module)(TB_Linker* l, TB_Module* m);
    TB_Exports(*export)(TB_Linker* l);
} TB_LinkerVtbl;

typedef struct TB_Linker {
    TB_Arch target_arch;

    ptrdiff_t entrypoint; // -1 if not available
    NL_Strmap(TB_LinkerSection*) sections;

    // for relocations
    DynArray(TB_Module*) ir_modules;
    TB_SymbolTable symtab;

    size_t trampoline_pos;  // relative to the .text section
    TB_Emitter trampolines; // these are for calling imported functions

    // Windows specific:
    //   on windows, we use DLLs to interact with the OS so
    //   there needs to be a way to load these immediately,
    //   imports do just that.
    DynArray(ImportTable) imports;

    TB_LinkerVtbl vtbl;
} TB_Linker;

// TB helpers
size_t tb__layout_text_section(TB_Module* m, ptrdiff_t* restrict entrypoint, const char* entrypoint_name);

// Symbol table
TB_LinkerSymbol* tb__find_symbol(TB_SymbolTable* restrict symtab, TB_Slice name);
bool tb__append_symbol(TB_SymbolTable* restrict symtab, const TB_LinkerSymbol* sym);
uint64_t tb__compute_rva(TB_Linker* l, TB_Module* m, const TB_Symbol* s);

// Section management
TB_LinkerSection* tb__find_section(TB_Linker* linker, const char* name, uint32_t flags);
TB_LinkerSection* tb__find_or_create_section(TB_Linker* linker, const char* name, uint32_t flags);
TB_LinkerSection* tb__find_or_create_section2(TB_Linker* linker, size_t name_len, const uint8_t* name_str, uint32_t flags);
TB_LinkerSectionPiece* tb__append_piece(TB_LinkerSection* section, int kind, size_t size, const void* data, TB_Module* mod);

size_t tb__pad_file(uint8_t* output, size_t write_pos, char pad, size_t align);
void tb__apply_external_relocs(TB_Linker* l, TB_Module* m, uint8_t* output);
size_t tb__apply_section_contents(TB_Linker* l, uint8_t* output, size_t write_pos, TB_LinkerSection* text, TB_LinkerSection* data, TB_LinkerSection* rdata, size_t section_alignment);
