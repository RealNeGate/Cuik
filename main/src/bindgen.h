#include <cuik.h>
#include <cuik_ast.h>
#include "stb_ds.h"
#include "dyn_array.h"

typedef struct {
    Cuik_Type* key;
    int value;
} DefinedTypeEntry;

typedef struct {
    void(*file)(const char* filepath);
    void(*decl)(TranslationUnit* tu, DefinedTypeEntry** defined_types, Stmt* s);
    void(*include)(const char* filepath);
    void(*define)(Cuik_DefineIter def);
} Bindgen;

extern Bindgen bindgen__c99;
extern Bindgen bindgen__odin;
