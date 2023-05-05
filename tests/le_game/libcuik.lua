local t = {}

local ffi = require("ffi")
local cuik_dll = ffi.load(ffi.os == "Windows" and "cuik.dll" or "cuik.so")

ffi.cdef[[
typedef struct Cuik_CPP Cuik_CPP;
typedef struct Cuik_Parser Cuik_Parser;
typedef struct Cuik_Linker Cuik_Linker;
typedef struct Cuik_Target Cuik_Target;
typedef struct Cuik_DriverArgs Cuik_DriverArgs;
typedef struct Cuik_Diagnostics Cuik_Diagnostics;
typedef struct Cuik_IThreadpool Cuik_IThreadpool;

typedef struct TB_Module TB_Module;
typedef struct TB_Symbol TB_Symbol;
typedef struct TB_Function TB_Function;
typedef struct TB_JITContext TB_JITContext;

typedef struct Stmt Stmt;

typedef enum Cuik_ParseVersion {
    // C language
    CUIK_VERSION_C89,
    CUIK_VERSION_C99,
    CUIK_VERSION_C11,
    CUIK_VERSION_C23,

    // GL shading language
    CUIK_VERSION_GLSL,
} Cuik_ParseVersion;

typedef enum TB_OutputFlavor {
    TB_FLAVOR_OBJECT,     // .o  .obj
    TB_FLAVOR_ASSEMBLY,   // .s  .asm
    TB_FLAVOR_SHARED,     // .so .dll
    TB_FLAVOR_STATIC,     // .a  .lib
    TB_FLAVOR_EXECUTABLE, //     .exe
} TB_OutputFlavor;

typedef struct Cuik_Toolchain {
    // we expect this to be heap allocated because cuik_toolchain_free
    void* ctx;

    void(*add_libraries)(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker);
    void(*set_preprocessor)(void* ctx, const Cuik_DriverArgs* args, Cuik_CPP* cpp);

    bool(*invoke_link)(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename);
} Cuik_Toolchain;

struct Cuik_DriverArgs {
    Cuik_ParseVersion version;
    TB_OutputFlavor flavor;

    Cuik_Target* target;
    Cuik_Toolchain toolchain;

    int threads, opt_level;
    const char* output_name;
    const char* entrypoint;

    void* diag_userdata;
    void* diag_callback;

    char** sources;
    char** includes;
    char** libraries;
    char** defines;

    int subsystem;

    bool ir              : 1;
    bool emit_ir         : 1;
    bool ast             : 1;
    bool types           : 1;
    bool run             : 1;
    bool bake            : 1;
    bool nocrt           : 1;
    bool live            : 1;
    bool time            : 1;
    bool verbose         : 1;
    bool syntax_only     : 1;
    bool test_preproc    : 1;
    bool debug_info      : 1;
    bool preprocess      : 1;
    bool think           : 1;
    bool based           : 1;
};

typedef struct String {
    size_t length;
    const unsigned char* data;
} String;

typedef struct SourceLoc {
    uint32_t raw;
} SourceLoc;

typedef struct SourceRange {
    SourceLoc start, end;
} SourceRange;

// This is what FileIDs refer to
typedef struct {
    const char* filename;
    bool is_system;

    int depth;
    SourceLoc include_site;
    // describes how far from the start of the file we are.
    // used by line_map on big files
    uint32_t file_pos_bias;

    // NOTE: this is the size of this file chunk, big files consist
    // of multiple chunks so you should use...
    //
    // TODO(NeGate): make function for doing this
    uint32_t content_length;
    const char* content;

    // a DynArray(uint32_t) sorted to make it possible to binary search
    //   [line] = file_pos
    uint32_t* line_map;
} Cuik_File;

typedef struct Token {
    // it's a TknType but GCC doesn't like incomplete enums
    int type     : 30;
    int expanded : 1;
    int hit_line : 1;

    SourceLoc location;
    String content;
} Token;

// This is what MacroIDs refer to
typedef struct MacroInvoke {
    String name;

    // 0 means it's got no parent
    uint32_t parent;

    SourceRange def_site;
    SourceLoc call_site;
} MacroInvoke;

typedef struct TokenArray {
    // DynArray(Token)
    struct Token* tokens;
    size_t current;
} TokenArray;

typedef struct TokenStream {
    const char* filepath;
    TokenArray list;

    Cuik_Diagnostics* diag;

    // if true, the preprocessor is allowed to delete after completion.
    // this shouldn't enabled when caching files
    bool is_owned;

    // DynArray(MacroInvoke)
    MacroInvoke* invokes;

    // DynArray(Cuik_File)
    Cuik_File* files;
} TokenStream;

// This is generated from
//    #pragma comment(lib, "somelib.lib")
typedef struct Cuik_ImportRequest {
    struct Cuik_ImportRequest* next;
    const char* lib_name;
} Cuik_ImportRequest;

typedef struct TranslationUnit TranslationUnit;

typedef struct Cuik_ParseResult {
    int error_count;

    TranslationUnit* tu;         // if error_count == 0, then tu is a valid TU.
    Cuik_ImportRequest* imports; // linked list of imported libs.
} Cuik_ParseResult;

typedef enum TB_FeatureSet_X64 {
    TB_FEATURE_X64_SSE3   = (1u << 0u),
    TB_FEATURE_X64_SSE41  = (1u << 1u),
    TB_FEATURE_X64_SSE42  = (1u << 2u),

    TB_FEATURE_X64_POPCNT = (1u << 3u),
    TB_FEATURE_X64_LZCNT  = (1u << 4u),

    TB_FEATURE_X64_CLMUL  = (1u << 5u),
    TB_FEATURE_X64_F16C   = (1u << 6u),

    TB_FEATURE_X64_BMI1   = (1u << 7u),
    TB_FEATURE_X64_BMI2   = (1u << 8u),

    TB_FEATURE_X64_AVX    = (1u << 9u),
    TB_FEATURE_X64_AVX2   = (1u << 10u),
} TB_FeatureSet_X64;

typedef struct TB_FeatureSet {
    TB_FeatureSet_X64 x64;
} TB_FeatureSet;

typedef enum TB_ISelMode {
    // FastISel
    TB_ISEL_FAST,
    TB_ISEL_COMPLEX
} TB_ISelMode;

// driver
Cuik_Target* cuik_target_host(void);
Cuik_Toolchain cuik_toolchain_host(void);

// preprocessor
Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_DriverArgs* args, bool should_finalize);
Cuik_CPP* cuik_driver_preprocess_cstr(const char* source, const Cuik_DriverArgs* args, bool should_finalize);
TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx);
void cuiklex_free_tokens(TokenStream* tokens);
void cuikdg_dump_to_stderr(TokenStream* tokens);

// parser
Cuik_ParseResult cuikparse_run(Cuik_ParseVersion version, TokenStream* restrict s, Cuik_Target* target, bool only_code_index);
int cuiksema_run(TranslationUnit* tu, Cuik_IThreadpool* thread_pool);

Stmt** cuik_get_top_level_stmts(TranslationUnit* tu);
size_t cuik_num_of_top_level_stmts(TranslationUnit* tu);
const char* cuik_stmt_decl_name(Stmt* stmt);

// irgen
void cuikcg_allocate_ir2(TranslationUnit* tu, TB_Module* m);
TB_Symbol* cuikcg_top_level(TranslationUnit* tu, TB_Module* m, Stmt* restrict s);

// tb
TB_Module* tb_module_create_for_host(const TB_FeatureSet* features, bool is_jit);
bool tb_module_compile_function(TB_Module* m, TB_Function* f, TB_ISelMode isel_mode);

// passing 0 to jit_heap_capacity will default to 4MiB
TB_JITContext* tb_module_begin_jit(TB_Module* m, size_t jit_heap_capacity);
void* tb_module_apply_function(TB_JITContext* jit, TB_Function* f);

// fixes page permissions, applies missing relocations
void tb_module_ready_jit(TB_JITContext* jit);
void tb_module_end_jit(TB_JITContext* jit);
]]

t.args = ffi.new("Cuik_DriverArgs", {
	version = cuik_dll.CUIK_VERSION_C23,
	target = cuik_dll.cuik_target_host(),
	toolchain = cuik_dll.cuik_toolchain_host(),
	flavor = cuik_dll.TB_FLAVOR_EXECUTABLE,
})

-- make TB module
local features = ffi.new("TB_FeatureSet", {})
tb_module = cuik_dll.tb_module_create_for_host(features, true)

local function compile_internal(cpp)
	if not cpp then
		print("Yikes... preprocessing failed!")
		return nil
	end

	-- parse into AST
	local tokens = cuik_dll.cuikpp_get_token_stream(cpp)
	local parse_result = cuik_dll.cuikparse_run(t.args.version, tokens, t.args.target, false)
	if parse_result.error_count > 0 then
		cuik_dll.cuikdg_dump_to_stderr(tokens)
		print("Yikes... parsing failed!")
		return nil
	end

	local tu = parse_result.tu
	if cuik_dll.cuiksema_run(tu, nil) > 0 then
	    cuik_dll.cuikdg_dump_to_stderr(tokens)
		print("Yikes... type checking failed!")
		return nil
	end

	-- IR gen & compile
	cuik_dll.cuikcg_allocate_ir2(tu, tb_module)

	local stmt_count = tonumber(ffi.cast("uint32_t", cuik_dll.cuik_num_of_top_level_stmts(tu)))
	local stmts = cuik_dll.cuik_get_top_level_stmts(tu)

	local symbols = {}
	for i=0,stmt_count-1 do
		local name = ffi.string(cuik_dll.cuik_stmt_decl_name(stmts[i]))
		local sym = cuik_dll.cuikcg_top_level(tu, tb_module, stmts[i])

		if sym ~= nil then
			cuik_dll.tb_module_compile_function(tb_module, ffi.cast("TB_Function*", sym), cuik_dll.TB_ISEL_FAST)
			symbols[name] = sym
		end
	end

	-- export JITted functions
	local result = {}
	local jit = cuik_dll.tb_module_begin_jit(mod, 0)
	for k,v in pairs(symbols) do
		local addr = cuik_dll.tb_module_apply_function(jit, ffi.cast("TB_Function*", v))
		result[k] = ffi.cast("int (*)()", addr)
	end

	cuik_dll.tb_module_ready_jit(jit)

	return result
end

function t.compile(source)
	return compile_internal(cuik_dll.cuik_driver_preprocess_cstr(source, t.args, true))
end

function t.compile_file(path)
	return compile_internal(cuik_dll.cuik_driver_preprocess(path, t.args, true))
end

return t
