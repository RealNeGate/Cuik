ffi = require("ffi")
bit = require("bit")

local t = {}
local cuik_dll = ffi.load(ffi.os == "Windows" and "tb.dll" or "tb.so")

ffi.cdef[[
typedef struct TokenStream TokenStream;
typedef struct Cuik_CPP Cuik_CPP;
typedef struct Cuik_Linker Cuik_Linker;
typedef struct Cuik_Target Cuik_Target;
typedef struct Cuik_DriverArgs Cuik_DriverArgs;
typedef struct Cuik_Diagnostics Cuik_Diagnostics;

typedef struct TranslationUnit TranslationUnit;
typedef struct CompilationUnit CompilationUnit;

typedef struct String {
    size_t length;
    const unsigned char* data;
} String;

typedef struct {
    uint16_t length;
    char data[260];
} Cuik_Path;

// These are your options for arguments in diagnostics
typedef enum {
    DIAG_NOTE,
    DIAG_WARN,
    DIAG_ERR,
} DiagType;

typedef void (*Cuik_DiagCallback)(Cuik_Diagnostics* diag, void* userdata, DiagType type);

typedef enum Cuik_Version {
    // C language
    CUIK_VERSION_C89,
    CUIK_VERSION_C99,
    CUIK_VERSION_C11,
    CUIK_VERSION_C23,

    // GL shading language
    CUIK_VERSION_GLSL,
} Cuik_Version;

typedef enum TB_OutputFlavor {
    TB_FLAVOR_OBJECT,     // .o  .obj
    TB_FLAVOR_SHARED,     // .so .dll
    TB_FLAVOR_STATIC,     // .a  .lib
    TB_FLAVOR_EXECUTABLE, //     .exe
} TB_OutputFlavor;

typedef enum TB_WindowsSubsystem {
    TB_WIN_SUBSYSTEM_UNKNOWN,
    TB_WIN_SUBSYSTEM_WINDOWS,
    TB_WIN_SUBSYSTEM_CONSOLE,
    TB_WIN_SUBSYSTEM_EFI_APP,
} TB_WindowsSubsystem;

typedef struct Cuik_Toolchain Cuik_Toolchain;
struct Cuik_Toolchain {
    // we expect this to be heap allocated because cuik_toolchain_free
    void* ctx;
    bool case_insensitive;

    void* (*init)(void);
    void  (*add_libraries)(void* ctx, bool nocrt, Cuik_Linker* linker);
    void  (*set_preprocessor)(void* ctx, bool nocrt, Cuik_CPP* cpp);
    void  (*print_verbose)(void* ctx, bool nocrt);
    bool  (*invoke_link)(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename);
};

struct Cuik_DriverArgs {
    Cuik_Version version;

    TB_OutputFlavor flavor;

    Cuik_Target* target;
    Cuik_Toolchain toolchain;

    int threads;
    const char* output_name;
    const char* entrypoint;
    const char* dep_file;

    void* diag_userdata;
    Cuik_DiagCallback diag_callback;

    Cuik_Path** sources;
    Cuik_Path** includes;
    Cuik_Path** libraries;
    Cuik_Path** libpaths;
    char** defines;

    TB_WindowsSubsystem subsystem;

    bool emit_ir         : 1;
    bool assembly        : 1;
    bool ast             : 1;
    bool run             : 1;
    bool bake            : 1;
    bool nocrt           : 1;
    bool live            : 1;
    bool write_deps      : 1;
    bool optimize        : 1;
    bool time            : 1;
    bool verbose         : 1;
    bool syntax_only     : 1;
    bool test_preproc    : 1;
    bool debug_info      : 1;
    bool preprocess      : 1;
    bool think           : 1;
    bool based           : 1;
    bool nochkstk        : 1;
    bool preserve_ast    : 1;
};

Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_DriverArgs* args, bool should_finalize);
Cuik_CPP* cuik_driver_preprocess_str(String source, const Cuik_DriverArgs* args, bool should_finalize);

TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx);
void cuikpp_dump_tokens(TokenStream* s);

Cuik_Target* cuik_target_host(void);

typedef struct Cuik_ImportRequest {
    struct Cuik_ImportRequest* next;
    const char* lib_name;
} Cuik_ImportRequest;

typedef struct Cuik_ParseResult {
    int error_count;

    TranslationUnit* tu;         // if error_count == 0, then tu is a valid TU.
    Cuik_ImportRequest* imports; // linked list of imported libs.
} Cuik_ParseResult;

typedef enum TB_Arch {
    TB_ARCH_UNKNOWN,

    TB_ARCH_X86_64,
    TB_ARCH_AARCH64,

    // they're almost identical so might as well do both.
    TB_ARCH_MIPS32,
    TB_ARCH_MIPS64,

    TB_ARCH_WASM32,

    TB_ARCH_MAX,
} TB_Arch;

Cuik_ParseResult cuikparse_run(Cuik_Version version, TokenStream* s, Cuik_Target* target, bool only_code_index);
int cuiksema_run(TranslationUnit* tu);

typedef struct TB_Module TB_Module;
TB_Module* tb_module_create_for_host(bool is_jit);

void cuikcg_allocate_ir(TranslationUnit* tu, TB_Module* m, bool debug);

typedef struct CompilationUnit CompilationUnit;
CompilationUnit* cuik_create_compilation_unit(void);
void cuik_add_to_compilation_unit(CompilationUnit* cu, TranslationUnit* tu);

void cuik_init(bool use_crash_handler);
]]

local src = [[
int foo(int x, int y) { return x + y; }
]]

cuik_dll.cuik_init(false)

function compile(src)
    local str = ffi.new("String", { length=#src, data=ffi.string(src) })

    local args = ffi.new("Cuik_DriverArgs")
    args.target = cuik_dll.cuik_target_host()
    args.version = cuik_dll.CUIK_VERSION_C11

    local cpp = cuik_dll.cuik_driver_preprocess_str(str, args, true)
    local tokens = cuik_dll.cuikpp_get_token_stream(cpp)
    -- cuik_dll.cuikpp_dump_tokens(tokens)

    local parse = cuik_dll.cuikparse_run(args.version, tokens, args.target, false)
    print(parse.tu)

    if cuik_dll.cuiksema_run(parse.tu) > 0 then
        print("ERROR!")
        return
    end

    local cu = cuik_dll.cuik_create_compilation_unit()
    cuik_dll.cuik_add_to_compilation_unit(cu, parse.tu)

    local mod = cuik_dll.tb_module_create_for_host(true)
    cuik_dll.cuikcg_allocate_ir(parse.tu, mod, args.debug_info)

    print(mod)
end

compile(src)

print("Hello, World!", cuik_dll)

