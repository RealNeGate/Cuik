buffer  = require "string.buffer"
inspect = require "meta/inspect"
ffi = require "ffi"
bit = require "bit"

local t = {}
local cuik_dll = ffi.load(ffi.os == "Windows" and "cuik.dll" or "bin/libcuik.so")

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
void cuikdg_dump_to_file(TokenStream* tokens, void* out);

typedef struct TB_Module TB_Module;
TB_Module* tb_module_create_for_host(bool is_jit);

void cuikcg_allocate_ir(TranslationUnit* tu, TB_Module* m, bool debug);

typedef struct CompilationUnit CompilationUnit;
CompilationUnit* cuik_create_compilation_unit(void);
void cuik_add_to_compilation_unit(CompilationUnit* cu, TranslationUnit* tu);

void cuik_init(bool use_crash_handler);

typedef struct Stmt Stmt;
typedef struct TB_Symbol TB_Symbol;
Stmt** cuik_get_top_level_stmts(TranslationUnit* tu);
size_t cuik_num_of_top_level_stmts(TranslationUnit* tu);
TB_Symbol* cuikcg_top_level(TranslationUnit* tu, TB_Module* m, Stmt* s);
const char* cuik_stmt_decl_name(Stmt* stmt);

typedef struct TB_Worklist TB_Worklist;
TB_Worklist* tb_worklist_alloc(void);
void tb_worklist_free(TB_Worklist* ws);

typedef struct TB_ArenaChunk TB_ArenaChunk;
typedef struct {
    TB_ArenaChunk* top;

    const char* tag;
    uint32_t allocs;
    uint32_t alloc_bytes;
} TB_Arena;

typedef struct TB_Function TB_Function;
typedef struct TB_FunctionOutput TB_FunctionOutput;
bool tb_opt(TB_Function* f, TB_Worklist* ws, bool preserve_types);
TB_FunctionOutput* tb_codegen(TB_Function* f, int ra, TB_Worklist* ws, TB_Arena* code_arena, bool emit_asm);
TB_Function* tb_symbol_as_function(TB_Symbol* s);
void tb_output_print_asm(TB_FunctionOutput* out, void* fp);

typedef struct TB_JIT TB_JIT;
TB_JIT* tb_jit_begin(TB_Module* m, size_t jit_heap_capacity);
void* tb_jit_place_function(TB_JIT* jit, TB_Function* f);
void tb_jit_end(TB_JIT* jit);
]]

cuik_dll.cuik_init(false)

function compile(src, optimize, decl_mappings)
    local str = ffi.new("String", { length=#src, data=ffi.string(src) })

    local args = ffi.new("Cuik_DriverArgs")
    args.target = cuik_dll.cuik_target_host()
    args.version = cuik_dll.CUIK_VERSION_C11

    local cpp = cuik_dll.cuik_driver_preprocess_str(str, args, true)
    local tokens = cuik_dll.cuikpp_get_token_stream(cpp)
    -- cuik_dll.cuikpp_dump_tokens(tokens)

    local parse = cuik_dll.cuikparse_run(args.version, tokens, args.target, false)
    if parse.tu == nil then
        cuik_dll.cuikdg_dump_to_file(tokens, nil)
        return
    end

    local r = cuik_dll.cuiksema_run(parse.tu)
    cuik_dll.cuikdg_dump_to_file(tokens, nil)
    if r > 0 then
        return
    end

    local cu = cuik_dll.cuik_create_compilation_unit()
    cuik_dll.cuik_add_to_compilation_unit(cu, parse.tu)

    local mod = cuik_dll.tb_module_create_for_host(true)
    cuik_dll.cuikcg_allocate_ir(parse.tu, mod, args.debug_info)

    local stmt_count = tonumber(ffi.cast("uint32_t", cuik_dll.cuik_num_of_top_level_stmts(parse.tu)))
    local stmts = cuik_dll.cuik_get_top_level_stmts(parse.tu)

    local symbols = {}
    local ir_worklist = cuik_dll.tb_worklist_alloc()
    for i=0,stmt_count-1 do
        local sym = cuik_dll.cuikcg_top_level(parse.tu, mod, stmts[i])
        if cuik_dll.tb_symbol_as_function(sym) ~= nil then
            local f = ffi.cast("TB_Function*", sym)
            if optimize then
                cuik_dll.tb_opt(f, ir_worklist, false)
            end

            local out = cuik_dll.tb_codegen(f, 0, ir_worklist, nil, true)
            cuik_dll.tb_output_print_asm(out, nil)

            symbols[i] = sym
        end
    end
    cuik_dll.tb_worklist_free(ir_worklist)

    -- export JITted functions
    local result = {}
    local jit = cuik_dll.tb_jit_begin(mod, 0)
    for i=0,stmt_count-1 do
        local n = cuik_dll.cuik_stmt_decl_name(stmts[i])

        if symbols[i] then
            local name = ffi.string(n)
            local decl_type = decl_mappings[name]
            if decl_type then
                ffi.cdef(decl_type)

                local addr = cuik_dll.tb_jit_place_function(jit, ffi.cast("TB_Function*", symbols[i]))
                result[name] = ffi.cast("FN_"..name.."*", addr)
            end
        end
    end

    return result
end

function LineBuffer()
    local t = { type="LineBuffer", indent=0, indent_str="", lines={} }

    function t:indent()
        self.indent = self.indent + 1
        self.indent_str = string.rep("    ", self.indent)
    end

    function t:undent()
        self.indent = self.indent - 1
        self.indent_str = string.rep("    ", self.indent)
    end

    function t:empty()
        self.lines[#self.lines + 1] = ""
    end

    function t:puts(str)
        self.lines[#self.lines + 1] = self.indent_str..str
    end

    function t:putf(fmt, ...)
        self.lines[#self.lines + 1] = self.indent_str..string.format(fmt, ...)
    end

    function t:to_str()
        return table.concat(self.lines, "\n")
    end
    return t
end

C_ELEM_TYPES = {
    ["i8"]  = "int8_t",
    ["i16"] = "int16_t",
    ["i32"] = "int32_t",
    ["i64"] = "int64_t",
    ["f32"] = "float",
    ["f64"] = "double",
}

-- description of matrix multiply op
function Op_Matmul(dt, N)
    local t = { dt=dt, N=N, block_size=4 }

    -- unique string for the matmul's properties
    function t:key()
        return string.format("NNN_matmul_%d_%d", self.N, self.block_size)
    end

    -- generate bare function
    function t:compile(L)
        assert(L.type == "LineBuffer")

        local key = self:key()
        local c_elem = C_ELEM_TYPES[self.dt]

        L:putf("void %s(Dope* dst, Dope* a, Dope* b) {", key)
        L:indent()
        do
            L:putf("%s* a_arr = (%s*) a->data;", c_elem, c_elem)
            L:putf("%s* b_arr = (%s*) b->data;", c_elem, c_elem)
            L:putf("%s* restrict dst_arr = (%s*) dst->data;", c_elem, c_elem)
            L:empty()

            L:indent()
            L:undent()

            L:puts("}")
        end
        L:undent()
        L:puts("}")
    end

    return t
end

local L = LineBuffer()

-- Introduce prelude
L:puts("#include <stdint.h>")
L:empty()
L:puts("typedef struct Dope {")
L:puts("    int offset;")
L:puts("    int limit;")
L:puts("    char* data;")
L:puts("} Dope;")
L:empty()

local mmm = Op_Matmul("f32", 300)
mmm:compile(L)

print(L:to_str())
os.exit()

function matmul_blocked(N)
    local src = buffer.new(1000)
    src:put("#include <stddef.h>\n")
    src:put("void matmul(float* dst, float* a, float* b) {\n")

    if false then
        for i=0,(N*N) - 1 do
            src:put(string.format("    float a%d = a[%d];\n", i, i))
            src:put(string.format("    float b%d = b[%d];\n", i, i))
        end
        for i=0,N-1 do
            for j=0,N-1 do
                src:put(string.format("    float sum%d_%d = -0.0f;\n", i, j))
                for k=0,N-1 do
                    src:put(string.format("    sum%d_%d += a%d * b%d;\n", i, j, i*N + k, k*N + j))
                end
                src:put(string.format("    dst[%d] = sum%d_%d;\n", i*N + j, i, j))
            end
        end
        src:put("}\n")
    end

    local block_size = 4
    -- zeroing loop (slightly unrolled)
    local elem_count = N * N
    local zero_unroll = 16
    src:put(string.format("    for (size_t i = 0; i < %d; i++) {\n", elem_count / zero_unroll))
    for i=0,zero_unroll-1 do
        src:put(string.format("        dst[i*%d + %d] = -0.0f;\n", zero_unroll, i))
    end
    src:put("    }\n")
    -- blocked matrix multiply
    src:put("\n")
    src:put(string.format("    for (size_t kk = 0; kk < %d; kk += %d) {\n", N, block_size))
    src:put(string.format("        for (size_t jj = 0; jj < %d; jj += %d) {\n", N, block_size))
    src:put(string.format("            for (size_t i = 0; i < %d; i++) {\n", N))
    -- load all items into regs before any stores to avoid aliasing issues
    for k=0,block_size-1 do
        src:put(string.format("                float a%d = a[i*%d + (kk+%d)];\n", k, N, k))
    end
    for j=0,block_size-1 do
        for k=0,block_size-1 do
            src:put(string.format("                float b%d = b[(kk+%d)*%d + (jj+%d)];\n", j*block_size + k, k, N, j))
        end
    end
    -- load destination block
    for j=0,block_size-1 do
        src:put(string.format("                float sum%d = dst[i*%d + (jj+%d)];\n", j, N, j))
    end
    -- fully unroll the block multiply
    for j=0,block_size-1 do
        for k=0,block_size-1 do
            src:put(string.format("                sum%d += a%d * b%d;\n", j, k, j*block_size + k))
        end
        src:put(string.format("                dst[i*%d + (jj+%d)] = sum%d;\n", N, j, j))
    end
    src:put("            }\n")
    src:put("        }\n")
    src:put("    }\n")
    src:put("}\n")

    return src:tostring()
end

function sample(N, dst, a, b)
    for i=0,N-1 do
        for j=0,N-1 do
            local sum = 0
            for k=0,N-1 do
                sum = sum + a[i*N + k]*b[k*N + j]
            end
            dst[i*N + j] = sum
        end
    end
end

math.randomseed(os.time())
function rand_mat(N)
    local m = ffi.new("float[?]", N*N)
    for i=0,(N*N)-1 do
        m[i] = math.random()
    end
    return m
end

function print_mat(N, mat)
    local str = buffer.new(1000)
    for i=0,N-1 do
        for j=0,N-1 do
            str:put(string.format("%f ", mat[i*N + j]))
        end
        str:put("\n")
    end
    print(str:tostring())
end

local N = 100

local src = matmul_blocked(N)
print(src)

local funcs = compile(src, true, {
    matmul="typedef int FN_matmul(float* dst, float* a, float* b);",
})

local aa = rand_mat(N)
local bb = rand_mat(N)
local dst = ffi.new("float[?]", N*N)

if true then
    funcs.matmul(dst, aa, bb)
    print_mat(N, dst)

    sample(N, dst, aa, bb)
    print_mat(N, dst)
end

local tries = 100
local x = os.clock()
for i=1,tries do
    funcs.matmul(dst, aa, bb)
end
local y = ((os.clock() - x) * 1000000.0) / tries
print(string.format("cool Cuik impl: %.2f us/op\n", y))

x = os.clock()
for i=1,tries do
    sample(N, dst, aa, bb)
end
local y = ((os.clock() - x) * 1000000.0) / tries
print(string.format("naive LJ impl: %.2f us/op\n", y))

