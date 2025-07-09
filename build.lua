--
--
--   _____       _ _     _                 _
--  | ___ \     (_) |   | |               | |
--  | |_/ /_   _ _| | __| |  ___ _   _ ___| |_ ___ _ __ ___
--  | ___ \ | | | | |/ _` | / __| | | / __| __/ _ \ '_ ` _ \
--  | |_/ / |_| | | | (_| | \__ \ |_| \__ \ ||  __/ | | | | |
--  \____/ \__,_|_|_|\__,_| |___/\__, |___/\__\___|_| |_| |_|
--                                __/ |
--                               |___/
--
--

-- silly little OS detection
local is_windows = package.config:sub(1,1) == "\\"

local options = {
    debug         = false,
    cuik          = false,
    tb            = false,
    driver        = false,
    shared        = false,
    lld           = false,
    gcc           = false,
    asan          = false,
    spall_auto    = false
}

-- Cuik/TB are broken down into several pieces
local modules = {
    common = { srcs={"common/common.c", "common/perf.c", "common/emitter.c"} },

    -- libraries:
    --   C preprocessor
    cuik_pp = { srcs={
            "cuik_pp/lexer.c", "cuik_pp/cpp.c", "cuik_pp/diagnostic.c"
        }, flags="-DCONFIG_HAS_CUIKPP", deps={"common"}
    },
    --   C frontend
    cuik_c = { srcs={
            "cuik_c/libcuik.c",
            -- toolchain support
            "cuik_c/toolchains/msvc.c", "cuik_c/toolchains/gnu.c", "cuik_c/toolchains/darwin.c",
            -- architectures
            "cuik_c/targets/x64_desc.c",  "cuik_c/targets/aarch64_desc.c",
            "cuik_c/targets/mips_desc.c", "cuik_c/targets/wasm_desc.c",
        }, flags="-DCONFIG_HAS_CUIKC", deps={"common", "cuik_pp"}
    },
    --   TildeBackend
    tb = { srcs={
            "tb/libtb.c",
            -- archictectures
            "tb/x64/x64_target.c", "tb/aarch64/aarch64_target.c", "tb/mips/mips_target.c", "tb/wasm/wasm_target.c"
        }, flags="-DCONFIG_HAS_TB", deps={"common", "cuik_pp"}
    },
    --   Linker
    linker = { srcs={
            "linker/linker.c",
        }, flags="-DCONFIG_HAS_LINKER", deps={"common", "cuik_pp"}
    },
    -- executables:
    --   Cuik command line
    driver = { is_exe=true, srcs={"main/main_driver.c"}, deps={"common", "cuik_pp", "cuik_c", "tb", "linker"} },

    -- external dependencies
    mimalloc = { srcs={"mimalloc/src/static.c"}, flags="-DCONFIG_HAS_MIMALLOC" }
}

local ldflags = ""
local cflags = " -g -march=haswell -I include -I common -Wall -Werror -Wno-unused -Wno-microsoft-enum-forward-reference -Wno-deprecated -DMI_SKIP_COLLECT_ON_EXIT -DCUIK_ALLOW_THREADS -I mimalloc/include"

local supported_archs = {
    x64 = "-DTB_HAS_X64",
    a64 = "-DTB_HAS_AARCH64",
}

local in_use_archs = {}

-- command lines
local has_arch = false
for i = 1, #arg do
    if arg[i]:sub(1, 1) == "-" then
        local a = arg[i]:sub(2)
        if supported_archs[a] then
            cflags = cflags.." "..supported_archs[a]
            in_use_archs[a] = true
            has_arch = true
        else
            options[a] = true
        end
    end
end

if not has_arch then
    print("Listen brosef, you gotta pass me an arch (or archs) to compile:")
    for k,v in pairs(supported_archs) do
        print(k)
    end
    os.exit(0)
end

if options.asan then
    cflags = cflags.." -fsanitize=address"
end

if options.driver then
    cflags = cflags.." -DCUIK_USE_CUIK"
end

if options.gcc then
    cflags = cflags.." -Wno-enum-compare -Wno-array-bounds -Wno-unknown-pragmas"
end

local cc = options.gcc and "gcc" or "clang"
local ar = options.gcc and "ar"  or "llvm-ar"

if not options.debug then
    cflags = cflags.." -O2 -DNDEBUG"
    if not options.gcc then
        -- options.lld = true
        -- cflags = cflags.." -flto"
    end
end

if options.spall_auto then
    cflags = cflags.." -DCUIK_USE_SPALL_AUTO -finstrument-functions-after-inlining"
end

local src = {}

if is_windows then
    cflags = cflags.." -D_CRT_SECURE_NO_WARNINGS"

    if options.shared then
        cflags = cflags.." -DCUIK_DLL -DTB_DLL"
        ldflags = ldflags.." /dll"
    end

    if options.asan then
        ld = "clang"
        ldflags = ldflags.." -fsanitize=address -g -o "
    else
        if options.lld then
            ld = "lld-link"
        else
            ld = "link"
        end

        ldflags = ldflags.." /stack:4194304 /nologo /incremental:no /debug onecore.lib msvcrt.lib libcmt.lib"
        ldflags = ldflags.." /defaultlib:libcmt /out:"
    end

    exe_ext = ".exe"
    dll_ext = ".dll"
    lib_ext = ".lib"
else
    ld = cc
    cflags = cflags.." -D_GNU_SOURCE"
    ldflags = ldflags.." -g -lc -lm "

    if options.lld then
        ldflags = ldflags.." -fuse-ld=lld"
    end

    if options.shared then
        cflags = cflags.." -fPIC"
        ldflags = ldflags.." -shared"
    end

    ldflags = ldflags.." -g -o "
    exe_ext = ""
    dll_ext = ".so"
    lib_ext = ".a"
end

local is_exe = false
local added = {}

-- resolve dependencies
function walk(name)
    if added[name] ~= nil then
        return
    end
    added[name] = true

    if options.shared and modules[name].is_exe then
        print("error: "..name.." is an executable, it cannot be compiled with -shared")
        exit(1)
    end

    -- print("Building "..name)

    if modules[name].is_exe then
        is_exe = true
    end

    if modules[name].flags then
        cflags = cflags.." "..modules[name].flags
    end

    for i,v in ipairs(modules[name].srcs) do
        src[#src + 1] = v
    end

    local deps = modules[name].deps
    if deps then
        for i,v in ipairs(deps) do walk(v) end
    end
end

walk("mimalloc")

if options.cuik then
    walk("cuik_c")
end

-- whatever the options says to compile, do that
for k,v in pairs(options) do
    if v and modules[k] then walk(k) end
end

-- generate ninja files
ninja = io.open("build.ninja", "wb")

function rule(name, content)
    ninja:write("rule "..name.."\n")
    for k,v in pairs(content) do
        ninja:write("  "..k.." = "..v.."\n")
    end
end

function command(out, input, cmd, extra_input)
    ninja:write("build "..out..": run "..input)
    if extra_input ~= nil then
        ninja:write("| "..extra_input.."\n")
    else
        ninja:write("\n")
    end
    ninja:write("  cmd = "..cmd:gsub("$in", input):gsub("$out", out).."\n")
end

function filename(file)
    return file:match("^.+/(.+)%..+")
end

ninja:write("cflags = "..cflags.."\n")
ninja:write("ldflags = "..ldflags.."\n")

rule("cc", {
    depfile = "$out.d",
    command = cc.." $in $cflags -MD -MF $out.d -c -o $out",
    description = "CC $out"
})
rule("link", {
    command = ld.." $in $ldflags$out",
    description = "LINK $out"
})
rule("lib", {
    depfile = "$out.d",
    command = ar.." -rcs $out $in",
    description = "LIB $out"
})
rule("run", {
    command = "$cmd",
    description = "$cmd"
})

-- TB's DSL
if added["tb"] then
    if in_use_archs["x64"] then
        command("tb/x64/x64_gen.h", "meta/dsl.lua", arg[-1].." $in x86 tb/x64/x64.machine tb/x64/x64_gen.h", "tb/x64/x64.machine")
    end

    if in_use_archs["a64"] then
        command("tb/aarch64/a64_gen.h", "meta/dsl.lua", arg[-1].." $in a64 tb/aarch64/a64.machine tb/aarch64/a64_gen.h", "tb/aarch64/a64.machine")
    end
end

-- lexer metaprogram
command("bin/objs/lexgen"..exe_ext, "meta/lexgen.c", cc.." $in -O1 -o $out")
command("cuik_pp/keywords.h cuik_pp/dfa.h", "bin/objs/lexgen"..exe_ext, "bin/objs/lexgen"..exe_ext)

-- package freestanding headers into C file
local x = {}
if is_windows then
    local cmd = io.popen("dir /B freestanding\\*.h")
    for c in cmd:lines() do
        x[#x + 1] = "freestanding/"..c
    end
    cmd:close()
else
    local cmd = io.popen("find freestanding/*.h -maxdepth 1")
    for c in cmd:lines() do
        x[#x + 1] = c
    end
    cmd:close()
end
local freestanding_headers = table.concat(x, " ")

command("bin/objs/hexembed"..exe_ext, "meta/hexembed.c", cc.." $in -O1 -o $out")
command("bin/objs/freestanding.c", freestanding_headers, "bin/objs/hexembed"..exe_ext.." $in", "bin/objs/hexembed"..exe_ext)

src[#src + 1] = "bin/objs/freestanding.c"

-- normal C files
local objs = {}
for i, f in ipairs(src) do
    local out = "bin/objs/"..filename(f)..".o"
    ninja:write("build "..out..": cc "..f)
    if added["cuik_pp"] then
        ninja:write(" | cuik_pp/keywords.h cuik_pp/dfa.h")
    end
    ninja:write("\n")
    objs[#objs + 1] = out
end

local obj_names = table.concat(objs, " ")

local exe_name = "cuik"
if not added["cuik_c"] then exe_name = "tb" end

if not is_windows and not is_exe then
    exe_name = "lib"..exe_name
end

-- placing executables into bin/
exe_name = "bin/"..exe_name

-- link or archive
if options.shared then
    ninja:write([[build ]]..exe_name..dll_ext..[[: link ]]..obj_names.."\n")
elseif is_exe then
    ninja:write([[build ]]..exe_name..exe_ext..[[: link ]]..obj_names.."\n")
else
    ninja:write([[build ]]..exe_name..lib_ext..[[: lib ]]..obj_names.."\n")
end

ninja:close()

local _0, _1, res = os.execute("ninja")
if res ~= 0 then os.exit(res) end
