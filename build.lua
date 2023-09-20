--[[

  _____       _ _     _                 _
 | ___ \     (_) |   | |               | |
 | |_/ /_   _ _| | __| |  ___ _   _ ___| |_ ___ _ __ ___
 | ___ \ | | | | |/ _` | / __| | | / __| __/ _ \ '_ ` _ \
 | |_/ / |_| | | | (_| | \__ \ |_| \__ \ ||  __/ | | | | |
 \____/ \__,_|_|_|\__,_| |___/\__, |___/\__\___|_| |_| |_|
                               __/ |
                              |___/

--]]

-- silly little OS detection
local is_windows = package.config:sub(1,1) == "\\"

local options = {
	log    = false,
	debug  = false,
	cuik   = false,
	tb     = false,
	driver = false,
	shared = false,
	test   = false,
	forth  = false,
	lld    = false,
	gcc    = false,
	asan   = false,
	spall_auto = false
}

-- Cuik/TB are broken down into several pieces
local modules = {
	common = { srcs={"common/common.c", "common/perf.c"} },
	cuik   = { srcs={"libCuik/lib/libcuik.c", "libCuik/lib/toolchains/msvc.c", "libCuik/lib/toolchains/gnu.c", "libCuik/lib/toolchains/darwin.c"}, flags="-I libCuik/include", deps={"common"} },
	tb     = { srcs={"tb/src/libtb.c", "tb/src/x64/x64.c"}, flags="-I tb/include -DCUIK_USE_TB", deps={"common"} },

	-- executables:
	--   Cuik command line
	driver = { is_exe=true, srcs={"main/main_driver.c"}, deps={"common", "cuik", "tb"} },
	--   forth
	forth  = { is_exe=true, srcs={"forth/forth.c"}, deps={"common", "tb"}, flags="-I libCuik/include" },

	-- external dependencies
	mimalloc = { srcs={"mimalloc/src/static.c"} }
}

-- command lines
for i = 1, #arg do
	if arg[i]:sub(1, 1) == "-" then
		options[arg[i]:sub(2)] = true
	end
end

local ldflags = ""
local cflags = " -g -march=haswell -I common -Wall -Werror -Wno-unused -Wno-deprecated -DMI_SKIP_COLLECT_ON_EXIT -DCUIK_ALLOW_THREADS -I mimalloc/include"

if options.asan then
	cflags = cflags.." -fsanitize=address"
else
	cflags = cflags.." -DTB_USE_MIMALLOC -DCUIK_USE_MIMALLOC"
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
end

if not options.log then
	cflags = cflags.." -DLOG_SUPPRESS"
end

if options.spall_auto then
	cflags = cflags.." -DCUIK_USE_SPALL_AUTO -finstrument-functions-after-inlining"
end

local src = {}

if is_windows then
	src[#src + 1] = "c11threads/threads_msvc.c"
	cflags = cflags.." -I c11threads -D_CRT_SECURE_NO_WARNINGS"

	if options.asan then
		ld = "clang"
		ldflags = ldflags.." -fsanitize=address -g -o "
	else
		if options.lld then
			ld = "lld-link"
		else
			ld = "link"
		end

		ldflags = ldflags.." /nologo /debug onecore.lib msvcrt.lib libcmt.lib"

		if options.shared then
			cflags = cflags.." -DCUIK_DLL -DTB_DLL"
			ldflags = ldflags.." /dll"
		end

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

-- lexer metaprogram
command("bin/lexgen"..exe_ext, "libCuik/meta/lexgen.c", cc.." $in -O1 -o $out")
command("libCuik/lib/preproc/keywords.h libCuik/lib/preproc/dfa.h", "bin/lexgen"..exe_ext, "bin/lexgen"..exe_ext)

-- package freestanding headers into C file
local x = {}
if is_windows then
	local cmd = io.popen("dir /B headers\\*.h")
	for c in cmd:lines() do
		x[#x + 1] = "headers/"..c
	end
	cmd:close()
else
	local cmd = io.popen("find headers/*.h -maxdepth 1")
	for c in cmd:lines() do
		x[#x + 1] = c
	end
	cmd:close()
end
local freestanding_headers = table.concat(x, " ")

command("bin/hexembed"..exe_ext, "libCuik/meta/hexembed.c", cc.." $in -O1 -o $out")
command("bin/freestanding.c", freestanding_headers, "bin/hexembed"..exe_ext.." $in", "bin/hexembed"..exe_ext)

src[#src + 1] = "bin/freestanding.c"

-- normal C files
local objs = {}
for i, f in ipairs(src) do
	local out = "bin/"..filename(f)..".o"
	ninja:write("build "..out..": cc "..f)
	if out == "bin/libcuik.o" then
		ninja:write(" | libCuik/lib/preproc/keywords.h libCuik/lib/preproc/dfa.h\n")
	else
		ninja:write("\n")
	end
	objs[#objs + 1] = out
end

local obj_names = table.concat(objs, " ")

local exe_name = "cuik"
if options.tb then exe_name = "tb" end
if options.forth then exe_name = "forth" end

-- link or archive
if options.shared then
	ninja:write([[build ]]..exe_name..dll_ext..[[: link ]]..obj_names.."\n")
elseif is_exe then
	ninja:write([[build ]]..exe_name..exe_ext..[[: link ]]..obj_names.."\n")
else
	ninja:write([[build ]]..exe_name..lib_ext..[[: lib ]]..obj_names.."\n")
end

ninja:close()

local res = os.execute("ninja")
if options.test then
	dofile("tests.lua")
end
