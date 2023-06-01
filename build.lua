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
	debug  = true,
	cuik   = false,
	tb     = false,
	driver = false,
	shared = false,
	lld    = false,
	spall_auto = false
}

-- command lines
for i = 1, #arg do
	if arg[i]:sub(1, 1) == "-" then
		options[arg[i]:sub(2)] = true
	end
end

if not options.shared and not options.cuik and not options.tb then
	options.driver = true
end

src = { "common/common.c", "mimalloc/src/static.c" }
function add_srcs(...)
    for i = 1, select("#",...) do
        src[#src + 1] = select(i,...)
    end
end

local ldflags = ""
local cflags = "-g -msse4 -I common -Wall -Werror -Wno-unused -Wno-deprecated -DTB_USE_MIMALLOC -DCUIK_USE_MIMALLOC -I mimalloc/include -DCUIK_ALLOW_THREADS"

local cc = "clang"
local ar = "llvm-ar -rcs"

if not options.debug then
	cflags = cflags.." -O2 -DNDEBUG"
end

if options.spall_auto then
	cflags = cflags.." -DCUIK_USE_SPALL_AUTO -finstrument-functions-after-inlining"
end

if is_windows then
	add_srcs("c11threads/threads_msvc.c")

	if options.lld then
		ld = "lld-link"
	else
		ld = "link"
	end

	cflags = cflags.." -I c11threads -D_CRT_SECURE_NO_WARNINGS"
	ldflags = ldflags.." /nologo /debug onecore.lib msvcrt.lib libcmt.lib"

	if options.shared then
		cflags = cflags.." -DCUIK_DLL -DTB_DLL"
		ldflags = ldflags.." /dll"
	end

	ldflags = ldflags.." /out:"
	exe_ext = ".exe"
	dll_ext = ".dll"
	lib_ext = ".lib"
else
	ld = cc
	ldflags = ldflags.." -g -lc -lm "

	if options.shared then
		ldflags = ldflags.." -fuse-ld=lld"
	end

	if options.shared then
		ldflags = ldflags.." -shared"
	end

	ldflags = ldflags.." -o "
	exe_ext = ""
	dll_ext = ".so"
	lib_ext = ".a"
end

if options.shared then
	options.cuik = true
	options.tb = true
end

if options.driver then
	add_srcs("main/main_driver.c")
	options.cuik = true
	options.tb = true
end

if options.cuik then
	cflags = cflags.." -I libCuik/include -DMINIZ_NO_MALLOC"
	add_srcs(
		"libCuik/lib/libcuik.c",
		"libCuik/lib/toolchains/msvc.c", "libCuik/lib/toolchains/gnu.c", "libCuik/lib/toolchains/darwin.c"
	)
end

if options.tb then
	cflags = cflags.." -I tb/include -DCUIK_USE_TB"
	add_srcs("tb/src/libtb.c", "tb/src/x64/x64.c")
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
	command = "clang $in $cflags -MD -MF $out.d -c -o $out",
	description = "CC $out"
})
rule("link", {
	command = ld.." $in $ldflags$out",
	description = "LINK $out"
})
rule("lib", {
	depfile = "$out.d",
	command = ar.." $out $in",
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
	local cmd = io.popen("dir /B crt\\include\\*.h")
	for c in cmd:lines() do
		x[#x + 1] = "crt/include/"..c
	end
	cmd:close()
else
	local cmd = io.popen("find crt/include/*.h -maxdepth 1")
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

-- link or archive
if options.driver or options.shared then
	-- link code
	local ext = options.shared and dll_ext or exe_ext
	ninja:write([[build cuik]]..ext..[[: link ]]..obj_names.."\n")
elseif options.cuik then
	ninja:write([[build cuik]]..lib_ext..[[: lib ]]..obj_names.."\n")
elseif options.tb then
	ninja:write([[build tb]]..lib_ext..[[: lib ]]..obj_names.."\n")
end

ninja:close()

os.exit(os.execute("ninja"))
