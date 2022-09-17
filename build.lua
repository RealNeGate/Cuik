build.mkdir("bin")
config.use_mimalloc = true

if config.use_mimalloc then
    -- Call into Cmake for a shared lib
    if config.os == "Windows" then
        build.command("cd deps/mimalloc && mkdir out")
        build.command("cd deps/mimalloc/out && cmake ../ -DMI_BUILD_STATIC=OFF -DMI_BUILD_OBJECT=OFF -DMI_BUILD_TESTS=OFF")
        build.command("cd deps/mimalloc/out && msbuild libmimalloc.sln -p:Configuration=Release")
        build.command("copy deps\\mimalloc\\out\\Release\\mimalloc.dll bin\\")
        build.command("copy deps\\mimalloc\\out\\Release\\mimalloc-redirect.dll bin\\")
    end
end

function compile_libcuik()
    local files = {
        "lib/*.c", "lib/preproc/*.c", "lib/front/*.c", "lib/targets/*.c",
        "lib/back/ir_gen.c", "lib/back/linker.c"
    }

    if config.os == "Windows" then
        table.insert(files, "lib/back/microsoft_craziness.c")
        table.insert(files, "deps/threads_msvc.c")
    else
        table.insert(files, "deps/mimalloc/src/static.c")
    end

    return { build.ar_chain(build.foreach_chain(files, "clang %f "..flags.." -c -o bin/%F.o", "bin/%F.o"), "bin/libcuik"..config.lib_ext) }
end

function compile_driver(driver_source)
    return build.foreach_chain({
        driver_source,
        "drivers/threadpool.c",
        "drivers/bindgen_c99.c",
        "drivers/bindgen_odin.c"
    }, "clang %f "..flags.." -c -o bin/%F.o", "bin/%F.o")
end

-- compile TB
os.execute("cd tilde-backend && truct")

-- C compiler flags
flags = "-Wall -Werror -Wno-unused-function -Wno-unused-variable "

if config.arch == "x64" then
    flags = "-g -msse4.2 -maes "
end

flags = flags.."-I lib -I include -I deps -I tilde-backend/include "
flags = flags.."-DCUIK_USE_TB -DTB_COMPILE_TESTS "

if config.os == "Windows" then flags = flags.."-D_CRT_SECURE_NO_WARNINGS " end
if config.opt then flags = flags.."-O2 -DNDEBUG " end

-- compile object files for Cuik
local objs = {}

-- on linux and mac the mimalloc override is an object file.
-- we also insert it first because non-windows platforms have
-- order dependent linking.
if config.os ~= "Windows" then
    build.append(objs, build.foreach_chain(
        { "deps/mimalloc/src/static.c" },
        "clang %f -DMI_MALLOC_OVERRIDE -I deps/mimalloc/include -c -o bin/%F.o",
        "bin/%F"..config.lib_ext
    ))
end

build.append(objs, compile_libcuik())
build.append(objs, compile_driver("drivers/main_driver.c"))

-- some of our libs
table.insert(objs, "tilde-backend/tildebackend"..config.lib_ext)

-- Slap everything together
local ld_flags = "-g "
if config.os == "Windows" then
    -- mimalloc requires we link at least one symbol explicitly to force the dll to load
    if config.use_mimalloc then
        ld_flags = ld_flags.." -Xlinker /include:mi_version "
    end

    ld_flags = " -Xlinker /incremental:no -lole32 -lAdvapi32 -lOleAut32 -lDbgHelp"
else
    -- libc, math, dynamic loader & threads on *nix
    ld_flags = ld_flags.." -lm -ldl -lpthread "
end

build.ld_chain(objs, ld_flags, "bin/cuik"..config.exe_ext)
build.done()
