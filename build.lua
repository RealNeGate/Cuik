
-- your based build system stuff goes here
files = {
    "lib/",
    "lib/preproc/",
    "lib/front/",
    "lib/back/",
    "lib/targets/",
}

if build.os == "Windows" then
    table.insert(files, "deps/threads_msvc.c")
else
    table.insert(files, "src/tb/system/posix.c")
end

local options = "-g -Wall -Werror -Wno-unused-function -Wno-unused-variable "
options = options.."-DCUIK_USE_TB "
options = options.."-D_CRT_SECURE_NO_WARNINGS "
options = options.."-msse4.2 -maes "
options = options.."-I lib "
options = options.."-I include "
options = options.."-I deps "
options = options.."-I tilde-backend/include "
if build.opt then
    options = options.."-O2 -DNDEBUG "
end

if build.os ~= "Windows" then
	-- libc & threads on *nix
	options = options.."-lm -lpthread "
end

local outputs = build.compile("libCuik.cache", files, options)
if outputs ~= nil then
    print("Step 5: Link")
    
    local deps = ""
    if build.os == "Windows" then
        deps = "tilde-backend/tildebackend.lib"
    end
    
    build.lib("bin/libcuik.lib", deps, outputs)
end

-- compile the driver now
local ld_flags = "-g "
if build.os == "Windows" then
    ld_flags = ld_flags.."bin/libcuik.lib -Xlinker /defaultlib:msvcrt -Xlinker /incremental:no -lole32 -lAdvapi32 -lOleAut32 -lDbgHelp"
end

local driver = build.compile("Cuik.cache", {
    "drivers/main_driver.c",
    "drivers/threadpool.c",
    "drivers/bindgen_c99.c",
    "drivers/bindgen_odin.c"
}, options)
if driver ~= nil then
    build.link("bin/cuik.exe", ld_flags, driver)
end
