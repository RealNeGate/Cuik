
-- dofile will return a boolean to tell us if the TB compile made changes
local changes = build.build_lua("tilde-backend")
local files = {
    "lib/", "lib/preproc/", "lib/front/", "lib/back/", "lib/targets/",
}

if config.os == "Windows" then
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
if config.opt then
    options = options.."-O2 -DNDEBUG "
end

if config.os ~= "Windows" then
	-- libc & threads on *nix
	options = options.."-lm -lpthread "
end

local outputs, libcuik_changes = build.compile("libCuik.cache", files, options)
changes = changes or libcuik_changes

if changes then
    local deps = ""
    if config.os == "Windows" then
        deps = "tilde-backend/tildebackend.lib"
    end
    
    build.lib("bin/libcuik.lib", deps, outputs)
end

-- compile the driver now
local driver, driver_changes = build.compile("Cuik.cache", {
    "drivers/main_driver.c",
    "drivers/threadpool.c",
    "drivers/bindgen_c99.c",
    "drivers/bindgen_odin.c"
}, options)
changes = changes or driver_changes

if changes then
    local ld_flags = "-g "
    if config.os == "Windows" then
        ld_flags = ld_flags.."bin/libcuik.lib -Xlinker /defaultlib:msvcrt -Xlinker /incremental:no -lole32 -lAdvapi32 -lOleAut32 -lDbgHelp"
    end


    -- Link everything together
    if changes then
        build.link("bin/cuik.exe", ld_flags, driver)
    end
end

return changes
