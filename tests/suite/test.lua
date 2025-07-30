-- silly little OS detection
local is_windows = package.config:sub(1,1) == "\\"

local x = {}
if is_windows then
    local cmd = io.popen("dir *.c /B")
    for c in cmd:lines() do
        x[#x + 1] = c
    end
    cmd:close()
else
    local cmd = io.popen("find *.c -maxdepth 1")
    for c in cmd:lines() do
        x[#x + 1] = c
    end
    cmd:close()
end

local exe_name = "a"
if not is_windows then
    exe_name = "./a.out"
end

configs = {
    "", "-O"
}

for i=1,#x do
    print("Testing... "..x[i])
    print("* Cuik")
    os.execute(string.format("cuik %s && %s > cuik.txt", x[i], exe_name))
    print("* Clang")
    os.execute(string.format("clang %s && %s > clang.txt", x[i], exe_name))
    print("* Diff")
    local diff = os.execute("git diff --no-index cuik.txt clang.txt")
    if diff ~= 0 then
        print("BAD!!!")
    end
end

for i=1,#x do
    print("Testing optimized... "..x[i])
    print("* Cuik")
    os.execute(string.format("cuik %s -O && %s > cuik.txt", x[i], exe_name))
    print("* Clang")
    os.execute(string.format("clang %s && %s > clang.txt", x[i], exe_name))
    print("* Diff")
    local diff = os.execute("git diff --no-index cuik.txt clang.txt")
    if diff ~= 0 then
        print("BAD!!!")
    end
end
